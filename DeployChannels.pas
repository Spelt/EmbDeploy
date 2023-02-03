unit DeployChannels;

interface
uses
  System.SysUtils, System.Classes;

type
  IDeployChannelBasic = interface
    ['{E9BA7BAA-8CAF-49F4-AEBD-C622C455458C}']
    procedure SetVerbose(const newVerbose: Boolean);
    function GetVerbose: boolean;
    procedure SetFileListName (const newFileList: string);
    function GetFileListName: string;
    procedure SetProjectRoot (const newProjectRoot: string);
    function GetProjectRoot: string;
    procedure SetChannelName (const newChannelName: string);
    function GetChannelName: string;
    procedure SetLogExceptions(const newLog: Boolean);
    function GetLogExceptions: boolean;

    property Verbose: boolean read GetVerbose write SetVerbose;
    property FileListName: string read GetFileListName write SetFileListName;
    property ProjectRoot: string read GetProjectRoot write SetProjectRoot;
    property ChannelName: string read GetChannelName write SetChannelName;
    property LogExceptions: boolean read GetLogExceptions write SetLogExceptions;
  end;

  IDeployChannel = interface (IDeployChannelBasic)
    ['{0AC82C72-6867-45DA-A37F-901C471D1A47}']
    procedure SetupChannel;
    function CleanChannel: boolean;
    function DeployFile(const localName: string; const remoteDir: string;
                        const operation: Integer; const remoteName: string):boolean;
    function RetrieveResult(const localFolder: string):boolean;
    procedure CloseChannel;
  end;

  TDeployBaseChannel = class (TInterfacedObject, IDeployChannelBasic)
  private
    fVerbose: Boolean;
    fFileListName,
    fProjectRoot: string;
    fChannelName: string;
    fLogExceptions: Boolean;
  public
    procedure SetVerbose(const newVerbose: Boolean);
    function GetVerbose: boolean;
    procedure SetFileListName (const newFileList: string);
    function GetFileListName: string;
    procedure SetProjectRoot (const newProjectRoot: string);
    function GetProjectRoot: string;
    procedure SetChannelName (const newChannelName: string);
    function GetChannelName: string;
    procedure SetLogExceptions(const newLog: Boolean);
    function GetLogExceptions: boolean;
  end;

  TProjectType = (ptApp, ptInstaller);

  TPAClientChannel = class(TDeployBaseChannel, IDeployChannel)
  private
    fRemoteProfile,
    fPAClientPath,
    fDelphiVersion,
    fPlatfrom: string;
    function CallPAClient(const aCommand: string; OnFinished: TProc<TStringList> = nil): Boolean;
  public
    constructor Create (const newRemoteProfile: string; const newPAClientPAth: string;
                        const newDelphiVersion: string; const newPlatform: string);
    procedure SetupChannel;
    function CleanChannel:boolean;
    function DeployFile(const localName: string; const remoteDir: string;
                        const operation: Integer; const remoteName: string):boolean;
    procedure CloseChannel;
    function CodeSignApp(const projectRoot, developerCertificateName, projectName:
        string): boolean;
    function CodeSignInstaller(const projectRoot, developerCertificateName, projectName: string): boolean;
    function CreateInstaller(const projectRoot, projectName, CertificateNameInstaller: string): boolean;
    function Notarize(const projectRoot, appleId, appSpecificPwEncoded, projectName,
        localPath, optionalNotarizationParam: string; projectType: TProjectType): boolean;
    function RetrieveResult(const localFolder: string):boolean;
  end;

  TFolderChannel = class(TDeployBaseChannel, IDeployChannel)
  private
    fFolder: string;
    fProjectName: string;
    fBaseProjectName: string;
  public
    constructor Create (const newFolder: string; const ProjectName: string;
                          const BaseProjectName: string);
    procedure SetupChannel;
    function CleanChannel:boolean;
    function DeployFile(const localName: string; const remoteDir: string;
                        const operation: Integer; const remoteName: string):boolean;
    procedure CloseChannel;
    function RetrieveResult(const localFolder: string):boolean;
  end;

implementation

uses
	System.Win.Registry, Winapi.Windows, System.IOUtils, StrUtils;

const
  // Paclient commands and the parameters to be substituted
  PACLIENT_CLEAN            = '--Clean="%s,%s"';     //0 - project root name, 1 - path to a temp file with containing a list of files
  PACLIENT_PUT              = '--put="%s,%s,%d,%s"'; //0 - local name, 1 - remote path, 2 - operation, 3 - remote name
  PACLIENT_CODE_SIGN_APP    = '--codesign="%s,%s,%s\..\%s.entitlements,1"'; //0 - project root name, 1 - project root name, 2 - project root no extension
  PACLIENT_CODE_SIGN_INST   = '--codesign="%s,%s"'; //0 - project root name, 1 - project root name, 2 - project root no extension
  PACLIENT_NOTARIZE_SIG     = '--notarizeapp="%s,%s,%s,%s,''%s'',''%s._@emb_requestuuid.tmp'',''%s._@emb_.token.tmp''"';
  PACLIENT_NOTARIZE_DO      = '--notarizationinfo="%s,%s,%s,20,''%s._@emb_.token.tmp''"';
  PACLIENT_NOTARIZE_STAPLE_APP   = '--stapleapp="%s,%s.zip"';
  PACLIENT_NOTARIZE_STAPLE_INST  = '--stapleapp="%s"';
  PACLIENT_RETRIEVE_DIR     = '--get=".\*.*,%s"'; //0 - local name, 1 - remote path, 2 - operation, 3 - remote name

  { TPAClientChannel }

// Execute the PaClient.exe app and pass it the command and profile
// Filter some of the paclient output by capturing the out and err pipes
function TPAClientChannel.CallPAClient(const aCommand: string; OnFinished: TProc<TStringList>): Boolean;
var
  Security           : TSecurityAttributes;
  PipeRead, PipeWrite: THandle;
  BytesInPipe        : Cardinal;
  Buffer             : array[0..2000] of AnsiChar;
  Output             : TStringList;
  StartInfo          : TStartupInfo;
  ProcInfo           : TProcessInformation;
  I                  : Integer;
  ExCode             : Cardinal;
  fullProcessPath    : string;
begin
  Result := false;
  // Create a pipe to capture the output
  Security.nLength              := SizeOf(TSecurityAttributes);
  Security.bInheritHandle       := true;
  Security.lpSecurityDescriptor := nil;
  if not CreatePipe(PipeRead, PipeWrite, @Security, 0) then
    if fLogExceptions then
    begin
      Writeln('OS Raised an exception.');
      Halt(1);
    end
    else
      RaiseLastOSError;

  try
    ZeroMemory(@StartInfo, SizeOf(StartInfo));
    ZeroMemory(@ProcInfo, SizeOf(ProcInfo));
    StartInfo.cb          := SizeOf(StartInfo);
    StartInfo.hStdOutput  := PipeWrite;
    StartInfo.hStdError   := PipeWrite;
    StartInfo.dwFlags     := STARTF_USESTDHANDLES;  // use output redirect pipe
    fullProcessPath:='"'+fPaclientPath+'"' + ' ' + aCommand + ' "' + fRemoteProfile+'"';
    if fVerbose then
      Writeln('Full Command Line: '+fullProcessPath);
    if CreateProcess(nil, PChar(fullProcessPath), nil, nil, true,
                          CREATE_NO_WINDOW, nil, nil, StartInfo, ProcInfo) then
    begin
      Output := TStringList.Create;
      try
        WaitForSingleObject(ProcInfo.hProcess, Infinite);
        // The process has finished, so close the write pipe and read the output
        CloseHandle(PipeWrite);
        ZeroMemory(@Buffer, Length(Buffer));
        ReadFile(PipeRead, Buffer[0], Length(Buffer), BytesInPipe, nil);

        // Parse the output, delete the first 4 lines, that are not very useful, and display the rest indented
        Output.Text:=String(Buffer);
        Output.Delete(0); Output.Delete(0); Output.Delete(0); Output.Delete(0);
        for I := 0 to Output.Count - 1 do
          WriteLn('  ' + Output[I]);

        // Check the exit code of paclient.exe / 0 is success
        Result := GetExitCodeProcess(ProcInfo.hProcess, ExCode) and (ExCode = 0);
      finally
        CloseHandle(ProcInfo.hProcess);
        CloseHandle(ProcInfo.hThread);

        if Assigned(OnFinished) then
          OnFinished(OutPut);

        Output.Free;
      end;
    end;
  finally
    CloseHandle(PipeRead);

    {$IFNDEF  DEBUG}  // The PipeWrite handle is closed twice,
                      //which is acceptable in Release,
                      //but raises exceptions when running with the debugger
    CloseHandle(PipeWrite);
    {$ENDIF}
  end;
end;

function TPAClientChannel.CleanChannel: boolean;
begin
  result:=CallPaclient(Format(PACLIENT_CLEAN, ['', fFileListName]));
end;

procedure TPAClientChannel.CloseChannel;
begin
end;

constructor TPAClientChannel.Create(const newRemoteProfile, newPAClientPAth,
  newDelphiVersion, newPlatform: string);
begin
  inherited Create;
  fRemoteProfile:=newRemoteProfile;
  fPAClientPath:=newPAClientPAth;
  fDelphiVersion:=newDelphiVersion;
  fPlatfrom:=newPlatform;
end;

function TPAClientChannel.DeployFile(const localName, remoteDir: string;
  const operation: Integer; const remoteName: string): boolean;
begin
  result:=CallPaclient(Format(PACLIENT_PUT, [localName, remoteDir,
                              operation, remoteName]));
end;

function TPAClientChannel.CodeSignApp(const projectRoot,
    developerCertificateName, projectName: string): boolean;
begin
  var sep := QuotedStr(developerCertificateName);
  var p := Format(PACLIENT_CODE_SIGN_APP, [ProjectRoot, sep, ProjectRoot, projectName]);
  result := CallPaclient(p);
end;

function TPAClientChannel.CodeSignInstaller(const projectRoot,
    developerCertificateName, projectName: string): boolean;
begin
  var sep := QuotedStr(developerCertificateName);
  var p := Format(PACLIENT_CODE_SIGN_INST, [ProjectRoot, sep]);
  result := CallPaclient(p);
end;

function TPAClientChannel.CreateInstaller(const projectRoot, projectName, CertificateNameInstaller: string): boolean;
begin
  var p := string.Format('--productbuild="%s,/Applications,%s.pkg,''%s''"',[ProjectRoot, ProjectName, CertificateNameInstaller]);
  result := CallPaclient(p);
end;

function TPAClientChannel.Notarize(const projectRoot, appleId, appSpecificPwEncoded, projectName,
  localPath, optionalNotarizationParam: string; projectType: TProjectType): boolean;
begin
  var p := string.Format(PACLIENT_NOTARIZE_SIG, [projectRoot, projectName, appleId, appSpecificPwEncoded, optionalNotarizationParam, localPath, localPath]);
  Writeln('');
  var notarizationUUID := '';
  result := CallPaclient(p,
  procedure (Output: TStringList)
  begin
    // Retrieve RequestUUID from the console output. Not from the temp txt file, was also a posibility.
    var tmpFileName := TPath.Combine(GetCurrentDir(), projectName + '._@emb_requestuuid.tmp');
    if FileExists(tmpFileName) then
    begin
      TFile.Delete(tmpFileName);
      Writeln('Deleted temp file: ' + tmpFileName);
    end;

    for var i:=0 to Output.Count - 1 do
    begin
      if ContainsText(Output.Strings[i], 'RequestUUID:') then
      begin
        var p := Pos(':', Output.Strings[i]);
        notarizationUUID := Copy(Output.Strings[i], p + 1, Length(Output.Strings[i]) - p + 1);
        notarizationUUID := Trim(notarizationUUID);
        Writeln('UUID=' + notarizationUUID);
        break;
      end;
    end;
  end);

  if (not result) or (notarizationUUID = '') then
    Exit;

  var notarizeDo:= string.Format(PACLIENT_NOTARIZE_DO, [notarizationUUID, appleId, appSpecificPwEncoded, localPath]);
  result := CallPaclient(notarizeDo,
  procedure (Output: TStringList)
  begin
    for var i:=0 to Output.Count - 1 do
    begin
      Writeln(output[i]);
    end;
  end);

  if not result then
    Exit;

  var staple := '';
  if ProjectType = TProjectType.ptApp then
    staple := string.Format(PACLIENT_NOTARIZE_STAPLE_APP,[projectRoot, projectName])
  else
    staple := string.Format(PACLIENT_NOTARIZE_STAPLE_INST,[projectRoot]);

  result := CallPaclient(staple,
  procedure (Output: TStringList)
  begin
    for var i:=0 to Output.Count - 1 do
    begin
      Writeln(output[i]);
    end;
  end);

end;

function TPAClientChannel.RetrieveResult(const localFolder: string): boolean;
begin
  result:=CallPaclient(Format(PACLIENT_RETRIEVE_DIR, [localFolder]));
end;

// Check if there is a remote profile and try to find one. Must be after the project is parsed
procedure TPAClientChannel.SetupChannel;
var
  Reg: TRegistry;
  regKey: string;
begin
  if fRemoteProfile.IsEmpty then
  begin
    Reg:=TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      regKey:='Software\Embarcadero\BDS\' + fDelphiVersion + '\RemoteProfiles';
      if fVerbose then
        Writeln('Looking at Registry Key: '+regKey);
      if Reg.OpenKey(regKey, false) then
        if Reg.ValueExists('Default_'+fPlatfrom) then
          fRemoteProfile := Reg.ReadString('Default_' + fPlatfrom);
        if fRemoteProfile.IsEmpty then
          if fLogExceptions then
          begin
            Writeln('Default remote profile not found. Please specify a profile');
            Halt(1);
          end
          else
            raise Exception.Create('Default remote profile not found. Please specify a profile');
    finally
      Reg.Free;
    end;
  end;

  Writeln('Using default profile: ' + fRemoteProfile);
end;

{ TFolderChannel }

function TFolderChannel.CleanChannel:boolean;
begin
  Result:=true;
end;

procedure TFolderChannel.CloseChannel;
begin

end;

constructor TFolderChannel.Create(const newFolder: string; const ProjectName: string;
                                      const BaseProjectName: string);
begin
  inherited Create;
  fFolder:=newFolder;
  fProjectName:=ProjectName;
  fBaseProjectName:=BaseProjectName;
end;

function TFolderChannel.DeployFile(const localName, remoteDir: string;
  const operation: Integer; const remoteName: string): boolean;
var
  Source,
  Target: TFileStream;
  targetDir,
  targetPath: string;
  fileAttributes: TFileAttributes;
begin
  result:=True;
  Source:= TFileStream.Create(localName, fmOpenRead);
 try
   if not DirectoryExists(remoteDir) then
   begin
    targetDir:=TPath.Combine(fFolder,remoteDir);
    TDirectory.CreateDirectory(targetDir);
    fileAttributes:=TDirectory.GetAttributes(targetDir);
    Exclude(fileAttributes, TFileAttribute.faReadOnly);
    TDirectory.SetAttributes(targetDir,fileAttributes);
   end;

   targetPath:=TPath.Combine(
                TPath.Combine(fFolder, remoteDir), remoteName);
   Target := TFileStream.Create(targetPath, fmOpenWrite or fmCreate );
   try
     Target.CopyFrom(Source, Source.Size ) ;
   finally
     Target.Free;
   end;

   fileAttributes:=TFile.GetAttributes(targetPath);
   Exclude(fileAttributes, TFileAttribute.faReadOnly);
   TFile.SetAttributes(targetPath, fileAttributes);

   if remoteName=fBaseProjectName then
   begin
     Include(fileAttributes, TFileAttribute.faArchive);
     TFile.SetAttributes(targetPath, fileAttributes);
   end;

 finally
   Source.Free;
 end;

end;

function TFolderChannel.RetrieveResult(const localFolder: string): boolean;
begin
  result := True;
end;

procedure TFolderChannel.SetupChannel;
  procedure DeleteDirectory(const Name: string);
  var
    F: TSearchRec;
    str: string;
  begin
    if FindFirst(Name + '\*', faAnyFile, F) = 0 then begin
      try
        repeat
          if (F.Attr and faDirectory <> 0) then
          begin
            if (F.Name <> '.') and (F.Name <> '..') then
            begin
              DeleteDirectory(Name + '\' + F.Name);
            end;
          end
          else
          begin
            str:=Name + '\' + F.Name;
            DeleteFile(PWideChar(str));
          end;
        until FindNext(F) <> 0;
      finally
        FindClose(F.FindHandle);
      end;
      RemoveDir(Name);
    end;
  end;

begin
  DeleteDirectory(TPath.Combine(fFolder, fProjectName));
end;

{ TDeployBaseChannel }

function TDeployBaseChannel.GetChannelName: string;
begin
  result:=fChannelName;
end;

function TDeployBaseChannel.GetFileListName: string;
begin
  result:=fFileListName;
end;

function TDeployBaseChannel.GetLogExceptions: boolean;
begin
  result:=fLogExceptions;
end;

function TDeployBaseChannel.GetProjectRoot: string;
begin
  result:=fProjectRoot;
end;

function TDeployBaseChannel.GetVerbose: boolean;
begin
  result:=fVerbose;
end;

procedure TDeployBaseChannel.SetChannelName(const newChannelName: string);
begin
  fChannelName:=newChannelName;
end;

procedure TDeployBaseChannel.SetFileListName(const newFileList: string);
begin
  fFileListName:=newFileList;
end;

procedure TDeployBaseChannel.SetLogExceptions(const newLog: Boolean);
begin
  fLogExceptions:=newLog;
end;

procedure TDeployBaseChannel.SetProjectRoot(const newProjectRoot: string);
begin
  fProjectRoot:=newProjectRoot;
end;

procedure TDeployBaseChannel.SetVerbose(const newVerbose: Boolean);
begin
  fVerbose:=newVerbose;
end;

end.
