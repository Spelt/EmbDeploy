unit Deployer;
interface
uses
  Winapi.ActiveX, Winapi.MsXML,
  System.Zip, System.IOUtils,
  System.Sysutils, System.Classes, System.Win.Registry, Winapi.Windows,
  DeployChannels, System.Generics.Collections;

type
  TDeployClass = record
    Required  : Boolean;
    Name      : String;
    RemoteDir : String;
    Operation : Integer;
    Extensions: String;           // What is it used for?
  end;
  TDeployFile = record
    Enabled      : Boolean;
    ClassName    : String;
    LocalName    : String;
    RemoteName   : String;
    RemoteDir    : String;
    Operation    : Integer;
    Configuration: String;    // Release/Debug/Base/Others...
  end;
  TEntitlementsRecord= record
    ReadOnlyMusic: Boolean; //<EL_ReadOnlyMusic>
    ///// The IDE doesn't create key for this entry
    ///// <EL_ReadWriteMusic>
    ReadWriteMusic: Boolean;
    ////
    ReadOnlyPictures: Boolean; //<EL_ReadOnlyPictures>
    ////// There is a bug in the creation of the file by the IDE
    ReadWritePictures: Boolean; //<EL_ReadWritePictures>
    /////  The key com.apple.security.assets.pictures.read-only is
    /////  generated twice.
    ///// The com.apple.security.assets.pictures.read-write is missing
    IncomingNetwork: Boolean; //<EL_IncomingNetwork>
    OutgoingNetwork: Boolean; //<EL_OutgoingNetwork>
    Location: Boolean; //<EL_Location>true
    USBDevice: Boolean; //<EL_USBDevices>
    ReadWriteCalendars: Boolean; //<EL_ReadWriteCalendars>
    ReadWriteFileDialog: Boolean; //<EL_ReadWriteFileDialog>
    ReadOnlyFileDialog: Boolean; //<EL_ReadOnlyFileDialog>
    ReadWriteDownloads: Boolean; //<EL_ReadWriteDownloads>
    ReadWriteMovies: Boolean; //<EL_ReadWriteMovies>
    ReadOnlyMovies: Boolean; //<EL_ReadOnlyMovies>
    RecordingMicrophone: Boolean; //<EL_RecordingMicrophone>
    Printing: Boolean; //<EL_Printing>
    CaptureCamera: Boolean; //<EL_CaptureCamera>
    ReadWriteAddressBook: Boolean; //<EL_ReadWriteAddressBook>
    ChildProcessInheritance: Boolean; //<EL_ChildProcessInheritance>
  end;
  TDeployer = class
  private
    fDelphiPath   : String;
    fDelphiVersion: String;  // The version number - 9.0, 10.0, 11.0, etc
    fConfig       : String;
    fProjectName  : String;
    fPlatform     : String;
    fProjectRoot  : String;
    fRemoteProfile: String;
    fDeployClasses: array of TDeployClass;
    fDeployFiles  : array of TDeployFile;
    fIgnoreErrors : Boolean;
    fPaclientPath : String;
    fVerbose      : Boolean;
    fDeployChannels: TList<IDeployChannel>;
    fBinaryFolder : string;
    fLogExceptions: Boolean;
    fDebugExcludeFiles: TList<string>; //Keeps a list of extensions to be excluded depending on
                                               //the configuration. Eg. .rsm file is deployed in Debug
                                               //but not in Release configuration. This allows for use of
                                               //Debug builds to Release deployments
    fReleaseExcludeFiles: TList<string>;
    fCertNameDev: string;
    fAppleId: string;
    fCertNameInstaller: string;
    fNotarizationExtraOptions: string;
    fProjectPath: string;
    fInstallerIsCreated: Boolean;
    fTeamId,
    fAppSpecificPassword : string;
    procedure GetEmbarcaderoPaths;
    procedure SetupChannels;
    procedure SetConfig(const Value: String);
    function PlatFormNeedsCodeSigning():Boolean;
    function PlatFormNeedsInstaller: Boolean;
    function ExtractTeamId(): string;
  public
    constructor Create(const aDelphiVersion: String);
    destructor Destroy; override;
    procedure BundleProject(const aProjectPath, aZIPName: String);
    procedure ParseProject(const aProjectPath: String);

    procedure CodeSignProject();
    procedure CreateInstallerProject();
    procedure CodeSignInstaller();
    procedure NotarizeInstaller();

    procedure ExecuteCommand(const aProjectPath, aCommand: String; const forcePlain: boolean = false; OnFinished: TProc<TStringList> = nil);
    procedure ExecuteCommandFile(const aProjectPath, filename: String);
    procedure DumpRemoteDirectory(const aProjectPath, directoryName, sepFiles: String);
    procedure RegisterPACLient;
    procedure RegisterFolder(const regPath: string; const project: string);
    property Config       : String  read fConfig        write SetConfig;
    property IgnoreErrors : Boolean read fIgnoreErrors  write fIgnoreErrors;
    property Platform     : String  read fPlatform      write fPlatform;
    property ProjectRoot  : String  read fProjectRoot   write fProjectRoot;
    property RemoteProfile: String  read fRemoteProfile write fRemoteProfile;
    property Verbose      : boolean read fVerbose       write fVerbose;
    property BinaryFolder : string  read fBinaryFolder  write fBinaryFolder;
    property LogExceptions: Boolean read fLogExceptions write fLogExceptions;
    property CertNameDev              : string read fCertNameDev    write fCertNameDev;
    property AppleId                  : string read fAppleId        write fAppleId;
    property AppSpecificPassword     : string read fAppSpecificPassword     write fAppSpecificPassword;


    property CertNameInstaller        : string read fCertNameInstaller        write fCertNameInstaller;
    property NotarizationExtraOptions : string read fNotarizationExtraOptions write fNotarizationExtraOptions;
    property ProjectPath              : string read fProjectPath              write fProjectPath;
  end;

implementation
uses
	System.StrUtils;

// Try to find the last version of Delphi installed to get the Redist folder and Paclient path
procedure TDeployer.GetEmbarcaderoPaths;
var
  Reg: TRegistry;
  Versions: TStringList;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    // If no Delphi version is supplied try to get the latest one from the registry
    if fDelphiVersion.IsEmpty then
    begin
      Versions := TStringList.Create;
      try
        if Reg.OpenKey('SOFTWARE\Embarcadero\BDS', false) then
        begin
          Reg.GetKeyNames(Versions);
          Reg.CloseKey;

          {
            If the version is not set it defaults to minimal 21.0 (Sydney).
            Then it looks for the highest version installed.
          }
          var v := 21.0;
          fDelphiVersion := '21.0';
          var fs := TFormatSettings.Create;
          fs.DecimalSeparator := '.';
          for var i := 0 to Versions.Count -1 do
          begin
            var vBDS := StrToFloat (Versions[i], fs);
            if vBDS > v then
            begin
              v := vBDS;
              fDelphiVersion := Versions[i];
            end;
          end;

        end;
      finally
        Versions.Free;
      end;
    end;
    var DelphiInstallPath := 'SOFTWARE\Embarcadero\BDS\' + fDelphiVersion;
    if Reg.OpenKey(DelphiInstallPath, false) then
      if Reg.ValueExists('RootDir') then
        fDelphiPath := Reg.ReadString('RootDir');
    if fDelphiPath.IsEmpty then
    if fLogExceptions then
    begin
      Writeln('The Delphi install path could not be found. Expecting registry: ' + DelphiInstallPath);
      Halt(1);
    end
    else
      raise Exception.Create('The Delphi install path could not be found. Expecting registry: ' + DelphiInstallPath);
    fPaclientPath := IncludeTrailingPathDelimiter(fDelphiPath) + 'bin\paclient.exe'
  finally
    Reg.Free;
  end;
end;

constructor TDeployer.Create(const aDelphiVersion: String);
begin
  inherited Create;
  fDeployChannels:=TList<IDeployChannel>.Create;
  fDebugExcludeFiles:=TList<string>.Create;
  fReleaseExcludeFiles:=TList<string>.Create;
  fReleaseExcludeFiles.Add('.rsm');
  fDelphiVersion := aDelphiVersion;
  fCertNameDev := '';
  fCertNameInstaller := '';
  fInstallerIsCreated := false;
  GetEmbarcaderoPaths;
  fTeamId := '';
end;

// Produce a ZIP archive of the files to be deployed (useful to produce archives of the OSX .APP bundles on Win)
procedure TDeployer.BundleProject(const aProjectPath, aZIPName: String);
var
  Zip: TZipFile;
  I: Integer;
begin
  ParseProject(aProjectPath);
  WriteLn(Format('Archiving %d files from project %s, config %s', [Length(fDeployFiles), aProjectPath, fConfig]));
  // Create the folders in the zip name, if any
  ForceDirectories(ExtractFilePath(ExpandFileName(aZIPName)));
  Zip := TZipFile.Create;
  try
    if FileExists(aZIPName) then
      TFile.Delete(aZIPName);
    Zip.Open(aZIPName, zmWrite);
    // Add each file to the archive with its path. The \ is replaced with / to make the archive work in OSX
    for I := 0 to Length(fDeployFiles) - 1 do
      Zip.Add(fDeployFiles[I].LocalName, fDeployFiles[I].RemoteDir.Replace('\', '/') + fDeployFiles[I].RemoteName.Replace('\', '/'));
  finally
    Zip.Free;
  end;
end;

procedure TDeployer.CodeSignProject();
begin
  if not PlatFormNeedsCodeSigning then
    Exit;

  for var tmpChannel in fDeployChannels do
  begin
    if not (tmpChannel is TPAClientChannel) then
      Continue;
    var channel := tmpChannel as TPAClientChannel;
    Writeln('Code signing ' + fProjectName);
    if (not channel.CodeSignApp(fProjectRoot, fCertNameDev, fProjectName)) and (not fIgnoreErrors) then
      if fLogExceptions then
      begin
        Writeln('Error in '+tmpChannel.ChannelName+'. Deployment stopped.');
        Halt(1);
      end
      else
        raise Exception.Create('Error in '+ tmpChannel.ChannelName+'. Deployment stopped.');
  end;
end;

function TDeployer.ExtractTeamId(): string;
begin
  var p1 := Pos('(',fCertNameDev);
  var p2 := Pos(')',fCertNameDev);
  if (p1 = 0) or (p2 = 0) then
    exit('');

  result := Copy(fCertNameDev, p1+1, p2 - p1-1);
  Writeln('Team id = ' + result);
end;

procedure TDeployer.CreateInstallerProject();
begin
  fInstallerIsCreated := false;
  if not PlatFormNeedsInstaller() then
    Exit;

  Writeln('Creating installer.');
  for var tmpChannel in fDeployChannels do
  begin
    if not (tmpChannel is TPAClientChannel) then
      Continue;

    var channel := tmpChannel as TPAClientChannel;
    var script := channel.CreateNativeInstallerScript(fProjectRoot, fProjectName);
    ExecuteCommand('', script);
  end;
  fInstallerIsCreated := true;
end;

procedure TDeployer.CodeSignInstaller();
begin
  if not fInstallerIsCreated then
    Exit;

  for var tmpChannel in fDeployChannels do
  begin
    if not (tmpChannel is TPAClientChannel) then
      Continue;
    var channel := tmpChannel as TPAClientChannel;
    var apkName := fProjectName + '.pkg';
    Writeln('Code signing the installer ' + apkName);
    if (not channel.CodeSignInstaller(apkName, fCertNameDev, fProjectName)) and (not fIgnoreErrors) then
      if fLogExceptions then
      begin
        Writeln('Error in '+tmpChannel.ChannelName+'. Deployment stopped.');
        Halt(1);
      end
      else
        raise Exception.Create('Error in '+ tmpChannel.ChannelName+'. Deployment stopped.');
  end;
end;

procedure TDeployer.NotarizeInstaller();
begin
  Writeln('Installer notarization ' + fProjectName);

  if fTeamId = '' then
    fTeamId := ExtractTeamId();

  if fAppleId = '' then
  begin
    if fLogExceptions then
    begin
      Writeln('Missing parameters: AppleId or AppSpecificPwEncoded. Deployment stopped.');
      Halt(1);
    end
    else
      raise Exception.Create('Missing parameters: AppleId or AppSpecificPwEncoded. Deployment stopped.');
  end;

  for var tmpChannel in fDeployChannels do
  begin
    if not (tmpChannel is TPAClientChannel) then
      Continue;

    var channel := tmpChannel as TPAClientChannel;
    var apkName := fProjectName + '.pkg';
    var localPath := string.format('%s\\%s', [ProjectPath, fProjectName]);
    begin
      var script := channel.CreateNativeNotarizationScript(AppleId, fAppSpecificPassword, fTeamId, fProjectName);
      ExecuteCommand('', script, false,
      procedure (Output: TStringList)
      begin
        var notarizationUUID:='';
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
    end;
  end;
end;

destructor TDeployer.Destroy;
begin
  fDeployChannels.Free;
  fDebugExcludeFiles.Free;
  fReleaseExcludeFiles.Free;
  inherited;
end;

// Parse the project file to find the list of files to be deployed
procedure TDeployer.ParseProject(const aProjectPath: String);
var
  XmlDoc: IXMLDOMDocument;
  Node  : IXMLDOMNode;
  Nodes : IXMLDOMNodeList;
  I, J, Count : Integer;
  entitlementPath: string;
  tmpDeployFile: TDeployFile;
  currExcludeList: TList<string>;
  tmpstr: string;
begin
  CoInitialize(nil);
  XmlDoc := CoDOMDocument.Create;
  try
    // Set the project name the same as the file
    fProjectName := ChangeFileExt(ExtractFileName(aProjectPath), '');
    // Load the project file
    if not XmlDoc.load(aProjectPath) then
      if fLogExceptions then
      begin
        Writeln('Project file could not be loaded');
        Halt(1);
      end
      else
        raise Exception.Create('Project file could not be loaded');
    // Read the default platform if not set with a parameter (OSX32, Win32/64, etc)
    if fPlatform.IsEmpty then
    begin
      if fLogExceptions then
      begin
        Writeln('Platform empty. Please specify a platform.');
        Halt(1);
      end
      else
        raise Exception.Create('Platform empty. Please specify a platform.');

      fPlatform := Node.nodeValue;
    end;

    // Read the default config if not set with a parameter (Release or Debug or another)
    if fConfig.IsEmpty then
    begin
      fConfig := 'Release';
    end;
    // Read the ProjectRoot for building the deploy file names
    if fProjectRoot.IsEmpty then
    begin
      Node := XmlDoc.selectSingleNode('//Deployment/ProjectRoot[@Platform="' + fPlatform + '"]');
      if not Assigned(Node) then
        if fLogExceptions then
        begin
          Writeln('ProjectRoot not found in the project.');
          Halt(1);
        end
        else
          raise Exception.Create('ProjectRoot not found in the project.');
      fProjectRoot := Node.attributes.getNamedItem('Name').nodeValue;
      fProjectRoot := fProjectRoot.Replace('$(PROJECTNAME)', fProjectName);
    end;

    // Get all the Platform subnodes of the DeployClass nodes for the specified platform
    Nodes := XmlDoc.selectNodes('//DeployClass/Platform[@Name="' + fPlatform + '"]');
    SetLength(fDeployClasses, Nodes.length);
    Count := 0;
    for I := 0 to Nodes.length - 1 do
    begin
      // Get the Name and Required attributes of the DeployClass node (the parent)
      if Nodes.item[I].parentNode.attributes.getNamedItem('Name') <> nil then
        fDeployClasses[Count].Name := Nodes.item[I].parentNode.attributes.getNamedItem('Name').nodeValue;
      if Nodes.item[I].parentNode.attributes.getNamedItem('Required') <> nil then
        fDeployClasses[Count].Required := StrToBool(Nodes.item[I].parentNode.attributes.getNamedItem('Required').nodeValue);
      // Get the RemoteDir, Operation, Extensions subnode values
      if Nodes.item[I].selectSingleNode('RemoteDir/node()') <> nil then
        fDeployClasses[Count].RemoteDir := Nodes.item[I].selectSingleNode('RemoteDir/node()').nodeValue;
      if Nodes.item[I].selectSingleNode('Operation/node()') <> nil then
        fDeployClasses[Count].Operation := Nodes.item[I].selectSingleNode('Operation/node()').nodeValue;
      if Nodes.item[I].selectSingleNode('Extensions/node()') <> nil then
        fDeployClasses[Count].Extensions := Nodes.item[I].selectSingleNode('Extensions/node()').nodeValue;
      // VG 300715: The XE8 sets the ProjectOSXEntitlements RemoteFolder incorrectly to "../" instead of "Contents"
      // Hardcode a quick fix for it and watch for other such problems
      //
      //Not sure how XE10, 10.1 (Seatlle) deal with this but in D10.1 Berlin the folder is set to "..\" and not to "../"
      {$IFDEF VER310}
        entitlementPath:='..\';
      {$ELSE}
        entitlementPath:='../';
      {$ENDIF}
      if fDeployClasses[Count].Name.Equals('ProjectOSXEntitlements') and fDeployClasses[Count].RemoteDir.Equals(entitlementPath) then
        fDeployClasses[Count].RemoteDir := 'Contents';
      Inc(Count);
    end;
    SetLength(fDeployClasses, Count);
    // Get all the Platform subnodes of the DeployFile nodes for the specified platform
    Nodes := XmlDoc.selectNodes('//DeployFile/Platform[@Name="' + fPlatform + '"]');
    SetLength(fDeployFiles, Nodes.length);
    Count := 0;
    for I := 0 to Nodes.length - 1 do
    begin
      // Get the LocalName, Configuration, Class attributes of the DeployFiles node (the parent)
      if Nodes.item[I].parentNode.attributes.getNamedItem('LocalName') <> nil then
        fDeployFiles[Count].LocalName     := Nodes.item[I].parentNode.attributes.getNamedItem('LocalName').nodeValue;
      if Nodes.item[I].parentNode.attributes.getNamedItem('Configuration') <> nil then
        fDeployFiles[Count].Configuration := Nodes.item[I].parentNode.attributes.getNamedItem('Configuration').nodeValue;
      if Nodes.item[I].parentNode.attributes.getNamedItem('Class') <> nil then
        fDeployFiles[Count].ClassName     := Nodes.item[I].parentNode.attributes.getNamedItem('Class').nodeValue;
      // Get the Enabled, RemoteDir, RemoteName subnode values
      if Nodes.item[I].selectSingleNode('Enabled/node()') <> nil then
        fDeployFiles[Count].Enabled := StrToBool(Nodes.item[I].selectSingleNode('Enabled/node()').nodeValue)
      else
        fDeployFiles[Count].Enabled := true;
      if Nodes.item[I].selectSingleNode('RemoteDir/node()') <> nil then
        fDeployFiles[Count].RemoteDir := Nodes.item[I].selectSingleNode('RemoteDir/node()').nodeValue;
      if Nodes.item[I].selectSingleNode('RemoteName/node()') <> nil then
        fDeployFiles[Count].RemoteName := Nodes.item[I].selectSingleNode('RemoteName/node()').nodeValue;
      Inc(Count);
    end;
    SetLength(fDeployFiles, Count);
    // Disable files that are not for the specified Configuration or are duplicated (e.g. both Base and Release configs)
    for I := 0 to Length(fDeployFiles) - 1 do
    begin
      // Check the Base configurations if there is a file with a Release/Debug config and use it, otherwise use the Base
      if (fDeployFiles[I].Configuration = 'Base') or fDeployFiles[I].Configuration.IsEmpty then
      begin
        for J := 0 to Length(fDeployFiles) - 1 do
          if (fDeployFiles[I].LocalName = fDeployFiles[J].LocalName) and (fDeployFiles[J].Configuration = fConfig) then
          begin
            // There is a more detailed config found, so disable the Base
            fDeployFiles[I].Enabled := false;
            Break;
          end;
      end else
        // Check if the file is for a different non-Base config and disable it
        // Second check added because Delphi seems to save incorrect info for some files in the Dproj
        // E.g. the ICNS file with class ProjectOSXResource appears only under a Release config, but not under a
        // Sandbox config. If Sandbox is selected from the IDE and deployed, the Dproj changes and the ICNS is included
        // in Sandbox, but sometimes excluded from Release. No pattern found
        // The only solution is not to disable 'standard' Class files, e.g. non user added

        {2023-02-03: ES. Is fixed }
        if (fDeployFiles[I].Configuration <> fConfig) then // and (fDeployFiles[I].ClassName = 'File') then
          fDeployFiles[I].Enabled := false;
    end;
    // Shrink the array to skip the disabled items
    Count := 0;
    for I := 0 to Length(fDeployFiles) - 1 do
      if fDeployFiles[I].Enabled then
      begin
        fDeployFiles[Count] := fDeployFiles[I];
        Inc(Count);
      end;
    SetLength(fDeployFiles, Count);
    // Match the above DeployFile and DeployClass entries and build the complete Remote/Local names, dirs, etc
    for I := 0 to Length(fDeployFiles) - 1 do
    begin
      // Find if there is the same DeployClass and copy the attributes from it if they don't exist already
      for J := 0 to Length(fDeployClasses) - 1 do
      begin
        if fDeployFiles[I].ClassName = fDeployClasses[J].Name then
        begin
          if fDeployFiles[I].RemoteDir.IsEmpty then
            fDeployFiles[I].RemoteDir := fDeployClasses[J].RemoteDir;
          fDeployFiles[I].Operation := fDeployClasses[J].Operation;
          Break;
        end;
      end;
      // Replace the BDS paths for the Redist files from Embarcadero
      fDeployFiles[I].LocalName := fDeployFiles[I].LocalName.Replace('$(BDS)\', fDelphiPath);
      // Finalize the Remote names and dirs
      fDeployFiles[I].RemoteDir := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(fProjectRoot) + fDeployFiles[I].RemoteDir);
      if fDeployFiles[I].RemoteName.IsEmpty then
        fDeployFiles[I].RemoteName := ExtractFileName(fDeployFiles[I].LocalName);
    end;
    if fVerbose then
    begin
      Writeln('--------------------');
      Writeln('Files to be deployed');
      Writeln('--------------------');
      Writeln('Class Name - Local Name - Remote Name - Remote Dir');
      Writeln('--------------------------------------------------');
      for i := 0 to Length(fDeployFiles)-1 do
        Writeln(Format('%10s - %10s - %10s - %10s',
            [fDeployFiles[i].ClassName, fDeployFiles[i].LocalName,
            fDeployFiles[i].RemoteName, fdeployFiles[i].RemoteDir]));
    end;

    //Change the folder to the binaryFolder is supplied in the command line
    if trim(fBinaryFolder)<>'' then
    begin
      for i := 0 to Length(fDeployFiles)-1 do
        if (Trim(fDeployFiles[i].ClassName)<>'DependencyModule')
          and (Trim(fDeployFiles[i].ClassName)<>'ProjectOSXResource')
          and (fDeployFiles[i].Configuration = fConfig) then
          fDeployFiles[i].LocalName:=TPath.Combine(
                            IncludeTrailingPathDelimiter(fBinaryFolder),
                            ExtractFileName(fDeployFiles[i].LocalName));

      if fVerbose then
      begin
        Writeln('--------------------');
        Writeln('Files to be deployed (after use of BinaryFolder:'+fBinaryFolder+')');
        Writeln('--------------------');
        Writeln('Class Name - Local Name - Remote Name - Remote Dir');
        Writeln('--------------------------------------------------');
        for i := 0 to Length(fDeployFiles)-1 do
        if fDeployFiles[i].Enabled then
        begin
          Writeln(Format('%10s - %10s - %10s - %10s',
              [fDeployFiles[i].ClassName, fDeployFiles[i].LocalName,
              fDeployFiles[i].RemoteName, fdeployFiles[i].RemoteDir]));
          Writeln('--------------------------------------------------');
        end;
      end;

    end;
    Writeln('Checking files against configuration ('+fConfig+')');

    currExcludeList:=fReleaseExcludeFiles;
    if fConfig='Debug' then
      currExcludeList:=fDebugExcludeFiles;

    i:=0;
    for tmpDeployFile in fDeployFiles do
    begin
      tmpstr:=ExtractFileExt(tmpDeployFile.LocalName);
      if currExcludeList.Contains(ExtractFileExt(tmpDeployFile.LocalName)) then
      begin
        Writeln('Ignored: '+tmpDeployFile.LocalName);
        fDeployFiles[i]:=fDeployFiles[High(fDeployFiles)];
        if i<High(fDeployFiles) then
          SetLength(fDeployFiles,length(fDeployFiles)-1);
      end;
      Inc(i);
    end;
  finally
    Node   := nil;
    Nodes  := nil;
    XmlDoc := nil;
  end;
end;

procedure TDeployer.RegisterFolder(const regPath: string; const project: string);
var
  newFolderChannel: IDeployChannel;
  projName: string;
begin
  fProjectName:=project;
  newFolderChannel:=TFolderChannel.Create(Trim(regPath), projName, fProjectName);
  newFolderChannel.ChannelName:='Folder Channel';
  newFolderChannel.Verbose:=fVerbose;
  newFolderChannel.ProjectRoot:=fProjectRoot;
  newFolderChannel.LogExceptions:=fLogExceptions;
  fDeployChannels.Add(newFolderChannel);
end;

procedure TDeployer.RegisterPACLient;
var
  newPAClientChannel: IDeployChannel;
begin
  newPAClientChannel:=TPAClientChannel.Create(fRemoteProfile, fPaclientPath,
                                                fDelphiVersion, fPlatform);
  newPAClientChannel.ChannelName:='PAClient';
  newPAClientChannel.Verbose:=fVerbose;
  newPAClientChannel.ProjectRoot:=fProjectRoot;
  newPAClientChannel.LogExceptions:=fLogExceptions;
  fDeployChannels.Add(newPAClientChannel);
end;

procedure TDeployer.SetConfig(const Value: String);
begin
  fConfig := Value;
  fConfig[Low(fConfig)] := UpCase(fConfig[Low(fConfig)]);
end;

procedure TDeployer.SetupChannels;
var
  tmpChannel: IDeployChannel;
begin
  for tmpChannel in fDeployChannels do
    tmpChannel.SetupChannel;
end;

// Execute a custom command on the remote server
// This is done by creating a temporary file with the command and executing it in the shell
procedure TDeployer.ExecuteCommand(const aProjectPath, aCommand: String; const forcePlain: boolean; OnFinished: TProc<TStringList>);
var
  Text, Cmd, TempFile:  String;
  tmpChannel: IDeployChannel;
begin
  //ParseProject(aProjectPath);

  // Check if there is a remote profile and try to find one. Must be after the project is parsed
  SetupChannels;

  // Replace any custom parameters in the command
  Cmd := aCommand.Replace('$PROOT', fProjectRoot, [rfReplaceAll]);

  // Build a temp file with the command as content
  TempFile := TPath.GetTempFileName;

//  // Build different files for Windows or OSX
  if (fPlatform = 'Win32') or (fPlatform = 'Win64') or (forcePlain) then
    Text := Cmd
  else
    Text := '#!/bin/bash' + #10 + Cmd;

  TFile.WriteAllText(TempFile, Text);

  // Deploy the file and execute it with the 5 operation
  // The file is deployed to the root deployment folder (the one above the project root folder) and executed there
  Writeln('Executing custom command: ' + aCommand);

  for tmpChannel in fDeployChannels do
  begin
    if tmpChannel is TPAClientChannel then
      if (not tmpChannel.DeployFile(TempFile,'.',5,'', OnFinished)) and (not fIgnoreErrors) then
      if fLogExceptions then
      begin
        Writeln('Error in '+tmpChannel.ChannelName+'. Command Error.');
        Halt(1);
      end
      else
        raise Exception.Create('Error in '+tmpChannel.ChannelName+'. Command Error.');
  end;

  TFile.Delete(TempFile);
  for tmpChannel in fDeployChannels do
    tmpChannel.CloseChannel;
end;

procedure TDeployer.ExecuteCommandFile(const aProjectPath, filename: String);
begin
  var completePath := '';
  if FileExists(filename) then  // check for absolute path first.
  begin
    completePath := filename;
  end
  else // check for relative path first.
  begin
    var s := TPath.Combine(ProjectPath, filename);
    if FileExists(s) then
      completePath := s
    else
     if (not fIgnoreErrors) then
        if fLogExceptions then
        begin
          Writeln('Command file does not exists. Command Error.');
          Halt(1);
        end
        else
          raise Exception.Create('Command file does not exists. Command Error.');
  end;

  Writeln('Command file found: ' + completePath);
  var sl := TStringList.Create();
  try
    sl.LoadFromFile(completePath);
    for var i := 0 to sl.count - 1 do
    begin
      var s := Trim(sl[i]);
      ExecuteCommand(aProjectPath, s, true);
    end;
  finally
    sl.Free;
  end;
end;

procedure TDeployer.DumpRemoteDirectory(const aProjectPath, directoryName, sepFiles: String);
begin
  var dir := TPath.Combine(ProjectPath, directoryName);
  if TDirectory.Exists(dir) then
    TDirectory.Delete(dir, true);
  TDirectory.CreateDirectory(dir);

  for var tmpChannel in fDeployChannels do
  begin
    if tmpChannel is TPAClientChannel then
      if (not tmpChannel.RetrieveResult(sepFiles, dir)) and (not fIgnoreErrors) then
        if fLogExceptions then
        begin
          Writeln('Error in '+tmpChannel.ChannelName+'. Command Error.');
          Halt(1);
        end
        else
          raise Exception.Create('Error in '+tmpChannel.ChannelName+'. Command Error.');
  end;

end;

function TDeployer.PlatFormNeedsCodeSigning: Boolean;
begin
  result := false;
  var p := Platform.ToUpper;
  if p.Contains('OSX') or p.Contains('IOS64') then
    result := true;
end;

function TDeployer.PlatFormNeedsInstaller: Boolean;
begin
  result := false;
  var p := Platform.ToUpper;
  if p.Contains('OSX') then
    result := true;
end;

end.
