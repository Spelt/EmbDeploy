{
Automated deployer for Embarcadero RAD Studio projects
Created by Vladimir Georgiev, 2013
MIT License (MIT)
}
program embdeploy;
{$APPTYPE CONSOLE}
{$R *.res}
uses  System.SysUtils,
  System.IOUtils,
  Deployer in 'Deployer.pas',
  DeployChannels in 'DeployChannels.pas';

const
  VERSION = '2.0';
var
  Deployer: TDeployer;
  Project, Param, DelphiVer: String;
  logExceptions: boolean;

// Display the parameters usage information
procedure ShowUsage;

  procedure ShowParam(aParam, aText: String);
  begin
    Writeln(Format('  %-16s %s', [aParam, aText]));
  end;

begin
  WriteLn('');
  Writeln('Usage: embdeploy [-delphiver "ver"] -deploy|(-cmd "command")|(-bundle "zip") [-platform|-profile|-config|-proot "name"] [-ignore] ProjectName');
  WriteLn('');
  ShowParam('ProjectName', 'Name (relative or absolute) of the project file (.dproj)');
  ShowParam('-delphiver "ver"',  'Delphi version to use the paclient from. It is the number from the HKCU/Software/Emb.. If not the latest installed ' +
                                'will be used');
  ShowParam('-deploy "name"', 'Deploy the project to the remote profile. If not specified the default one from ' +
                                'the IDE is used');
  ShowParam('-platform "name"', 'Platform to deploy (Win32, Win64, OSX64, iOSDevice, etc).');
  ShowParam('-profile "name"',  'Name of the remote profile to use. If not specified the default one for the platform is used');
  ShowParam('-config "name"',   'Release or Debug configuration. If not specified the default one from the project file is used');
  ShowParam('-proot "name"',    'Remote project root folder. If not specified a default one is generated from the project name');
  ShowParam('-cmd "command"', 'Execute an arbitrary command line on the remote server. The command is anything that ' +
                              'can be executed from a terminal or command line prompt. It is executed from ' +
                              'above the remote project folder. The command can contain the $PROOT parameter, which is ' +
                              'replaced with the project root folder, e.g. $PROOT/Contents/... becomes myproject.app/Contents/...');
  ShowParam('-ignore', 'Ignore errors reported by paclient.exe and continue deploying');
  ShowParam('-bundle "zipname"', 'Produce a ZIP archive of the files to be deployed. Useful for making a ZIP of an OSX project APP bundle');
  ShowParam('-verbose', 'Produces detailed debugging messages');
  ShowParam('-registerPAClient','Uses the PAClient to deploy the project');
  ShowParam('-registerFolder "folder"', 'OSX only: Creates the APP folder structure on Windows.'+
                                      ' Useful for building OSX without the need to use the paclient on OSX');
  ShowParam('-binaryFolder "folder"','The folder for the binary files. If not provided, the default location is assumed');
  ShowParam('-logExceptions','Logs any exceptions and quits instead of raising them');

  ShowParam('-codeSign', 'Code sign and notarize the deployment.');

  ShowParam('-appleId "name"',' You can find it in Project options - Deployment - Provisioning - Release Build type: Developer ID."');
  ShowParam('-appSpecificPassswordEncoded "name"','You can find it in the ouput of a manual deploy command from within the IDE. Its very long 204 chars."');
  ShowParam('-notarizationExtraOptions "name"','You can find it in Project options - Deployment - Provisioning - Release Build type: Developer ID."');
  ShowParam('-certificateNameDeveloper "name"','For OSX development the name of the "Developer ID Application Certificate". You can find it in Project options - Deployment - Provisioning - Build type: Developer ID."');
  ShowParam('-certificateNameInstaller "name"','For OSX installer the name of the "Developer ID Installer Certificate". You can find it in Project options - Deployment - Provisioning - Build type: Application Store."');
  ShowParam('-batchCmdFileAfterDeploy "command script file"', 'Same as cmd but points to a relative or absolute script file and it is fired AFTER code signing and notarizing the project. ' +
                                 'The command only must be enclosed with double quotes. An inline quote needs to be escaped.');
  ShowParam('-dumpRemoteResultDir "name"','A local directory relative to the project folder in which to dump the whole resulting deployment including code signing and notarization.');
  ShowParam('-dumpSepFilenames "name"','Used with -dumpRemoteResultDir. Define here a list of files like "file1.pkg;file2.zip"');


end;

// Check if the valid combination of parameters is passed
function ValidateParams: Boolean;
var
  tmpMessage: string;
begin
  Project := ParamStr(ParamCount);
  if not FileExists(Project) then
  begin
    tmpMessage:='Project "' + Project +'" not found';
    if logExceptions then
    begin
      Writeln(tmpMessage);
      Halt(1);
    end
    else
      raise Exception.Create(tmpMessage);
  end;
  Result := FindCmdLineSwitch('deploy') or FindCmdLineSwitch('cmd') or FindCmdLineSwitch('bundle');
end;
// Main application body
begin
  try
    ExitCode := 1; // Default to error and change to success later
    Writeln('Automated deployer for Embarcadero RAD Studio projects - Version ' + VERSION);
    Writeln('Written by Vladimir Georgiev, 2013-2023');
    if FindCmdLineSwitch('?') or (ParamCount=0) then
    begin
      ShowUsage;
      Exit;
    end;
    logExceptions:=FindCmdLineSwitch('logExceptions');
    ValidateParams;

    if FindCmdLineSwitch('delphiver', Param) then
      DelphiVer := Param;
    Deployer := TDeployer.Create(DelphiVer);
    try
      Deployer.LogExceptions:=logExceptions;
      Deployer.ProjectPath := GetCurrentDir();
      if FindCmdLineSwitch('platform', Param) then
        Deployer.Platform := Param;
      if FindCmdLineSwitch('profile', Param) then
        Deployer.RemoteProfile := Param;
      if FindCmdLineSwitch('config', Param) then
        Deployer.Config := Param;
      if FindCmdLineSwitch('proot', Param) then
        Deployer.ProjectRoot := Param;
      Deployer.IgnoreErrors := FindCmdLineSwitch('ignore');
      Deployer.Verbose:=FindCmdLineSwitch('verbose');
      Deployer.BinaryFolder:='';
      if FindCmdLineSwitch('binaryFolder', Param) then
        Deployer.BinaryFolder:=Param;
      if FindCmdLineSwitch('registerPAClient') then
        Deployer.RegisterPACLient;
      if FindCmdLineSwitch('registerFolder', Param) then
        Deployer.RegisterFolder(Param, TPath.GetFileNameWithoutExtension(Project));

      Deployer.CodeSign := FindCmdLineSwitch('codeSign');
      if FindCmdLineSwitch('certificateNameDeveloper', Param) then
        Deployer.CertNameDev := param;
      if FindCmdLineSwitch('certificateNameInstaller', Param) then
        Deployer.CertNameInstaller := param;
      if FindCmdLineSwitch('appleId', Param) then
        Deployer.AppleId := param;
      if FindCmdLineSwitch('appSpecificPassswordEncoded', Param) then
        Deployer.AppSpecificPwEncoded := param;
      if FindCmdLineSwitch('notarizationExtraOptions', Param) then
      begin
        Deployer.NotarizationExtraOptions := param;
      end;

      Deployer.ParseProject(Project);

      // Deploy the project
      if FindCmdLineSwitch('deploy') then
      begin
        Deployer.DeployProject(Project);
        Writeln('Deployment complete');
      end;

      // Codesign the project
      if Deployer.CodeSign then
      begin
        Deployer.CodeSignProject();
        Writeln('CodeSigning project complete');

        Deployer.NotarizeProject();
        Writeln('Notarizing project complete');
      end;

      if FindCmdLineSwitch('batchCmdFileAfterDeploy', Param) then
      begin
        Deployer.ExecuteCommandFile(Project, Param);
        Writeln('Command file executed');
      end;

      // Create an installer for the project
      if FindCmdLineSwitch('certificateNameInstaller') then
      begin
        Deployer.CreateInstallerProject();
        Writeln('Creating an installer complete');

        Deployer.CodeSignInstaller();
        Writeln('Codesigning installer complete');

        Deployer.NotarizeInstaller();
        Writeln('Notarizing installer complete');
      end;

      // Execute a custom remote command
      if FindCmdLineSwitch('cmd', Param) then
      begin
        Deployer.ExecuteCommand(Project, Param);
        Writeln('Command executed');
      end;

      // Make a ZIP bundle of the project deployment files
      if FindCmdLineSwitch('bundle', Param) then
      begin
        Deployer.BundleProject(Project, Param);
        Writeln('ZIP bundle complete');
      end;

      if FindCmdLineSwitch('dumpRemoteResultDir', Param) then
      begin
        var sepFiles:= '.\*';
        var Param2 : string;
        if FindCmdLineSwitch('dumpSepFilenames', Param2) then
          sepfiles := Param2;
        Deployer.DumpRemoteDirectory(Project, Param, sepFiles);
        Writeln('Remote result directory dumped.');
      end;

      ExitCode := 0; // Success
    finally
      Writeln('Finished');
      Deployer.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Error deploying project:');
      Writeln(E.Message);
    end;
  end;

 {$IFDEF DEBUG}
 // Sleep(60000);
 {$ENDIF}

end.
