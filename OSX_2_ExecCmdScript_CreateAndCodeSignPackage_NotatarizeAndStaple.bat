cd\
cd "C:\Dev\Delphi\boxoffice-3-developer\client-side\boxoffice\src\project files"

C:\Dev\Delphi\3Party\EmbDeploy\Win32\release\embdeploy.exe -platform OSX64 -verbose -profile "macPro-m2-Ed" -certificateNameDeveloper "Developer ID Application: Cramgo BV (FZRAYS6PHS)" -certificateNameInstaller "Developer ID Installer: Cramgo BV (FZRAYS6PHS)" -appleId "espelt@cramgo.nl" -AppSpecificPasswordEncoded "853F47C85F980780CF3CE532A4577BB6E456080C7EB9ECA09F177775D849A8A7A5EE5D147F731C61C1BD5765FAB3C363BAB3361C04652294162B458CE5F52284C946F35DD545FA95BFA2AF3F072C1D0A92003E071DBA097BE3FD16D14425789365CA9A8422F1" -commandBefore "../../../Installers/mac/Scripts/BoxOffice-Jenkins.txt" -dumpRemoteResultDir "..\..\..\macDeploy" -dumpSepFilenames "Boxoffice.app.zip;Boxoffice.pkg" 

pause