call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
cd\
cd "C:\Dev\Delphi\boxoffice-3-developer\client-side\boxoffice\src\project files"
msbuild Boxoffice.dproj /t:Build /p:Config=Release /target:Deploy /p:platform=OSX64

pause
