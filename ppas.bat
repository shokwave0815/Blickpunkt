@echo off
SET THEFILE=C:\dev\LazarusProjects\Blickpunkt\Blickpunkt.exe
echo Linking %THEFILE%
C:\dev\Lazarus\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup    -o C:\dev\LazarusProjects\Blickpunkt\Blickpunkt.exe C:\dev\LazarusProjects\Blickpunkt\link6028.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
