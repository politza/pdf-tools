@echo off

cd %APPVEYOR_BUILD_FOLDER%

REM Create a writeable TMPDIR
mkdir %APPVEYOR_BUILD_FOLDER%\pack
set PACKDIR=%APPVEYOR_BUILD_FOLDER%\pack

IF %COMPILER%==msys2 (
  @echo on
  SET "PATH=C:\%MSYS2_DIR%\%MSYSTEM%\bin;C:\%MSYS2_DIR%\usr\bin;C:\%MSYS2_DIR%\home\appveyor\.cask\bin;%PATH%"

  REM Copy epdfinfo.exe and all dependencies
  bash -lc "pushd /c/projects/pdf-tools; ldd server/epdfinfo.exe | grep mingw | cut -d' ' -f 3 | xargs -I {} cp {} ./pack/; cp server/epdfinfo.exe ./pack/; cp /mingw64/bin/*eay32.dll ./pack/"

  REM Package epdfinfo.exe and all dependencies
  7z a epdfinfo.zip %PACKDIR%\*.*
)
