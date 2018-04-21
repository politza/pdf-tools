@echo off

cd %APPVEYOR_BUILD_FOLDER%

echo Compiler: %COMPILER%
echo Architecture: %MSYS2_ARCH%
echo Platform: %PLATFORM%
echo MSYS2 directory: %MSYS2_DIR%
echo MSYS2 system: %MSYSTEM%
echo Bits: %BIT%

REM Create a writeable TMPDIR
mkdir %APPVEYOR_BUILD_FOLDER%\tmp
set TMPDIR=%APPVEYOR_BUILD_FOLDER%\tmp

IF %COMPILER%==msys2 (
  @echo on
  SET "PATH=C:\%MSYS2_DIR%\%MSYSTEM%\bin;C:\%MSYS2_DIR%\usr\bin;C:\%MSYS2_DIR%\home\appveyor\.cask\bin;%PATH%"

  bash -lc "pacman -S --needed --noconfirm git"

  REM dependencies
  bash -lc "pacman -S --needed --noconfirm mingw-w64-x86_64-zlib mingw-w64-x86_64-libpng mingw-w64-x86_64-poppler mingw-w64-x86_64-imagemagick openssl mingw-w64-x86_64-openssl"

  REM Set up emacs
  bash -lc "pacman -S --needed --noconfirm mingw-w64-x86_64-emacs"

  REM Set up Cask
  bash -lc "curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python"
)
