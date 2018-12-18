@setlocal
cd ..\..\replxx
@if not exist build\ (
  mkdir build
)
cd build
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS_RELEASE="/MT /O2 /Ob2 /DNDEBUG" ..
msbuild replxx.vcxproj /p:Configuration=Release
@endlocal

@setlocal
cd ..\src
call msvcbuild.bat static
@endlocal

@setlocal
cd ..\ext\re
call msvcbuild.bat
@endlocal

@setlocal
cd ..\ext\tsx
call msvcbuild.bat
@endlocal

@setlocal
@set REPLXX_H_DIR=..\..\replxx\include
@set SCHEME_H_DIR=..\src
@set MSCOMPILE=cl /nologo /O2 /W3 /c /D_CRT_SECURE_NO_WARNINGS /I%SCHEME_H_DIR% /I%REPLXX_H_DIR%
@set MSLINK=link /nologo
@set EXT_LIBS=..\ext\re\lib\re.lib ..\ext\tsx\lib\tsx.lib ..\..\replxx\build\Release\replxx.lib

@if not exist bin\ (
  mkdir bin
)

%MSCOMPILE% /MT repl.c
%MSLINK% /out:bin\repl.exe repl.obj ..\src\lib\miniscm.lib %EXT_LIBS%

del repl.obj
@endlocal
