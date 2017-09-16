cd ..\src
call msvcbuild.bat static

cd ..\ext\re
call msvcbuild.bat

cd ..\..\ext\tsx
call msvcbuild.bat

cd ..\..\example
@setlocal
@set SCHEME_H_DIR=..\src
@set MSCOMPILE=cl /nologo /O2 /W3 /c /D_CRT_SECURE_NO_WARNINGS /I%SCHEME_H_DIR%
@set MSLINK=link /nologo
@set EXT_LIBS=..\ext\re\lib\re.lib ..\ext\tsx\lib\tsx.lib

@if not exist bin\ (
  mkdir bin
)

%MSCOMPILE% /MT repl.c
%MSLINK% /out:bin\repl.exe repl.obj ..\src\lib\miniscm.lib %EXT_LIBS%

del repl.obj
