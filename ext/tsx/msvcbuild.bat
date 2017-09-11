@setlocal
@set SCHEME_H_DIR=..\..\src
@set MSCOMPILE=cl /nologo /O2 /W3 /c /D_CRT_SECURE_NO_DEPRECATE /D_WINSOCK_DEPRECATED_NO_WARNINGS /I%SCHEME_H_DIR%
@set MSLIB=lib /nologo
@set MSLINK=link /nologo

@if not exist lib\ (
  mkdir lib
)

%MSCOMPILE% /MT tsx.c
%MSLIB% /out:lib\tsx.lib tsx.obj ws2_32.lib

del tsx.obj
