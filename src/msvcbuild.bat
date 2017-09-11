@setlocal
@set MSCOMPILE=cl /nologo /O2 /W3 /c /D_CRT_SECURE_NO_WARNINGS
@set MSLINK=link /nologo
@set MSLIB=lib /nologo

@if "%1" == "static" goto STATIC
@if not exist bin\ (
  mkdir bin
)
%MSCOMPILE% /MT /DSTANDALONE=1 miniscm.c
%MSLINK% /out:bin\miniscm.exe miniscm.obj

@goto END

:STATIC
@if not exist lib\ (
  mkdir lib
)
%MSCOMPILE% /MT /DSTANDALONE=0 miniscm.c
%MSLIB% /out:lib\miniscm.lib miniscm.obj

:END
del miniscm.obj
