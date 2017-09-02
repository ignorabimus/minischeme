@setlocal
@set TSCOMPILE=cl /nologo /O2 /W3 /c /D_CRT_SECURE_NO_WARNINGS
@set TSLINK=link /nologo
@set TSLIB=lib /nologo

@if "%1" == "static" goto STATIC
@if not exist bin\ (
  mkdir bin
)
%TSCOMPILE% /MT /DSTANDALONE=1 miniscm.c
%TSLINK% /out:bin\miniscm.exe miniscm.obj

@goto END

:STATIC
@if not exist lib\ (
  mkdir lib
)
%TSCOMPILE% /MT /DSTANDALONE=0 miniscm.c
%TSLIB% /out:lib\miniscm.lib miniscm.obj

:END
del miniscm.obj
