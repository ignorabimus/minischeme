@setlocal
@set SCHEME_H_DIR=..\..\src
@set MSCOMPILE=cl /nologo /O2 /W3 /c /D_CRT_SECURE_NO_DEPRECATE /I%SCHEME_H_DIR%
@set MSLIB=lib /nologo
@set REGSRC=re.c regcomp.c regexec.c regerror.c regfree.c
@set OBJPRODN=re.obj regcomp.obj regexec.obj regerror.obj regfree.obj

@if not exist lib\ (
  mkdir lib
)

%MSCOMPILE% /MT %REGSRC%
%MSLIB% /out:lib\re.lib %OBJPRODN%

del %OBJPRODN%
