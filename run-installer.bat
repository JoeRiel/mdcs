@echo off

set VERSION=1.12.3

:: Modify per your setup. 
set MAPLECLI="%PROGRAMFILES%\Maple 15\bin.X86_64_WINDOWS\cmaple.exe"

:: The escaped backslashes are there to allow this work with the maple script.
set MLA="\\\"mdcs-installer-%VERSION%.mla\\\""

%MAPLECLI% -c "march(open,%MLA%)" -c done
