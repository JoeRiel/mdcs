@echo off

set VERSION=1.5
:: Modify per your setup.
IF %processor_architecture == AMD64 set MAPLECLI="%PROGRAMFILES(X86)%\Maple 15\bin.X86_64_WINDOWS\cmaple.exe" ELSE set MAPLECLI="%PROGRAMFILES%\Maple 15\bin.win\cmaple.exe"

:: The escaped backslashes are there to allow this work with the maple script.
set MLA="\\\"mdcs-installer-%VERSION%.mla\\\""

%MAPLECLI% -c "march(open,%MLA%)" -c done
