@echo off

set VERSION=1.5
:: Modify per your setup.  If MAPLE is already defined, it uses the definition.
set MAPLECLI=c:\math\maple15\bin.win\cmaple.exe
:: The escaped backslashes are there to allow this work with the maple script.
set MLA="\\\"mdcs-installer-%VERSION%.mla\\\""
:: %MAPLECLI% -c "march(open,%MLA%)" -c done

set EMACS=c:\emacs\emacs-23.3-bin-i386\emacs-23.3\bin\emacs

%EMACS% --batch --no-site-file --no-init-file --eval "(push \\\"%A\\\" load-path)" --funcall=batch-byte-compile
                       "%{}s 2>&1"