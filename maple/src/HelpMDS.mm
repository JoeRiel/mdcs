#LINK mdc.mpl
##INCLUDE ../include/mpldoc_macros.mpi
##
##PROCEDURE(help) mdc[HelpMDS]
##HALFLINE display the html help page for the Maple Debugger Server
##INDEXPAGE mdc[Exports],HelpMDS,display the html help page for the Maple Debugger Server
##AUTHOR   Joe Riel
##DATE     Dec 2011
##CALLINGSEQUENCE
##- mds:-HelpMDS\(\)
##DESCRIPTION
##- The `HelpMDS` command displays the html version of the help page
##  for the Maple Debugger Server (MDS).
##  **This command only works on Windows machines.**
##
##- The usual help page for MDS is in **info** format
##  and, if properly installed, is available in Emacs.
##  Because installing the info page and updating the **dir**
##  node requires manual intervention on a Windows machine,
##  this command provides easy access to the html
##  version of the help page.
##
##- The html file is located in the **doc** subdirectory
##  of the `mdc` installation.  This command opens that
##  file in the default system browser.
##
##EXAMPLES(notest,noexecute)
##> mdc:-HelpMDS();
##SEEALSO
##- "mdc"

HelpMDS := proc()
local html,platform,shellExecute,uri;

    platform := kernelopts('platform');
    if platform <> "windows" then
        error "this command is only available on Windows machines";
    end if;
    html := FileTools:-JoinPath([kernelopts('toolboxdir' = "emacs"), "doc", "mds.html"]);
    if not FileTools:-Exists(html) then
        error "file '%1' does not exist", html;
    end if;
    uri := sprintf("file://%s", html);
    shellExecute := define_external('ShellExecuteA'
                                    , 'hwnd' :: integer[4]
                                    , 'lpOperation' :: string
                                    , 'lpFile' :: string
                                    , 'lpParameters' :: string
                                    , 'lpDirectory' :: string
                                    , 'nShowCmd' :: integer[4]
                                    , 'RETURN' :: integer[4]
                                    , 'LIB' = "C:\\WINDOWS\\SYSTEM32\\shell32.dll"
                                   );
    shellExecute(0,"",uri,"","",1);
    NULL
end proc;
