#{{{ CreateInstaller
CreateInstaller := proc()

local installer, version;

    # This is updated by bin/version
    version := "0.1.4";

    installer := "mdcs-installer.mla";

    if FileTools:-Exists(installer) then
        FileTools:-Remove(installer);
    end if;

    InstallerBuilder:-Build("mdc"
                            , 'target' = installer
                            , 'version' = version
                            , 'author' = "Joe Riel"
                            , 'manifest' = [NULL
                                            , "mdc.mla" = "lib/mdc.mla"
                                            , "mdc.hdb" = "lib/mdc.hdb"

                                            , "lisp/mds-client.el" = "lisp/mds-client.el"
                                            , "lisp/mds-cp.el" = "lisp/mds-cp.el"
                                            , "lisp/mds-login.el" = "lisp/mds-login.el"
                                            , "lisp/mds-out.el" = "lisp/mds-out.el"
                                            , "lisp/mds-re.el" = "lisp/mds-re.el"
                                            , "lisp/mds-ss.el" = "lisp/mds-ss.el"
                                            , "lisp/mds-wm.el" = "lisp/mds-wm.el"
                                            , "lisp/mds.el" = "lisp/mds.el"

                                            , "doc/mds" = "info/mds"

                                           ]
                            , 'welcome' = [ 'text' = cat( sprintf("Welcome to the installer for the Maple Debugger Client/Server, version %s.\n", version)
                                                          , "\n"
                                                          , "This installer unpacks the Maple archive and help database to a folder under the user's HOME directory.\n"
                                                        )

                                          ]
                            , 'installation' = [NULL
                                                , 'text' = "Installing the Maple package mdc and the Emacs package mds."
                                                , 'script' = proc()
                                                             local elispdir
                                                                 , dirsep
                                                                 , file, files
                                                                 , is_system
                                                                 , pdir
                                                                 , srcdir
                                                                 ;
                                                             uses FT = FileTools, ST = StringTools;
                                                                 dirsep := kernelopts('dirsep');
                                                                 # mdc(); DEBUG();
                                                                 elispdir := ST:-Join([kernelopts('homedir'), ".emacs.d", "mds"], dirsep);
                                                                 is_system := ToolboxInstaller:-Data:-Get("system_installation");
                                                                 pdir := ToolboxInstaller:-Tools:-GetInstallDirectory(is_system);
                                                                 srcdir := ST:-Join([pdir, ToolboxInstaller:-Data:-Get("toolbox_name"), "lisp"], dirsep);
                                                                 files := FT:-ListDirectory(srcdir,'returnonly'="*.el");
                                                                 for file in files do
                                                                     FT:-Copy(cat(srcdir,dirsep,file), cat(elispdir,dirsep,file), 'force'=true);
                                                                 end do;
                                                                 NULL;
                                                             end proc
                                               ]
                            , 'finish' = [NULL
                                          , 'text' = ( "For help with mdc, type ?mdc in Maple." )
                                         ]


                            , 'width' = 1000
                            #, 'welcome' = ['script' = proc() mdc(); DEBUG(); NULL end proc]
                           );
end proc:
#}}}

CreateInstaller();
