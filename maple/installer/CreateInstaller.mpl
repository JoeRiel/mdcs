#{{{ InstallScript
InstallScript :=  proc()
local config
    , file
    , Install
    , is_system
    , MakePath
    , pdir
    , result
    , src
    , srcdir
    , tboxdir
    ;
global LispDir, InfoDir, DirFile, UpdateDir;

uses FT = FileTools, ST = StringTools;

    #{{{ Assign MakePath
    MakePath := proc()
        StringTools:-Join([_passed], kernelopts(':-dirsep'));
    end proc;
    #}}}
    #{{{ Assign tboxdir
    is_system := ToolboxInstaller:-Data:-Get("system_installation");
    pdir := ToolboxInstaller:-Tools:-GetInstallDirectory(is_system);
    tboxdir := MakePath(pdir, ToolboxInstaller:-Data:-Get("toolbox_name"));
    #}}}
    #{{{ Assign Install procedure
    Install := proc(srcdir, dstdir, files :: list)
    local file, src, dst;
    uses FT=FileTools;
        if not FT:-Exists(dstdir) then
            printf("Creating directory %s\n", dstdir);
            FT:-MakeDirectory(dstdir, 'recurse'=true);
        end if;
        for file in files do
            src := MakePath(srcdir,file);
            dst := MakePath(dstdir,file);
            printf("Copying %s --> %s\n", src, dst);
            FT:-Copy(src,dst,'force'=true);
        end do;
    end proc;
    #}}}
    #{{{ Assign Defaults
    LispDir := MakePath(kernelopts('homedir'), ".emacs.d", "mds");
    InfoDir := MakePath(kernelopts('homedir'), "share", "info");
    DirFile := MakePath(InfoDir, "dir");
    UpdateDir := proc(dirfile::string, file::string)
    local cmd;
        cmd := sprintf("ginstall-info --dir-file=%s %s"
                       , dirfile
                       , file
                      );
        ssystem(cmd);
    end proc;
    #}}}
    #{{{ Read configuration file
    config := MakePath(tboxdir, "config.mpl");
    if FileTools:-Exists(config) then
        printf("Reading configuration file %s\n", config);
        read config;
    else
        printf("Configuration file %s does not exist, using default locations.\n", config);
    end if;
    #}}}
    #{{{ Install lisp files
    srcdir := MakePath(tboxdir, "lisp");
    Install(srcdir, LispDir, FT:-ListDirectory(srcdir,'returnonly'="*.el"));
    #}}}
    #{{{ Install info files
    Install(MakePath(tboxdir, "info"), InfoDir, ["mds","maplev"]);
    #}}}
    #{{{ Update dir node
    try
        for file in ["mds","maplev"] do
            src := MakePath(tboxdir, "info", file);
            result := UpdateDir(DirFile, src);
            if result[1] <> 0 then
                error;
            end if;
        end do;
    catch:
        printf("Problem updating dir node.  Edit config file FUCK YOU BILL GATES '%s', or update the dir node manually.\n"
                , config );
    end try;
    printf("Problem updating dir node.  Edit config file '%s' to customize UpdateDir, or update the dir node manually.\n"
           , config );
    #}}}
    NULL;
end proc:
#}}}
#{{{ CreateInstaller
CreateInstaller := proc()

local installer, version;

    # This is updated by bin/version
    version := "0.1.1.21";

    installer := sprintf("mdcs-installer-%s.mla", version);

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

                                            , "../maplev/lisp/maplev.el" = "lisp/maplev.el"

                                            , "doc/README-installer" = "README-installer"
                                            , "doc/mds" = "info/mds"
                                            , "doc/mds.html" = "doc/mds.html"

                                            , "../maplev/doc/maplev" = "info/maplev"
                                            , "../maplev/doc/maplev.html" = "doc/maplev.html"

                                            , "maple/installer/config.mpl" = "_config.mpl"

                                           ]
                            , 'welcome' = [ 'text' = cat( sprintf("Welcome to the installer for the Maple Debugger Client/Server, version %s.\n", version)
                                                          , "\n"
                                                          , "This installer unpacks the Maple archive and help database to a folder under the user's HOME directory.\n"
                                                        )

                                          ]
                            , 'installation' = [NULL
                                                , 'text' = ( "Installing the Lisp and Info files.\n"
                                                             "By default, the lisp files are installed in $HOME/.emacs.d/mds\n"
                                                             "and the info files in $HOME/share/info.\n"
                                                             "To change these defaults, stop the installation, rename the file _config.mpl\n"
                                                             "to config.mpl, edit it appropriately, and restart the installation.\n"
                                                             "See the file README-installer for details."
                                                           )
                                                , 'script' = eval(InstallScript)
                                               ]
                            , 'finish' = [NULL
                                          , 'text' = ("To finish the installation, see the README-installer file.\n\n"
                                                      "For help with mdc, type ?mdc in Maple." )
                                          , 'script' = proc() help("mdc") end proc
                                         ]


                            , 'width' = 1000
                           );
end proc:
#}}}

CreateInstaller();