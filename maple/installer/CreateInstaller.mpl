#{{{ InstallScript
InstallScript :=  proc()
local cmd
    , config
    , elfiles
    , Emacs
    , file
    , Install
    , is_system
    , MakePath
    , pdir
    , platform
    , result
    , src
    , srcdir
    , tboxdir
    ;
global LispDir, InfoDir, DirFile, MapleLib, UpdateDir;

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
    Install := proc(srcdir, dstdir, files :: list, { clear :: truefalse := false } )
    local content, file, src, dst;
    uses FT=FileTools;
        if not FT:-Exists(dstdir) then
            printf("Creating directory %s\n", dstdir);
            FT:-MakeDirectory(dstdir, 'recurse'=true);
        elif clear then
            content := FT:-ListDirectory(dstdir);
            for file in content do
                file := MakePath(dstdir,file);
                printf("Deleting file %s\n", file);
                FT:-Remove(file);
            end do;
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
    Emacs := "emacs";
    MapleLib := MakePath(kernelopts('homedir'), "maple", "toolbox", "emacs", "lib");
    LispDir := MakePath(kernelopts('homedir'), ".emacs.d", "maple");
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
    #{{{ Install Maple files
    if assigned(MapleLib) then
        srcdir := MakePath(tboxdir,"lib");
        if MapleLib <> srcdir then
            printf("\nInstalling Maple files...\n");
            Install(srcdir, MapleLib, ["mdc.mla", "mdc.hdb"]);
        end if;
    end if;
    #}}}
    #{{{ Install lisp files
    
    printf("\nInstalling lisp files...\n");
    srcdir := MakePath(tboxdir, "lisp");
    elfiles := FT:-ListDirectory(srcdir,'returnonly'="*.el");
    Install(srcdir, LispDir, elfiles);
    
    #}}}
    #{{{ Byte-compile lisp files

    platform := kernelopts(':-platform');
    
    if platform = "unix" then
        printf("\nByte-compiling lisp files...\n");
        
        elfiles := map[3](cat, LispDir, kernelopts('dirsep'), elfiles);
        
        cmd := sprintf("%s --batch --no-site-file --no-init-file "
                       "--eval \"(push \\\"%A\\\" load-path)\" "
                       "--funcall=batch-byte-compile "
                       "%{}s 2>&1"
                       , Emacs
                       , LispDir
                       , < elfiles >
                      );
        printf("%s\n", cmd);
        result := ssystem(cmd);
        if result[1] <> 0 then
            WARNING("problem byte-compiling lisp files:\n%1"
                    , result[2]
                   );
        end if;
    else
        WARNING("the lisp files were not automatically byte-compiled. "
                "Byte-compiling is not a requirement, but will "
                "allow the code to run faster.  You can manually byte-compile the "
                "files from inside Emacs using the command byte-recompile-directory. "
                "Launch Emacs, then type C-u 0 M-x byte-recompile-directory and "
                "select the directory where the lisp files were installed."
               );
    end if;

    #}}}
    #{{{ Install info files
    
    printf("\nInstalling info files...\n");
    Install(MakePath(tboxdir, "info"), InfoDir, ["mds","maplev"]);
    #}}}
    #{{{ Update dir node
    
    if platform = "unix" then
        printf("\nUpdating info dir node...\n");
        try
            for file in ["mds","maplev"] do
                src := MakePath(tboxdir, "info", file);
                result := UpdateDir(DirFile, src);
                if result[1] <> 0 then
                    error result[2];
                end if;
            end do;
        catch:
            WARNING("problem updating the dir node. "
                    "Edit config file '%1', or update the dir node manually; "
                    "see README-Installer.  The following error occured:\n%2"
                    , config
                    , result[2]
                   );
        end try;
    else
        WARNING("the dir file, used by Emacs help system to "
                "provide a menu of help topics, was not updated. "
                "You can do so manually. If you cannot figure out how to do so, "
                "an html version of the documentation for mds "
                "(the Emacs-based Maple Debugger Server) "
                "is provided in the doc subdirectory of the installation."
               );
    end if;
    #}}}
    NULL;
end proc:
#}}}
#{{{ CreateInstaller

CreateInstaller := proc()

local installer, version;
global InstallScript;

    # This is updated by bin/version
    version := "1.4";

    installer := sprintf("mdcs-installer-%s.mla", version);

    if FileTools:-Exists(installer) then
        FileTools:-Remove(installer);
    end if;

    InstallerBuilder:-Build(
        "emacs"
        , ':-target' = installer
        , ':-version' = version
        , ':-author' = "Joe Riel"
        , ':-manifest' = [NULL
                          , "mdc.mla" = "lib/mdc.mla"
                          , "mdc.hdb" = "lib/mdc.hdb"
                          , "../maplev/maplev.mla" = "lib/maplev.mla"

                          , seq(`=`(sprintf("lisp/%s",file)$2)
                                , file = [NULL
                                          , "mds-client.el"
                                          , "mds-cp.el"
                                          , "mds-login.el"
                                          , "mds-out.el"
                                          , "mds-patch.el"
                                          , "mds-re.el"
                                          , "mds-ss.el"
                                          , "mds-wm.el"
                                          , "mds.el"
                                         ])

                          , "../maplev/lisp/maplev.el" = "lisp/maplev.el"

                          , "doc/README-installer" = "README-installer"
                          , "doc/mds" = "info/mds"
                          , "doc/mds.html" = "doc/mds.html"

                          , "../maplev/doc/maplev" = "info/maplev"
                          , "../maplev/doc/maplev.html" = "doc/maplev.html"

                          , "maple/installer/config.mpl" = "_config.mpl"

                         ]
        , ':-welcome' = [ 'text' = cat( sprintf("Welcome to the installer for the Maple Debugger Client/Server, version %s.\n", version)
                                        , "\n"
                                        , "This installer unpacks the Maple archive and help database to a folder under the user's HOME directory.\n"
                                      )

                        ]
        , ':-installation' = [NULL
                              , 'text' = ( "Installing the Lisp and Info files...\n"
                                           "\n"
                                           "The default locations are\n"
                                           "  lisp files --> HOME/.emacs.d/maple\n"
                                           "  info files --> HOME/share/info\n"
                                           "where HOME = kernelopts('homedir').\n"
                                           "\n"
                                           "To change these defaults, stop the installation,\n"
                                           "rename the file _config.mpl to config.mpl,\n"
                                           "edit it appropriately, and restart the installation.\n"
                                           "See the file README-installer for details."
                                         )
                              , 'script' = eval(InstallScript)
                             ]
        , ':-finish' = [NULL
                        , 'text' = ("To finish the installation, see the README-installer file.\n\n"
                                    "For help with mdc, type ?mdc in Maple." )
                        , 'script' = proc() help("mdc") end proc
                       ]


        , ':-width' = 1000
                           );
end proc:

#}}}

CreateInstaller();
