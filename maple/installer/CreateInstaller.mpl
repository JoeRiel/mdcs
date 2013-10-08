#{{{ InstallScript

InstallScript :=  proc()
local cmd
    , config
    , elfiles
    , file
    , Install
    , is_system
    , lispDir
    , MakePath
    , pdir
    , platform
    , result
    , src
    , srcdir
    , tboxdir
    ;
global Emacs, LispDir, InfoDir, DirFile, MapleLib, UpdateDir;

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
    # Use emacs to extract user-emacs-directory (typically ~/.emacs.d)
    cmd := sprintf("%s --batch --no-init-file --no-site-file "
                   "--eval \"(princ (file-truename user-emacs-directory))\""
                   , Emacs);
    result := ssystem(cmd);
    if result[1] <> 0 then
        WARNING("problem executing '%1':\n%s", cmd, result[2]);
        LispDir := MakePath(kernelopts('homedir'),"/.emacs.d/maple");
    else
        LispDir := cat(result[2], "maple");
    end if;
    MapleLib := MakePath(kernelopts('homedir'), "maple", "toolbox", "emacs", "lib");
    InfoDir := MakePath(kernelopts('homedir'), "share", "info");
    DirFile := MakePath(InfoDir, "dir");
    UpdateDir := proc(dirfile::string, file::string)
    local cmd;
        cmd := sprintf("ginstall-info --dir-file=%s %s"
                       , dirfile
                       , file
                      );
        result := ssystem(cmd);
        if result[1] <> 0 then
            error "problem executing '%1':/n%2", cmd, result[2];
        end if;
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
            Install(srcdir, MapleLib, ["mdc.mla", "mdc.hdb", "mdc.help"]);
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

    try
        printf("\nByte-compiling lisp files...\n");

        elfiles := map[3](cat, LispDir, kernelopts('dirsep'), elfiles);

        # Emacs requires forward-quotes
        if platform <> "unix" then
            lispDir := ST:-SubstituteAll(LispDir, "\\", "/");
        else
            lispDir := LispDir;
        end if;

        cmd := sprintf("%s --batch --no-site-file --no-init-file "
                       "--eval \"(push \\\"%A\\\" load-path)\" "
                       "--funcall=batch-byte-recompile-directory \"%A\""
                       , Emacs
                       , lispDir
                       , lispDir
                      );
        printf("%s\n", cmd);
        result := ssystem(cmd);
        if result[1] <> 0 then
            WARNING("problem byte-compiling lisp files:\n%1"
                    , result[2]
                   );
        end if;
    catch:
        WARNING("the lisp files were not automatically byte-compiled. "
                "Byte-compiling is not a requirement, but will "
                "allow the code to run faster.  You can manually byte-compile the "
                "files from inside Emacs using the command byte-recompile-directory. "
                "Launch Emacs, then type C-u 0 M-x byte-recompile-directory and "
                "select the directory where the lisp files were installed: "
                "%1"
                , lispDir
               );
        error;
    end try;

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
                UpdateDir(DirFile, src);
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

local file, elisp, elisp_map, installer, manifest, maplev_dir, maplev_lisp_dir, version;
global InstallScript;
uses FT = FileTools;

    # This is updated by bin/version
    version := "2.4.3";

    installer := sprintf("mdcs-installer-%s.mla", version);

    if FT:-Exists(installer) then
        FT:-Remove(installer);
    end if;

    maplev_dir := "/home/joe/emacs/maplev";
    maplev_lisp_dir := FT:-AbsolutePath("lisp", maplev_dir);

    elisp := sort(FT:-ListDirectory(maplev_lisp_dir, 'returnonly' = "*.el"));
    elisp_map := ( NULL
                   , seq(FT:-AbsolutePath(file, maplev_lisp_dir)
                         = FT:-AbsolutePath(file, "lisp")
                         , file = elisp
                        )
                   , seq(`=`(sprintf("lisp/%s",file)$2)
                         , file = [NULL
                                   #, "mds-cp.el"
                                   , "mds-client.el"
                                   , "mds-custom.el"
                                   , "mds-li.el"
                                   , "mds-login.el"
                                   , "mds-out.el"
                                   , "mds-patch.el"
                                   , "mds-queue.el"
                                   , "mds-re.el"
                                   , "mds-ss.el"
                                   , "mds-thing.el"
                                   , "mds-wm.el"
                                   , "mds.el"
                                  ])
                 );

    manifest := [NULL
                 , "mdc.mla" = "lib/mdc.mla"
                 , "mdc.hdb" = "lib/mdc.hdb"
                 , "mdc.help" = "lib/mdc.help"
                 , "../maplev/maplev.mla" = "lib/maplev.mla"

                 (* data files *)
                 , "data/Sample.mpl" = "data/Sample.mpl"

                 (* emacs stuff *)
                 , elisp_map
                 , FT:-AbsolutePath("doc/maplev", maplev_dir) = "info/maplev"
                 , FT:-AbsolutePath("doc/maplev.html", maplev_dir) = "doc/maplev.html"

                 , ".emacs" = ".emacs"

                 , "README-installer" = "README-installer"
                 , "doc/mds" = "info/mds"
                 , "doc/mds.html" = "doc/mds.html"

                 , "maple/installer/config.mpl" = "_config.mpl"

                ];

    InstallerBuilder:-Build(
        "emacs"
        , ':-target' = installer
        , ':-version' = version
        , ':-author' = "Joe Riel"
        , ':-welcome' = [ 'text' = cat( sprintf("Welcome to the installer for the "
                                                "Maple Debugger Client/Server, version %s.\n"
                                                , version)
                                        , "\n"
                                        , ( "This installer unpacks the Maple archive "
                                            "and help database to a folder under the user's HOME directory.\n"
                                          )
                                      )
                        ]
        , ':-manifest' = manifest
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
                        , 'script' = proc()
                                         printf("\n"
                                                "For help with the debugger client, type ?mdc in Maple.\n"
                                                "For help with the debugger server, look at the mds node "
                                                "in info.  An html version of the mds documentation "
                                                "is available in $HOME/maple/toolbox/doc/mdc.html.\n"
                                               );
                                     end proc
                       ]

        , ':-width' = 1000 );
end proc:

#}}}

CreateInstaller();
