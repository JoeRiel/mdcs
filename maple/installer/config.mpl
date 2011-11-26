# Optional configuration file used with mdcs-installer.
#
# To change the default locations where the installer
# installs files, rename this file to config.mpl
# and modify it accordingly.

# Assign a utility procedure for generating path names.
MakePath := () -> StringTools:-Join([_passed], kernelopts(':-dirsep')):

# Define the locations.  Actually, the values assigned
# here correspond to the default locations, but this file
# is not used unless it has been renamed as described above.

# LispDir : directory where lisp files are stored (~/.emacs.d/mds/)
# InfoDir : directory where info files are stored (~/share/info/)
# DirFile : file used to update the dir node for the info system (~/share/info/dir)

LispDir := MakePath(kernelopts('homedir'), ".emacs.d", "mds"):
InfoDir := MakePath(kernelopts('homedir'), "share", "info"):
DirFile := MakePath(InfoDir, "dir"):

# Procedure to update dir/info entries.
# This (the default) uses the O/S command ginstall-info
# to do the actual work.  If your O/S does not provide
# a command to do this, modify this procedure to do nothing,
# then update the file manually.
UpdateDir := proc(dirfile::string, file::string)
local cmd;
    cmd := sprintf("ginstall-info --dir-file=%s %s"
                   , dirfile
                   , file
                  );
    ssystem(cmd);
end proc:


