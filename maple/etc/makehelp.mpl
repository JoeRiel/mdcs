# Commands for loading *.mw files extracted by mpldoc into an hdb file

MakeHelpAll := proc(dir :: string, hdb :: string, $)
local file, files;
    files := FileTools:-ListDirectory(dir, 'returnonly' = "*.mw");
    for file in files do
        MakeHelp(FileTools:-AbsolutePath(file, dir), hdb);
    end do;
end proc:

MakeHelp := proc(filename :: string, hdb :: string, $)
local basename, parts, topic;
    basename := FileTools:-Filename( filename );
    if not StringTools:-RegMatch("(.*)\\.mw$", basename) then
        error "expected a Maple worksheet file, got %1", filename;
    end if;
    basename := basename[..-4];
    parts := StringTools:-Split(basename, "-");
    topic := cat(parts[1], seq(sprintf("[%s]", parts[i]), i=2..nops(parts)));
    makehelp(topic, filename, hdb);
    return NULL;
end proc:

