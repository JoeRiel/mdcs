##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE CMD DataFile
##MODULE mdc[DataFile]
##HALFLINE return the path to a data file provided with the toolbox
##AUTHOR   Joe Riel
##DATE     Apr 2012
##CALLINGSEQUENCE
##- \CMD('filename')
##PARAMETERS
##- 'filename' : ::string::; basename and extension of file
##RETURNS
##- ::string::
##DESCRIPTION
##- Return the path to 'filename', which is located in
##  the data directory of the installed toolbox.
##  If the file does not exist, raise an error.
##EXAMPLES
##> mdc:-DataFile("Sample.mpl");
##SEEALSO
##- "kernelopts"
##TEST
## $include <test_macros.mi>
## AssignFUNC(DataFile):
##
## Try("1", FUNC(), "/home/joe/maple/toolbox/emacs/data/");
## Try("1", FUNC("Sample.mpl"), "/home/joe/maple/toolbox/emacs/data/Sample.mpl");

DataFile := proc( filename :: string := "", $ )
local file;
    file := StringTools:-Join( [ kernelopts('toolboxdir' = "emacs")
                                 , "data"
                                 , filename
                               ], kernelopts('dirsep')
                             );
    if not FileTools:-Exists(file) then
        error "file %1 does not exist", file;
    end if;

    file;

end proc:

