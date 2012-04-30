##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE SUBMOD LineInfo
##DEFINE THISMOD \PKG[\SUBMOD]
##MODULE \THISMOD
##HALFLINE methods for lineinfo
##AUTHOR   Joe Riel
##DATE     Apr 2012
##DESCRIPTION
##- Maple 16 partially implements a **lineinfo** feature that,
##  when a Maple source file is read (with "read"),
##  records the filename and statement positions
##  of each procedure in the file.
##
##  Currently this information is discarded when the procedures
##  are saved to a Maple archive (.mla file).
##
##- The following demonstrates the basic operations.
##
##> srcfile := mdc:-DataFile("Sample.mpl");
##> read srcfile;
##- Assign the `lineinfo` output of "debugopts".
##> lf := [debugopts('lineinfo'=MyModule:-A)];
##- Note that the paths are absolute.
##  That is because the `read` command used an absolute path.
##  This will probably be required when building into an mla.
##  Any relevant include paths will also have to be absolute paths.
##- Read the source file to a string.
##> src := FileTools:-Text:-ReadFile(srcfile):
##- Assign a procedure to print the source corresponding to a
##  statement number.  The 0-th statement is the entire procedure.
##>  print_statement := proc(st)
##>> local b,e;
##>>     b := 1+op([st+1,3],lf);
##>>     e := 1+op([st+1,4],lf);
##>>     printf("%s\n", substring(src, b..e));
##>> end proc:
##> showstat(MyModule:-A);
##- Print the source for the procedure.
##> print_statement(0);
##- Print the source for selected statements.
##> print_statement(1);
##> print_statement(2);
##> print_statement(3);
##> print_statement(4);
##
##SECTION Design
##- The following operations are required:
##
##-- Forward lookup: given the statement number of a procedure,
##  return the filename and file position.
##-- Reverse lookup: given the position in a source file,
##  return the corresponding procedure and statement number.
##
##- If true source code debugging is to be implemented, the
##  forward lookup must be efficient.
##
##
##SUBSECTION Data Structures
##- Assume that the lineinfo data is generated on-the-fly,
##  via a call to ~debugopts('lineinfo'=procname)~.  That
##  should only be done once, when a procedure is first entered.
##  The data is cached---either in Maple, Emacs, or both.
##  If cached only in Maple, the filename and character position
##  must be sent with each statement.
##
##  Development is easier if caching is done in Maple.
##  Performance *may* be better if it is mirrored in Emacs,
##  because then we only need the procedure id (address) and
##  statement number, which are already being sent.
##
##ENDSUBSECTION
##
##
##TEST
## $include <AssignFunc.mi>
## macro(NE=testnoerror):
### mdc(mdc:-LineInfo:-StoreAll, mdc:-LineInfo:-Get);
##
## Try[NE]("1.0", proc() read mdc:-DataFile("Sample.mpl"); end());
## Try[NE]("1.1", addressof(MyModule:-A), 'assign'="addr");
## Try("1.2", mdc:-LineInfo:-Get(addr,1), "/home/joe/maple/toolbox/emacs/data/Sample.mpl",  8, 113, 119);
## Try("1.3", mdc:-LineInfo:-Get(addr,2), "/home/joe/maple/toolbox/emacs/data/Sample.mpl", 10, 156, 244);

LineInfo := module()

export Get
    ,  StoreAll
    ;

local ModuleLoad
    , Info
    ;

    #{{{ ModuleLoad

    ModuleLoad := proc()
        # Initialize the info table
        Info := table('sparse');
    end proc;

    #}}}
    #{{{ Get

##DEFINE PROC Get
##PROCEDURE \THISMOD[\PROC]
##HALFLINE return the source filename and statement position
##AUTHOR   Joe Riel
##DATE     Apr 2012
##CALLINGSEQUENCE
##- \PROC('addr','statement')
##PARAMETERS
##- 'addr'      : ::integer::; Maple address of procedure
##- 'statement' : ::posint::; statement number
##RETURNS
##- `(filename, lineno, charbeg, charend)`
##-- `filename` : ::string::; source file name
##-- `lineno`   : ::posint::; line number
##-- `charbeg`  : ::posint::; file character position of beginning of statement
##-- `charend`  : ::posint::; file character position of end of statement

    Get := proc( addr, statement :: posint )
    local i,info,datum;

        info := Info[addr];

        if info = 0 then
            StoreAll(addr);
            return thisproc(_passed);
        elif info = NULL then
            return NULL;
        end if;

        datum := info:-statement_position[statement];

        ( info:-filenames[datum[1]]
         , seq(datum[i], i = 2..4)  # line, beg, end
        );

    end proc;
    #}}}
    #{{{ StoreAll

    StoreAll := proc( addr )
    local filenames,i,info;

        if Info[addr] <> 0 then
            return NULL
        end if;

        info := [debugopts('lineinfo' = pointto(addr))];

        if info = [] then
            # no lineinfo available for prc
            Info[addr] := NULL;

        else
            filenames := ListTools:-MakeUnique(map2(op,1,info));
            # Convert filenames to integers, corresponding to position in 'filenames'
            info := subs([seq(filenames[i]=i, i=1..numelems(filenames))], info);
            # Insert filenames and an Array of the statement positions
            # into a two-field record, and store in the sparse table
            # Info, which is indexed by the procedure address.
            Info[addr] := Record['packed'](':-filenames' = filenames
                                           , ':-statement_position' = Array(0..numelems(info)-1, 1..4
                                                                            , info
                                                                            , 'datatype'=integer[4]
                                                                           )
                                          );
        end if;
        return NULL
    end proc;
    #}}}

end module:
