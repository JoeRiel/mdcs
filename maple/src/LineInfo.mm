##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE SUBMOD LineInfo
##DEFINE THISMOD \PKG[\SUBMOD]
##DEFINE THISPROC \PKG[\SUBMOD][\PROC]
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
    ,  LookupStatement
    ,  Store
    ;

local ModuleLoad

##TOPIC Info
##HALFLINE table storing lineinfo information
##AUTHOR   Joe Riel
##DATE     May 2012
##DESCRIPTION
##- The 'Info' table stores accumulated lineinfo information.
##  Its indices consist of addresses of procedures, and filenames
##  of sources.
##
##- The table is created by calls to "Store".
##
##- An *address* entry associates the address of a procedure
##  with its source information.
##  The entry consists of a packed record with two fields,
##  'filenames' and 'positions'.
##
##-- 'filenames' : list of strings corresponding to the filenames
##  in which the procedure is defined.
##
##-- 'positions' : four column Array, indexed from 0 to `n`,
##  where `n` is the maximum statement number of the procedure.
##  Each row holds the data for one statement.
##  The 0 zero holds the data for the entire procedure.
##  The columns contain the following data:
##SET(lead=numeric)
##--- Index into 'filenames'.
##--- Line number of the file at which the statement begins.
##--- File offset of the beginning character position of the statement.
##--- File offset of the ending character position of the statement.
##UNSET
##
##-- The file offsets are from the beginning of the file (not the line).
##   An offset of 0 corresponds to the first character in the file.
##
##-- The end position does not include a terminator charactor (colon or semicolon).
##
##-- If a procedure has no *lineinfo* data, its *address* entry is `NULL`.
##
##- A *filename* entry associates a source filename with the procedures
##  it contains, either wholly or partially.
##
##-- An entry consists of an expression sequence of addresses.

    , Info
    ;

##PROCEDURE ModuleLoad

    ModuleLoad := proc()
        # Initialize the info table
        Info := table();
    end proc;

##DEFINE PROC Get
##PROCEDURE \THISPROC
##HALFLINE return the source filename and source position data
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
##DESCRIPTION
##- Return the source filename
##  and source position data
##  associated with a 'statement' number of a procedure,
##  given its address, 'addr'.
##TEST
## $include <AssignFunc.mi>
## $include <lineinfo.mpl>
## AssignFUNC(mdc:-LineInfo:-Get):
## AssignLocal(Store, mdc:-LineInfo:-Store):
## macro(NE='testnoerror'
##       ,TA='(verify,table(Or(truefalse,record(Or(truefalse,Array)))))'
##       ,RE='(verify,record(Or(truefalse,Array)))'
##       ,TE='testerror'
##      );
### mdc(FUNC):
##
## Try[NE]("1.0", proc() save f, "f.mpl"; read "f.mpl" end());
## Try[NE]("1.1", addressof(f), 'assign'="af"):
## Try[NE]("1.2", Store(af, myinfo));
## Try("1.3.0", FUNC(af, 0, myinfo), "f.mpl", 1,  5, 66);
## Try("1.3.1", FUNC(af, 1, myinfo), "f.mpl", 1, 22, 47);
## Try("1.3.2", FUNC(af, 2, myinfo), "f.mpl", 1, 37, 38);
## Try[TE]("1.3.err", FUNC(af, 8, myinfo), "%1 index out of range");

    Get := proc( addr :: integer, statement :: nonnegint, info := Info )
    local i,datum,rec;

        rec := info[addr];

        if rec = 0 then
            # nothing saved,
            Store(addr);
            return thisproc(_passed);
        elif rec = NULL then
            return NULL;
        end if;

        datum := rec:-positions[statement];

        ( rec:-filenames[datum[1]]
         , seq(datum[i], i = 2..4)  # line, beg, end
        );

    end proc;

##DEFINE PROC Store
##PROCEDURE \THISPROC
##HALFLINE store the lineinfo data of a procedure
##AUTHOR   Joe Riel
##DATE     Apr 2012
##CALLINGSEQUENCE
##- \PROC('addr')
##PARAMETERS
##- 'addr' : ::integer::; Maple address of procedure
##RETURNS
##- `NULL`
##DESCRIPTION
##- Store the *lineinfo* data of a procedure into the module local
##  table `Info`.  If the procedure has no lineinfo data, store `NULL`.
##
##- The 'addr' parameter is the address of the procedure.  It is used
##  as the index into 'Info'.  If an entry already exists in the
##  table, nothing is done.
##
##- See "Info" for a description of the table structure.
##
##TEST
## $include <AssignFunc.mi>
## $include <lineinfo.mpl>
## AssignFUNC(mdc:-LineInfo:-Store):
## macro(NE='testnoerror'
##       ,TA='(verify,table(Or(truefalse,record(Or(truefalse,Array)))))'
##       ,RE='(verify,record(Or(truefalse,Array)))'
##      );
### mdc(FUNC):
##
## Try[NE]("1.0", proc() save f, "f.mpl"; read "f.mpl" end());
## Try[NE]("1.1", addressof(f), 'assign'="af"):
## Try[RE]("1.2", FUNC(af, myinfo)
##         , Record['packed']('filenames' = ["f.mpl"]
##                            , 'positions' = (Array(0..4, 1..4
##                                                   , [NULL
##                                                      ,[1,1,5,66]
##                                                      ,[1,1,22,47]
##                                                      ,[1,1,37,38]
##                                                      ,[1,1,40,42]
##                                                      ,[1,1,50,59]
##                                                     ]
##                                                   , 'datatype' = integer[4]))));
## Try[TA]("1.3", eval(myinfo)
##         , table( [ af = Record['packed']('filenames' = ["f.mpl"]
##                                          , 'positions' =  Array(0..4, 1..4
##                                                                 , [NULL
##                                                                    ,[1,1,5,66]
##                                                                    ,[1,1,22,47]
##                                                                    ,[1,1,37,38]
##                                                                    ,[1,1,40,42]
##                                                                    ,[1,1,50,59]
##                                                                   ]
##                                                                 , 'datatype' = integer[4]))
##                    , "f.mpl" = af
##                  ]
##                ));

    Store := proc( addr :: integer, info := Info )
    local file, filenames, i, lineinfo;

        if assigned(info[addr]) then
            return info[addr];
        end if;

        lineinfo := [debugopts(':-lineinfo' = pointto(addr))];

        if lineinfo = [] then
            # no lineinfo available for prc
            info[addr] := NULL;

        else
            filenames := ListTools:-MakeUnique(map2(op,1,lineinfo));
            # Convert filenames to integers, corresponding to position in 'filenames'
            lineinfo := subs([seq(filenames[i]=i, i=1..numelems(filenames))], lineinfo);
            # Insert filenames and an Array of the statement positions
            # into a two-field record, and store in the sparse table
            # info, which is indexed by the procedure address.
            info[addr] := Record['packed'](':-filenames' = filenames
                                           , ':-positions' = Array(0..numelems(lineinfo)-1, 1..4
                                                                   , lineinfo
                                                                   , 'datatype'=integer[4]
                                                                  )
                                          );

            # Append addr to the entry for each filename.
            for file in filenames do
                if assigned(info[file]) then
                    info[file] := (info[file], addr);
                else
                    info[file] := addr;
                end if;
            end do;

        end if;

        return info[addr];

    end proc;

##DEFINE PROC LookupStatement
##PROCEDURE \THISPROC
##HALFLINE ...
##AUTHOR   Joe Riel
##DATE     May 2012
##CALLINGSEQUENCE
##- \PROC('filename','offset')
##PARAMETERS
##- 'filename' : ::string::; source file
##- 'offset'   : ::nonegint::; character offset from beginning of file
##- 'addr'     : ::integer::; address of procedure
##RETURNS
##- `state` - ::nonnegint::; statement number in procedure
##
##NOTES
##- Because `lineinfo` points to the macro call rather
##  than its expansion, there is a bijection from
##  the lineinfo positions to the source.
##
##- That is not entirely true.  A file can be included
##  into separate procedures, so given just the position
##  info from an include file, we cannot go to the
##  procedure/statement.  However,
##

    LookupStatement := proc( filename :: string
                             , addr :: integer
                             , lineno :: posint
                             , offset :: nonnegint
                             , info := Info
                           )

    local addrs;

        if not assigned(info[filename]) then
            return 0;
        end if;

        addrs := [info[filename]];

        # Can we partition the file into procedures?  Maybe.  Hmm.
        # One danger of using the source as the main buffer is that it
        # tempts the user to set breakpoints in procedures that
        # haven't been visited, hence which aren't in the Info table.
        #
        # Narrowing to the procedure body prevents that, but does not
        # seem right.  Better to raise an appropriate error (maple or
        # emacs?).


        # If offset is outside a procedure body, raise an error.
        # If offset is outside a statement, but on the line of a
        # statement, use that statement, otherwise, if it is
        # in a nesting statement, use that, otherwise


    end proc;

end module:
