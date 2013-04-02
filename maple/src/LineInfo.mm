##INCLUDE ../include/mpldoc_macros.mpi

##MODULE mdc[LineInfo]
##HALFLINE methods for lineinfo
##AUTHOR   Joe Riel
##DATE     Apr 2012
##DESCRIPTION(notest)
##- Maple 16 partially implements a **lineinfo** feature that,
##  when a Maple source file is read (with "read"),
##  records the filename and statement positions
##  of each procedure in the file.  The information
##  can be retrieved with ~debugopts('lineinfo')~.
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
### mdc(mdc:-LineInfo:-Store, mdc:-LineInfo:-Get);
##
## Try[NE]("1.0", proc() read mdc:-DataFile("Sample.mpl"); end());
## Try[NE]("1.1", addressof(MyModule:-A), 'assign'="addr");
## Try[NE]("1.2", mdc:-LineInfo:-Store(addr));
## Try("1.3.0", mdc:-LineInfo:-Get(addr,0), "/home/joe/maple/toolbox/emacs/data/Sample.mpl",  7, 110, 364);
## Try("1.3.1", mdc:-LineInfo:-Get(addr,1), "/home/joe/maple/toolbox/emacs/data/Sample.mpl", 10, 172, 178);

LineInfo := module()

export Breakpoints
    ,  Get
    ,  LookupStatement
    ,  Store
    ;

local ModuleLoad

##TOPIC Info
##HALFLINE table storing lineinfo information
##AUTHOR   Joe Riel
##DATE     May 2012
##DESCRIPTION
##- The 'Info' table, which is created by calls to "Store",
##  stores accumulated lineinfo information.
##  Its indices consist of
##    filenames of sources
##  and
##    addresses of procedures.
##
##- A *filename* entry associates a source filename with the addresses
##  of the procedures it contains, either wholly or partially.
##
##-- An entry consists of an expression sequence of addresses.
##
##- An *address* entry associates the address of a procedure
##  with its source information.
##  The entry consists of a packed record with two fields,
##  'filenames' and 'positions':
##
##-- 'filenames' : list of strings corresponding to the filenames
##  in which the procedure is defined.
##
##-- 'positions' : four column Array, indexed from 0 to `n`,
##  where `n` is the maximum statement number of the procedure.
##  Each row holds the data for one statement.
##  The 0 row holds the data for the entire procedure.
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

    , Info
    ;

##PROCEDURE mdc[LineInfo][ModuleLoad]
##HALFLINE assign the module-local varible Info an empty table.
##AUTHOR   Joe Riel
##DATE     May 2012

    ModuleLoad := proc()
        # Initialize the info table
        Info := table('sparse');
        NULL;
    end proc;

##PROCEDURE mdc[LineInfo][Get]
##HALFLINE return the source location of a procedure statement
##AUTHOR   Joe Riel
##DATE     Apr 2012
##CALLINGSEQUENCE
##- Get('addr','statement')
##PARAMETERS
##- 'addr'      : ::integer::; Maple address of procedure
##- 'statement' : ::posint::; statement number
##RETURNS
##- `(filename, lineno, charbeg, charend)`
##-- `filename` : ::string or 0::; source file name
##-- `lineno`   : ::posint::; line number
##-- `charbeg`  : ::posint::; file character position of beginning of statement
##-- `charend`  : ::posint::; file character position of end of statement
##DESCRIPTION
##- Return the source filename and source position data
##  associated with a 'statement' number of a procedure,
##  given its address, 'addr'.
##OPTIONS
##opt(ret_begin,truefalse)
##  True means return just the character offset of the beginning of
##  the statement.
##  The default is false.
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
## Try[NE]("1.0.1", proc() save f, "f.mpl"; read "f.mpl" end());
## Try[NE]("1.0.2", addressof(f), 'assign'="af"):
## Try[NE]("1.0.4", Store(af, myinfo));
## Try[NE]("1.0.5", cat("/home/joe/tmp/mpldoc/f.mpl"), 'assign' = "f_mpl");
## Try("1.1.0", FUNC(af, 0, myinfo), f_mpl, 1,  5, 66);
## Try("1.1.1", FUNC(af, 1, myinfo), f_mpl, 1, 22, 47);
## Try("1.1.2", FUNC(af, 2, myinfo), f_mpl, 1, 37, 38);
## Try[TE]("1.1.err", FUNC(af, 8, myinfo), "%1 index out of range");

    Get := proc( addr :: integer
                 , statement :: nonnegint
                 , { ret_begin :: truefalse := false }
               )
    local i,datum,rec;

        rec := Info[addr];

        if rec = 0 then
            # nothing saved,
            Store(addr);
            return procname(_passed);
        elif rec = NULL then
            return NULL;
        end if;

        datum := rec:-positions[statement];

        if ret_begin then
            datum[3];
        else
            ( rec:-filenames[datum[1]]
              , seq(datum[i], i = 2..4)  # line, beg, end
            );
        end if;

    end proc;

##PROCEDURE mdc[LineInfo][Store]
##HALFLINE store the lineinfo data of a procedure
##AUTHOR   Joe Riel
##DATE     Apr 2012
##CALLINGSEQUENCE
##- Store('addr')
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
## debug(FUNC):
##
## Try[NE]("1.0", proc() save f, "f.mpl"; read "f.mpl" end());
## Try[NE]("1.1", addressof(f), 'assign'="af"):
## Try[NE]("1.2", table('sparse'), 'assign'="myinfo");
## Try[NE]("1.4", sprintf("%s/f.mpl", currentdir()), 'assign' = "f_mpl");
## Try[RE]("1.5", FUNC(af, myinfo)
##         , Record['packed']('filenames' = [f_mpl]
##                            , 'positions' = (Array(0..4, 1..4
##                                                   , [NULL
##                                                      ,[1,1,5,66]
##                                                      ,[1,1,22,47]
##                                                      ,[1,1,37,38]
##                                                      ,[1,1,40,42]
##                                                      ,[1,1,50,59]
##                                                     ]
##                                                   , 'datatype' = integer[4]))));
## Try[TA]("1.6", eval(myinfo)
##         , table('sparse'
##                 , [ af = Record['packed']('filenames' = [f_mpl]
##                                           , 'positions' =  Array(0..4, 1..4
##                                                                  , [NULL
##                                                                     ,[1,1,5,66]
##                                                                     ,[1,1,22,47]
##                                                                     ,[1,1,37,38]
##                                                                     ,[1,1,40,42]
##                                                                     ,[1,1,50,59]
##                                                                    ]
##                                                                  , 'datatype' = integer[4]))
##                            , f_mpl = af
##                   ]
##                ));

    Store := proc( addr :: integer, info := Info )
    local file, filenames, i, lineinfo, missing;

        try
            lineinfo := [debugopts(':-lineinfo' = pointto(addr))];
        catch:
            lineinfo := [];
        end try;

        if lineinfo = [] then
            # no lineinfo available for prc
            info[addr] := NULL;

        else
            filenames := ListTools:-MakeUnique(map2(op,1,lineinfo));

            if filenames = [""] then
                # The empty string indicates the source is a Maple worksheet.
                info[addr] := NULL;
            else
                missing := remove(FileTools:-Exists or Testzero, filenames);
                if missing <> [] then
                    WARNING("cannot find source files: %1", missing);
                    info[addr] := NULL;
                else
                    # Convert filenames to integers, corresponding to position in 'filenames'
                    lineinfo := subs([seq(filenames[i]=i, i=1..numelems(filenames))], lineinfo);
                    # Insert filenames and an Array of the statement positions
                    # into a two-field record, and store in the sparse table
                    # info, which is indexed by the procedure address.

                    filenames := map(nm -> `if`(nm=0
                                                , 0
                                                , FileTools:-AbsolutePath(nm)
                                               )
                                     , filenames);

                    info[addr] := Record['packed'](':-filenames' = filenames
                                                   , ':-positions' = Array(0..numelems(lineinfo)-1, 1..4
                                                                           , lineinfo
                                                                           , 'datatype' = integer[4]
                                                                          )
                                                  );

                    # Append addr to the entry for each filename.
                    for file in filenames do
                        if info[file] = 0 then
                            info[file] := addr;
                        else
                            info[file] := (info[file], addr);
                        end if;
                    end do;
                end if;

            end if;
        end if;

        return info[addr];

    end proc;

##PROCEDURE mdc[LineInfo][LookupStatement]
##HALFLINE return statement information at a given source position
##AUTHOR   Joe Riel
##DATE     May 2012
##CALLINGSEQUENCE
##- LookupStatemen('filename','lastaddr','lineno','offset')
##PARAMETERS
##- 'filename' : ::string::; source file name
##- 'lastaddr' : ::integer::; address of likely procedure
##- 'lineno'   : ::posint::; line number of position in source file
##- 'offset'   : ::nonnegint::; character offset (from 0) of position in file
##RETURNS
##- `(addr,state,offset)`
##-- `addr` - ::integer::; procedure address
##-- `state` - ::nonnegint::; statement number in procedure
##-- `offset` - ::nonnegint::; character offset of beginning of statement
##- ~"no candidates"~ - returned if there are no known source candidates
##  at position.
##ALGORITHM
##- Given 'filename',
##  get the list of possible procedures (addresses).
##
##- Determine the appropriate address.
##-- From the position information, determine which procedures
##  are candidates.
##-- If more than one procedure is a candidate, use 'lastaddr',
##  provided it matches one of them.  Otherwise exit unsuccessfully.
##  Multiple matches can occur with an *include* file.
##
##- With address determined, find the statement.
##
##NOTES
##- Because `lineinfo` points to the macro call rather
##  than its expansion, there is a bijection from
##  the lineinfo positions to the source.
##
##- That is not entirely true.  A file can be included
##  into separate procedures, so given just the position
##  info from an include file, we cannot go to the
##  procedure/statement.
##
##TEST
## $include <AssignFunc.mi>
## $include "/home/joe/emacs/mdcs/maple/include/lineinfo.mpl"
## AssignFUNC(mdc:-LineInfo:-LookupStatement):
## AssignLocal(Store,mdc:-LineInfo:-Store):
## macro(NE='testnoerror'
##       ,TA='(verify,table(Or(truefalse,record(Or(truefalse,Array)))))'
##       ,RE='(verify,record(Or(truefalse,Array)))'
##      );
### mdc(FUNC):
## Try[NE]("1.0.1", "\"/home/joe/emacs/mdcs/maple/include/lineinfo.mpl\"", 'assign' = "src"):
## Try[NE]("1.0.2", proc() read src; end proc()):
## Try[NE]("1.0.3", addressof(f), 'assign'="af"):
## Try[NE]("1.0.4", mdc:-LineInfo:-Store(af));
##
## Try("1.1.1", [FUNC](src, af, 5, 51), [af,3,59]);
## Try("1.1.2", [FUNC](src, af, 5, 10), [af,3,59]);
## Try("1.1.3", [FUNC](src, af, 6, 10), [af,4,78]);
## Try("1.1.4", [FUNC](src, af, 7, 10), [af,4,78]);

    LookupStatement := proc( filename :: string
                             , lastaddr :: integer
                             , lineno :: posint
                             , offset :: nonnegint
                           )

    local addr, addrs, i,n,pos;

        if Info[filename] = 0 then
            error "no lineinfo data exists for file '%1'", filename;
        end if;

        # Find appropriate addr by selecting those procedures
        # whose character range contains 'offset'.  If more than one,
        # then use the 'lastaddr'.
        addrs := [Info[filename]];
        addrs := select( addr -> ( Info[addr]:-positions[0,3] <= offset
                                   and
                                   offset <= Info[addr]:-positions[0,4]
                                 )
                         , addrs );
        if addrs = [] then
            error "no candidates";
        elif 1 < numelems(addrs) then
            if member(lastaddr, addrs) then
                addr := lastaddr;
            else
                error "no candidates";
            end if;
        else
            addr := addrs[1];
        end if;

        # Search for line position.
        # Assign 'pos' the Array of position data for 'addr'
        pos := Info[addr]:-positions;

        # Get first statement on or following 'lineno'
        n := upperbound(pos,1);
        for i to n while pos[i,2] < lineno do end do;
        ASSERT(i<=n, "cannot find statement");
        return (addr,i,pos[i,3]);

    end proc;

##PROCEDURE mdc[LineInfo][Breakpoints]
##HALFLINE return positions of breakpoints in a procedure
##AUTHOR   Joe Riel
##DATE     Jun 2012
##CALLINGSEQUENCE
##- Breakpoints('addr')
##PARAMETERS
##- 'addr' : ::integer::; address of procedure
##RETURNS
##- ::string::
##DESCRIPTION
##- Returns a string consisting of the character offsets
##  of the beginning of statements with breakpoints
##  in a procedure.
##
##- The parameter 'addr' is address of the procedure.
##  The procedure is assumed to have an entry
##  in the *Info* table.
##
##- If there are no breakpoints, return the empty string.
##  Otherwise return a string consisting of the offsets,
##  each preceded by a space.  For example, if the
##  procedure has breakpoints at offsets 12 and 24, the
##  string returned is ~" 12 24"~.
##TEST
## $include <AssignFunc.mi>
## $include <lineinfo.mpl>
## AssignFUNC(mdc:-LineInfo:-Breakpoints):
## AssignLocal(Store, mdc:-LineInfo:-Store):
## macro(NE='testnoerror'
##       ,TA='(verify,table(Or(truefalse,record(Or(truefalse,Array)))))'
##       ,RE='(verify,record(Or(truefalse,Array)))'
##       ,TE='testerror'
##      );
### mdc(FUNC):
##
## Try[NE]("1.0.1", proc() save f, "f.mpl"; read "f.mpl" end());
## Try[NE]("1.0.2", addressof(f), 'assign'="af"):
## Try[NE]("1.0.4", mdc:-LineInfo:-Store(af));
##
## Try("1.1.0", FUNC(af), "");
## Try[NE]("1.1.0", stopat(f));
## Try("1.1.1", FUNC(af), " 22");
## Try[NE]("1.2.0", stopat(f,3)):
## Try("1.2.1", FUNC(af), " 22 40");

    Breakpoints := proc( addr :: integer)
    local all,begs,prc,result,state,str;
        prc := pointto(addr);
        # Quick test to handle procedures with no breakpoints.
        # The eval is problematic; some procedures have issues.  The
        # symptom is delayed.  During debugging, the simulation at
        # some point runs to completion.
        # if not has(eval(prc,2),'DEBUG') then
        #    return "";
        # end if;
        str := debugopts('procdump' = prc);
        begs := Info[addr]:-positions;
        result := "";
        while StringTools:-RegMatch("\n *([0-9]+)\\*(.*)", str, 'all', 'state', 'str') do
            state := sscanf(state,"%d")[1];
            result := sprintf("%s %d", result, begs[state,3]);
        end do;
        result;
    end proc;

##

end module:

