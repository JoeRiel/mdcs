##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE SUBMOD Format
##MODULE \MOD[\SUBMOD]
##HALFLINE code used by the Emacs Maple debugger
##AUTHOR   Joe Riel
##DATE     Dec 2009

Format := module()

export ArgsToEqs
    ,  GoTry
    ,  PrettyPrint
    ,  showstat
    ,  Try
    ;

local indexed2slashed
    , prettyprint
    , T
    ;

    PrettyPrint := proc() prettyprint(true, _passed) end proc:

#{{{ ArgsToEqs

##DEFINE PROC ArgsToEqs
##PROCEDURE \MOD[\SUBMOD][\PROC]
##HALFLINE return equations defining the parameters of a procedure call
##AUTHOR   Joe Riel
##DATE     Dec 2009
##CALLINGSEQUENCE
##- ArgsToEqs('prc', 'pargs', 'rargs', 'oargs')
##PARAMETERS
##- 'prc'   : ::procedure:: or ::string::
##- 'pargs' : ::list(list)::; declared positional arguments, each enclosed in a list
##- 'rargs' : ::list::; undeclared arguments (\_rest)
##- 'oargs' : ::list::; optional arguments (\_options)
##RETURNS
##- ::exprseq(equation)::; equations mapping parameters to values
##DESCRIPTION
##- The `\PROC` procedure
##  returns an expression sequence
##  of equations mapping the parameters of a procedure
##  to their assigned values.
##
##- The 'prc' parameter is either a procedure, ("thisproc"), or a string
##  that parses to a procedure.
##
##- The 'pargs' parameter is the list of the declared positional arguments passed to 'prc'.
##  Each argument is enclosed in a sublist, that is needed to prevent a `NULL`
##  from disappearing.  This argument is typically generated by
##  ~[seq([_params[k]], k=1.._nparams)]~ inside a call to the procedure.
##
##- The 'rargs' parameter is a list of the undeclared arguments.
##  It equals ~[_rest]~ evaluated insided a call to the procedure.
##
##- The 'oargs' parameter is a list of the optional arguments.
##  It equals ~[_options]~ evaluated insided a call to the procedure.
##
##NOTES
##- The reason that 'prc' can be passed as string is multifold:
##  (1) "procname" does not work with overloaded procedures;
##  (2) "thisproc" is not available in Maple 13; and
##  (3) the actual name may not work unless ~kernelopts(opaquemodules=false)~.
##
##- A problem with passing a string is that parsing it does not necessarily
##  return the same procedure.
##
##EXAMPLES(notest)
##- Assign a macro that mimics the elisp function "mdb-show-args-as-equations".
##> macro(printargs=\MOD:-\SUBMOD:-\PROC(thisproc, [seq([_params[_k]], _k=1.._nparams)],[_rest],[_options])):
##> f := proc(pos, optpos:=1, { keyword :: truefalse := false }) printargs; end proc:
##> f(3);
##> f(3,4,keyword);
##> f(3,4,5,keyword);
##- Ensure that it works with `NULL` default values.
##> g := proc(y:=NULL, z:=1, { key := NULL} ) printargs; end proc:
##> g();
##
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdc:-Format:-ArgsToEqs):
## $define NE testnoerror
##
## Try[NE]("1.1.0", proc(x,y) end, 'assign' = "proc1_1");
## Try("1.1.1", [FUNC]("proc1_1", [[1],[2]],[],[]), [x=1, y=2]);
##
## Try[NE]("1.2.0", proc() end, 'assign' = "proc1_2");
## Try("1.2.1", [FUNC]("proc1_2", [],[1,2],[]), [_rest = (1,2)]);
##
## Try[NE]("2.1.0", proc(x, {b::truefalse:=false}, $) end, 'assign' = "proc2_1");
## Try("2.1.1", [FUNC]("proc2_1", [[1]], [], [b=false]), [x=1, b=false]);
## Try("2.2.2", [FUNC]("proc2_1", [[1]], [], [b=true]),  [x=1, b=true ]);
##
## Try[NE]("3.1.0", proc(x,y:=1,z::integer:=2) end, 'assign' = "proc3_1");
## Try("3.1.1", [FUNC]("proc3_1", [[1],[2]], [], []), [y=1, z=2] );
##
## Try[NE]("3.2.0", proc(x,y:=1,z::integer:=NULL) end, 'assign' = "proc3_2");
## Try("3.2.1", [FUNC]("proc3_2", [[1],[]], [], []), [y=1, z="NULL"]);
##
## Try[NE]("3.3.0", proc(x,y,$) end, 'assign' = "proc3_3");
## Try("3.3.1", [FUNC]("proc3_3", [[[1,2]],[2]], [], []), [x=[1,2], y=2]);
##
## macro(printargs=FUNC(thisproc, [seq([_params[_k]], _k=1.._nparams)],[_rest],[_options])):
## Try[NE]("5.1.0", proc(x:=NULL,y:=[1],$) printargs; end, 'assign' = "proc5_1");
## Try("5.1.1", [proc5_1()], [x="NULL",y=[1]]);
##

    ArgsToEqs := proc(prc :: {string,procedure,integer}
                      , pargs :: list
                      , rargs :: list
                      , oargs :: list
                     )
    local defparams, opacity, params, p, pos, i, m, n, ppargs;

        # Assign params the procedure's formal parameters.
        if prc :: integer then
            params := [op(1,eval(pointto(prc)))];
        elif prc :: procedure then
            params := [op(1,eval(prc))];
        else (* prc is a string, attempt to parse *)
            try
                opacity := kernelopts('opaquemodules'=false);
                params := [op(1,eval(parse(prc)))];
            finally
                kernelopts('opaquemodules'=opacity);
            end try;
        end if;

        # If $ was used in the procedure assignment, then all optional parameters
        # can be easily removed.  Otherwise remove them later.
        if member(:-` $`, params, 'pos') then
            params := params[1..pos-1];
        end if;

        # Remove default parameters from parameters.
        (defparams, params) := selectremove(type, params, 'assignment');

        # Remove type declarations from params and defparams.
        params := [seq(`if`(p :: symbol
                            , p
                            , op(1,p)
                           )
                       , p in params)];

        defparams := [seq(`if`(op(1,p) :: symbol
                               , op(1,p)
                               , op([1,1],p)
                              )
                          , p in defparams)];

        # Remove remaining options from the default positional parameters.
        defparams := remove(member, defparams, lhs~(oargs));

        m := nops(defparams);
        n := nops(pargs) - m;

        ppargs := ( NULL
                    , seq(params[i] = pargs[i][], i=1..n)       # required positional
                    , seq(defparams[i] = pargs[n+i][], i=1..m)  # default positional
                    , oargs[]                                   # optional args
                    , `if`( rargs = []                          # _rest
                            , NULL
                            , ':-_rest' = rargs[]
                          )
                  );

        # return ppargs;
        return prettyprint(false, ppargs);

    end proc;

#}}}
#{{{ prettyprint

##DEFINE PROC prettyprint
##PROCEDURE \MOD[\SUBMOD][\PROC]
##HALFLINE pretty print a Maple expression
##AUTHOR   Joe Riel
##DATE     Dec 2009
##CALLINGSEQUENCE
##- \PROC('top', ... )
##PARAMETERS
##- 'top' : ::truefalse::
##RETURNS
##- ::exprseq::
##DESCRIPTION
##- The `\PROC` procedure
##  returns an expression sequence that better displays
##  a Maple expression in the debugger.   It works
##  by splitting an expression into an expression
##  sequence, which are then displayed on
##  separate lines in the debugger output buffer.
##
##- The optional 'top' parameter is 'true' only
##  if this `\PROC` was called directly by Emacs.
##
##- A sequence of expressions are handled separately, in order.
##
##- A ::record:: or ::table::
##  is returned as an expression sequence of equations,
##  the left side the field/index,
##  the right side the entry.
##
##- A ::set:: or ::list::
##  is returned as an expression sequence of its members.
##
##- A ::procedure:: is displayed with "showstat".
##
##- Any other expression type is returned as-is.
##
##
##NOTES
##- Need to provide a means to indicate the type.
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdc:-Format:-prettyprint):
## kernelopts(opaquemodules=false):
## mdc:-Debugger:-Printf := printf:
## mdc:-Format:-showstat := (x->x):
## kernelopts(opaquemodules=true):
##
## $define T true
## Try("t", FUNC(T), "NULL");
## Try("exprseq", FUNC(T,a,b,c), a,b,c);
## Try("set", FUNC(T,{a,b}), a,b);
## Try("list", FUNC(T,[a,b]), a,b);
## Try("record", FUNC(T,Record(a=1,b=2)), a=1,b=2);
## Try("table", {FUNC}(T,table([a=1,b=2])), {a=1,b=2});


    prettyprint := proc(top :: truefalse := true)
    local eqs, ex, fld, i, ix, n, typ, opacity, rest;
    global _fake_name;

        if nargs > 2 then
            return seq(procname(false,ex), ex in [_rest]);
        end if;

        # Assign _rest; using it directly does not always work.
        rest := _rest;

        if rest :: set then
            if top then
                Debugger:-Printf("(*set: %d*)\n", nops(rest));
                return `if`(rest = []
                            , Debugger:-Printf("NULL\n")
                            , rest[]
                           );
            else
                return rest;
            end if;
        elif rest :: list then
            if top then
                Debugger:-Printf("(*list: %d*)\n", nops(rest));
                return `if`(rest = []
                            , Debugger:-Printf("NULL\n")
                            , rest[]
                           );
            else
                return rest;
            end if;

        elif rest :: 'record' then
            eqs := seq(`if`(assigned(rest[fld])
                            , fld = procname(false, rest[fld])
                            , fld
                           )
                       , fld in [exports(rest)]);
            if top then
                return Debugger:-Printf("(*record*)\n"), eqs;
            else
                return 'record'(eqs);
            end if;

        elif rest :: `module` then
            # type/object won't work for some older maples.
            if attributes(rest)='object' then
                try
                    if top then
                        Debugger:-Printf("(*object*)\n");
                    end if;
                    opacity := kernelopts('opaquemodules'=false);
                    rest:-ModulePrint;
                    return ModulePrint(rest);
                catch:
                    Debugger:-Printf("object(...)\n");
                finally
                    kernelopts('opaquemodules'=opacity);
                end try;
                return NULL;
            elif top then
                Debugger:-Printf("(*module*)\n");
                return seq(fld = procname(false, rest[fld]), fld in [exports(rest)]);
            else
                return `module() ... end module`; # questionable
            end if;

        elif rest :: table then
            typ := op(0,eval(rest));
            eqs := seq(ix[] = procname(false, rest[ix[]]), ix in [indices(rest)]);
            if top then
                return (Debugger:-Printf("(*%a*)\n", typ), eqs);
            else
                return (typ -> 'typ'(eqs))(typ);
            end if;

        elif rest :: procedure then
            if top then
                if _rest :: name then
                    Format:-showstat(convert(_rest,string));
                else
                    # rest is an evaluated expression.  Assign to
                    # a the global name _fake_name, which is then
                    # displayed.  This is done because
                    # debugopts(procdump) requires a name.
                    _fake_name := rest;
                    Format:-showstat("_fake_name");
                end if;
                return NULL;
            else
                # this can be improved.
                return `proc() ... end proc`;
            end if;
        elif rest :: string then
            if StringTools:-Has(rest, "\n") then
                Debugger:-Printf("(*String with newlines [chars: %d]: *)\n", length(rest));
                Debugger:-Printf("%s\n", rest);
                return NULL;
            else
                return rest;
            end if;

        elif rest = NULL then
            return "NULL";
        elif rest :: 'name = anything' then
            return lhs(rest) = procname(false,rhs(rest));
        elif rest :: Vector then
            if top then
                n := op(1,rest);
                Debugger:-Printf("(*Vector: %d*)\n", n);
                return seq(rest[i], i=1..n);
            else
                return rest;
            end if;
        else
            return rest;
        end if;
    end proc:

#}}}
#{{{ showstat

##DEFINE CMD showstat
##PROCEDURE \MOD[\SUBMOD][\CMD]
##HALFLINE display a procedure with statement numbers for debugging
##AUTHOR   Joe Riel
##DATE     May 2010
##CALLINGSEQUENCE
##- \CMD('p')
##PARAMETERS
##- 'p' : ::string::; string corresponding to procedure name
##RETURNS
##- `NULL`
##DESCRIPTION
##- The `\CMD` command
##  formats a procedure with statement numbers
##  and sends the string to the server.
##
##- It is essentially equivalent to "showstat",
##  but (currently) only takes one argument and
##  rather than being a procedure is a string that
##  parses to a procedure (name).
##
##- The purpose of this is to allow passing
##  names of procedures that require the
##  use of ~kernelopts(opaquemodules=false)~
##  to access.

    showstat := proc(p :: string)
    local opacity,prc;
    global _prc;
        try
            opacity := kernelopts('opaquemodules' = false);
            prc := parse(p);
            try
                eval(prc);
            catch:
            end try;
            prc := sprintf("%A", debugopts('procdump' = prc));
            WriteTagf(TAG_SS_DEAD, "%s", prc);
        finally
            kernelopts('opaquemodules' = opacity);
        end try;
        return NULL;
    end proc;

#}}}
#{{{ indexed2slashed

##DEFINE PROC indexed2slashed
##PROCEDURE \MOD[\SUBMOD][\PROC]
##HALFLINE convert indexed name to a slashed name
##AUTHOR   Joe Riel
##DATE     May 2010
##CALLINGSEQUENCE
##- \PROC('nm')
##PARAMETERS
##- 'nm' : ::name::; name to convert
##RETURNS
##- ::symbol::
##DESCRIPTION
##- The `\PROC` procedure
##  converts an indexed name to a slashed name.
##  For example, ~base[index]~ is converted to
##  ~`base/index`~.
##
##- If a name is not indexed it is returned as is.
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdc:-Format:-indexed2slashed);
## $define NE testnoerror
## #stopat(FUNC):
## Try("1.0", FUNC(a), a);
## Try("1.1", FUNC(a[1]), `a/1`);
## Try("1.2", FUNC(a[1][2]), `a/1/2`);
##
## Try("3.1", FUNC(`a+b`[1][2]), `a+b/1/2`);
##
## $define TE testerror
## err := "cannot convert":
## Try[TE]("10.1", FUNC(a[]), err);
## Try[TE]("10.2", FUNC(a[1,2]), err);

    indexed2slashed := proc(nm :: name, $)
        if nm :: indexed then
            if nops(nm) <> 1 then
                error "cannot convert";
            end if;
            return nprintf("%A/%A", procname(op(0,nm)), op(1,nm));
        else
            return nm;
        end if;
    end proc;

#}}}

#{{{ GoTry

##DEFINE PROC GoTry
##PROCEDURE \MOD[\SUBMOD][\PROC]
##HALFLINE reassign Try to save tests for execution with the go command
##AUTHOR   Joe Riel
##DATE     May 2011
##CALLINGSEQUENCE
##- \PROC('dbg')
##PARAMETERS
##PARAMETERS
##- 'dbg' : (optional) keyword parameter 'debug'
##RETURNS
##- `NULL`
##DESCRIPTION
##- The `\PROC` procedure
##  is intended to be used with the "TestTools" package
##  in the idiosyncratic manner of the author.
##
##- It redefines the "Try" export so that the tests
##  are saved in a table and execute with the
##  "go" procedure.  The argument to `go` is the test identifier.
##  If only one test is defined, then calling `go` with no
##  arguments runs that test.
##
##- If the optional keyword 'debug', or equation 'debug=true',
##  is passed to `\PROC`, then the global procedure named
##  `FUNC` is debugged.

    T := table();

    GoTry := proc( { debug :: truefalse := false }, $ )
    global go, Try;
        unprotect('Try');
        stoperror('all');
        if debug then
            stopat('FUNC');
        end if;

        go := proc( id := NULL
                    , { list :: truefalse := false }
                    , $
                  )
            if list then
                return indices(op(4,op(procname)),'nolist');
            elif id = NULL then
                if numelems(T) = 1 then
                    return eval(entries(T,'nolist'));
                end if;
            elif assigned(T[id]) then
                return eval(T[id]);
            end if;
        end proc;

        Try := proc(id, expr::uneval)
            T[id] := 'expr';
            return NULL;
        end proc;
        return NULL;
    end proc;

#}}}

end module:

