# -*- mode:mpldoc -*-

##DEFINE MOD mdb
##MODULE \MOD
##HALFLINE code used by the Emacs Maple debugger
##AUTHOR   Joe Riel
##DATE     Dec 2009

mdb := module()

export PrettyPrint, ArgsToEqs, PrintProc, stopat;
local prettyprint;

    PrettyPrint := proc() prettyprint(true, _passed) end proc:

#{{{ ArgsToEqs

##DEFINE PROC ArgsToEqs
##PROCEDURE \MOD[\PROC]
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
##  from disappearing.
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
##EXAMPLES
##- Assign a macro that mimics the elisp function "mdb-show-args-as-equations".
##> macro(printargs=\MOD:-\PROC(thisproc, [seq([_params[_k]], _k=1.._nparams)],[_rest],[_options])):
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
## AssignFUNC(mdb:-ArgsToEqs):
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
## Try("3.2.1", [FUNC]("proc3_2", [[1],[]], [], []), [y=1, z=NULL]);
##
## Try[NE]("3.3.0", proc(x,y,$) end, 'assign' = "proc3_3");
## Try("3.3.1", [FUNC]("proc3_3", [[[1,2]],[2]], [], []), [x=[1,2], y=2]);
##
## macro(printargs=FUNC(thisproc, [seq([_params[_k]], _k=1.._nparams)],[_rest],[_options])):
## Try[NE]("5.1.0", proc(x:=NULL,y:=[1],$) printargs; end, 'assign' = "proc5_1");
## Try("5.1.1", [proc5_1()], [x=NULL,y=[1]]);

    ArgsToEqs := proc(prc :: {string,procedure}
                      , pargs :: list
                      , rargs :: list
                      , oargs :: list
                     )
    local defparams, opacity, params, p, pos, i, m, n;

        # Assign params the procedure's formal parameters.
        if prc :: procedure then
            params := [op(1,eval(prc))];
        else
            # prc is a string; attempt to parse ...
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

    local ppargs := ( NULL
                      , seq(params[i] = pargs[i][], i=1..n)       # required positional
                      , seq(defparams[i] = pargs[n+i][], i=1..m)  # default positional
                      , oargs[]                                   # optional args
                      , `if`( rargs = []                          # _rest
                              , NULL
                              , ':-_rest' = rargs[]
                            )
                    );

        return prettyprint(false, ppargs);

    end proc;

#}}}
#{{{ prettyprint

##DEFINE PROC prettyprint
##PROCEDURE \MOD[\PROC]
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
## AssignFUNC(mdb:-prettyprint):
## $define T true
## Try("t", FUNC(T));
## Try("exprseq", FUNC(T,a,b,c), a,b,c);
## Try("set", FUNC(T,{a,b}), a,b);
## Try("list", FUNC(T,[a,b]), a,b);
## Try("record", FUNC(T,Record(a=1,b=2)), a=1,b=2);
## Try("table", {FUNC}(T,table([a=1,b=2])), {a=1,b=2});


    prettyprint := proc(top :: truefalse := true)
    local eqs, ex, expr, fld, indx;
        if nargs > 2 then
            seq(procname(false,ex), ex in [_rest]);
        else
            expr := _rest;

            if expr :: set then
                if top then printf("(*set*)\n");
                    return `if`(expr = []
                                , printf("NULL\n")
                                , expr[]
                               );
                else
                    return expr;
                end if;
            elif expr :: list then
                if top then printf("(*list*)\n");
                    return `if`(expr = []
                                , printf("NULL\n")
                                , expr[]
                               );
                else
                    return expr;
                end if;

            elif expr :: 'record' then
                eqs := seq(fld = procname(false, expr[fld]), fld in [exports(expr)]);
                if top then
                    return printf("(*record*)\n"), eqs;
                else
                    return 'record'(eqs);
                end if;

            elif expr :: `module` then
                # type/object won't work for some older maples.
                if attributes(expr)='object' then
                    try
                        if top then
                            printf("(*object*)\n");
                        end if;
                        local opacity := kernelopts('opaquemodules'=false);
                        expr:-ModulePrint;
                        return ModulePrint(expr);
                    catch:
                        printf("object(...)\n");
                    finally
                        kernelopts('opaquemodules'=opacity);
                    end try;
                    return NULL;
                elif top then
                    printf("(*module*)\n");
                    return seq(fld = procname(false, expr[fld]), fld in [exports(expr)]);
                else
                    return `module() ... end module`; # questionable
                end if;

            elif expr :: table then
                eqs := seq(indx = procname(false, expr[indx]), indx in [indices(expr,'nolist')]);
                if top then
                    return printf("(*table*)\n"), eqs;
                else
                    return 'table'(eqs);
                end if;
            elif expr :: procedure then
                if top then
                    showstat(expr);
                    return NULL;
                else
                    `proc() ... end proc`;
                end if;
            elif expr = NULL then
                printf("NULL\n");
                return NULL;
            elif expr :: 'name = anything' then
                return lhs(expr) = procname(false,rhs(expr));
            else
                return expr;
            end if;
        end if;
    end proc:

#}}}
#{{{ PrintProc

# Experimental.

$ifdef SKIP
    PrintProc := proc(nm, prc)
    local listing;
        #listing := debugopts('procdump' = prc);
        #printf("\n%a := %s\n", nm, listing);
        printf("\n%", showstat(nm));
    end proc;
$endif

#}}}

#{{{ stopat

##DEFINE CMD stopat
##PROCEDURE \MOD[\CMD]
##HALFLINE a fast method to instrument a procedure
##AUTHOR   Erik Postma
##DATE     May 2010
##CALLINGSEQUENCE
##- \CMD('p', 'n', 'cond')
##PARAMETERS
##- 'p'    : ::{name,string}::; procedure to instrument
##- 'n'    : (optional) ::posint::; statement number
##- 'cond' : (optional) ::uneval::; condition
##RETURNS
##- `NULL`
##DESCRIPTION
##- The `\CMD` command
##  is a fast version of  ":-stopat", with a few modifications.
##  It sets a breakpoint at a statement number of a procedure.
##
##- The 'p' parameter is the procedure to instrument.
##-- If 'p' is indexed it is converted to a *slashed* name.
##-- If 'p' is a string it is parsed.
##  This provides a means to enter a module local procedure without assigning
##   _kernelopts('opaquemodules'=false)_.
##
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdb:-stopat);
## $define NE testnoerror
##
## Try[NE]("1.0", FUNC("int:-ModuleApply"));
## Try[NE]("2.0", proc() for local i to 5 do i^2 od; end proc, 'assign'="f");
## Try    ("2.1", FUNC(f,1,i>3));
## Try    ("2.2", f());

    stopat := proc(p :: {name,string}
                   , n :: posint
                   , cond :: uneval
                   , $ )
    local
        pn, opacity, pnm;

        try
            opacity := kernelopts('opaquemodules'=false);

            if p :: indexed then
                # Convert indexed to slashed name;
                # e.g. pkg[func] --> `pkg/func`
                pn := indexed2slashed(p);
                if not eval(pn)::procedure
                or (eval(p)::procedure and not has(eval(p),pn)) then
                    pn := p;
                fi;
            elif p :: string then
                pn := parse(p);
            else
                pn := p;
            end if;

            # eval in order to make sure everything is loaded from the
            # library.
            pnm := eval(pn);
            while assigned(pnm[':-ModuleApply']) do
                pnm := eval(pnm:-ModuleApply);
            end do;
        finally
            kernelopts('opaquemodules'=opacity);
        end try;

        if   _npassed = 1 then debugopts('stopat'=[pn,1])
        elif _npassed = 2 then debugopts('stopat'=[pn,n])
        else                   debugopts('stopat'=[pn,n,'cond'])
        end if;

        return NULL;

    end proc:

#}}}
#{{{ indexed2slashed

##DEFINE PROC indexed2slashed
##PROCEDURE \MOD[\PROC]
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
## AssignFUNC(mdb:-indexed2slashed);
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

local
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

end module:

#{{{ Write to Maple Archive

mla := "mdb.mla":
if FileTools:-Exists(mla) then
    fremove(mla);
end if;
LibraryTools:-Save(mdb, mla);

#}}}
