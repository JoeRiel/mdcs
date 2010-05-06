# -*- mode:mpldoc -*-

##DEFINE MOD mdb
##MODULE \MOD
##HALFLINE code used by the Emacs Maple debugger
##AUTHOR   Joe Riel
##DATE     Dec 2009

mdb := module()

export PrettyPrint, ArgsToEqs, PrintProc;
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
##> macro(printargs=\MOD:-\PROC(thisproc, `[]`~([_params[..]]),[_rest],[_options])):
##> f := proc(pos, optpos:=1, { keyword :: truefalse := false }) printargs; end proc:
##> f(3);
##> f(3,4,keyword);
##> f(3,4,5,keyword);
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

    ArgsToEqs := proc(prc :: {string,procedure}
                      , pargs :: list
                      , rargs :: list
                      , oargs :: list
                     )
    local defparams, params, p;

        # Assign params the procedure's formal parameters.
        if prc :: procedure then
            params := [op(1,eval(prc))];
        else
            # prc is a string; attempt to parse ...
            try
                local opacity := kernelopts('opaquemodules'=false);
                params := [op(1,eval(parse(prc)))];
            finally
                kernelopts('opaquemodules'=opacity);
            end try;
        end if;

        # If $ was used in the procedure assignment, then all optional parameters
        # can be easily removed.  Otherwise remove them later.
    local pos;
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

    local i, m, n;
        m := nops(defparams);
        n := nops(pargs) - m;

        return ( seq(params[i] = pargs[i][], i=1..n)         # required positional
                 , seq(defparams[i] = pargs[n+i][], i=1..m)  # default positional
                 , oargs[]                                   # optional args
                 , `if`( rargs = []                          # _rest
                         , NULL
                         , ':-_rest' = rargs[]
                       )
               );

    end proc;

#}}}
#{{{ prettprint

##DEFINE PROC prettyprint
##PROCEDURE \MOD[\PROC]
##HALFLINE
##AUTHOR   Joe Riel
##DATE     Dec 2009
##CALLINGSEQUENCE
##- \PROC('top', ...)
##PARAMETERS
##- 'top' : ::truefalse::
##RETURNS
##- ::exprseq::
##DESCRIPTION
##- The `\PROC` procedure
##
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdb:-prettyprint):
## $define T true
## Try("t", FUNC(T));
## Try("set", FUNC(T,{a,b}), a,b);
## Try("list", FUNC(T,[a,b]), a,b);
## Try("record", FUNC(T,Record(a=23)), a=23);
## Try("table", FUNC(T,table([a=23])), a=23);

    prettyprint := proc(top :: truefalse := true)
    local eqs, ex, expr, fld, indx;
        if nargs > 2 then
            seq(procname(false,ex), ex in [_rest]);
        else
            expr := _rest;

            if expr :: 'Or(set,list)' then
                return expr[];
            elif expr :: 'record' then
                fld;
                eqs := seq(fld = procname(false, expr[fld]), fld in [exports(expr)]);
                if top then
                    return eqs;
                else
                    return 'record'(eqs);
                end if;
            elif expr :: table then
                eqs := seq(indx = procname(false, expr[indx]), indx in [indices(expr,'nolist')]);
                if top then
                    return eqs;
                else
                    return 'table'(eqs);
                end if;
            elif expr :: procedure then
                showstat(expr);
                return NULL;
            else
                return expr;
            end if;
        end if;
    end proc:

#}}}
#{{{ PrintProc

# Experimental.

    PrintProc := proc(nm, prc)
    local listing;
        #listing := debugopts('procdump' = prc);
        #printf("\n%a := %s\n", nm, listing);
        printf("\n%", showstat(nm));
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
