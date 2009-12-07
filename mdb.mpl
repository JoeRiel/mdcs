# -*- mode:mpldoc -*-

##DEFINE MOD mdb
##MODULE \MOD
##HALFLINE code used by the Emacs Maple debugger
##AUTHOR   Joe Riel
##DATE     Dec 2009

mdb := module()

export PrettyPrint, ArgsToEqs;
local prettyprint;

    PrettyPrint := proc() prettyprint(true, _passed) end proc:

#{{{ prettprint

    prettyprint := proc(top :: truefalse := true)
    local eqs, ex, expr;
        if nargs > 2 then
            seq(procname(false,ex), ex in [_rest]);
        else
            expr := _rest;

            if expr :: 'Or(set,list)' then
                expr[];
            elif expr :: 'record' then
                local fld;
                eqs := seq(fld = procname(false, expr[fld]), fld in [exports(expr)]);
                if top then
                    eqs;
                else 'record'(eqs)
                end if;
            elif expr :: table then
                local indx;
                eqs := seq(indx = procname(false, expr[indx]), indx in [indices(expr,'nolist')]);
                if top then
                    eqs
                else
                    'table'(eqs);
                end if;
            elif expr :: procedure then
                showstat(expr);
                return NULL;
            else
                expr
            end if;
        end if;
    end proc:

#}}}
#{{{ ArgsToEqs

##DEFINE PROC ArgsToEqs
##PROCEDURE \MOD[\PROC]
##HALFLINE create equations defining the parameters of a procedure call
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
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdb:-ArgsToEqs):
## $define NE testnoerror
##
## Try[NE]("1.1.0", proc(x,y) end, 'assign' = "proc1_1");
## Try("1.1.1", FUNC("proc1_1", [[1],[2]],[],[]), x=1, y=2);
##
## Try[NE]("1.2.0", proc() end, 'assign' = "proc1_2");
## Try("1.2.1", FUNC("proc1_2", [],[1,2],[]), _rest = (1,2));
##
## Try[NE]("2.1.0", proc(x, {b::truefalse:=false}, $) end, 'assign' = "proc2_1");
## Try("2.1.1", FUNC("proc2_1", [[1]], [], [b=false]), x=1, b=false);
## Try("2.2.2", FUNC("proc2_1", [[1]], [], [b=true]), x=1,b=true);
##
## Try[NE]("3.1.0", proc(x,y:=1,z::integer:=2) end, 'assign' = "proc3_1");
## Try("3.1.1", FUNC("proc3_1", [[1],[2]], [], []), y=1, z=2);
##
## Try[NE]("3.2.0", proc(x,y:=1,z::integer:=NULL) end, 'assign' = "proc3_2");
## Try("3.2.1", FUNC("proc3_2", [[1],[]], [], []), y=1, z=NULL);
##
## Try[NE]("3.3.0", proc(x,y,$) end, 'assign' = "proc3_3");
## Try("3.3.1", FUNC("proc3_3", [[[1,2]],[2]], [], []), x=[1,2], y=2);

    ArgsToEqs := proc(prc :: {string,procedure}
                      , pargs :: list
                      , rargs :: list
                      , oargs :: list
                     )
    local defparams, params, p;

        # Assign params the procedure's formal parameters.
        if prc :: procedure then
            params := op(1,prc);
        else
            # prc is a string; attempt to parse ...
            try
                local opacity := kernelopts('opaquemodules'=false);
                params := [op(1,eval(parse(prc)))];
            finally
                kernelopts('opaquemodules'=opacity);
            end try;
        end if;

        # Eliminate the $ parameter and everything beyond it.
    local pos;
        if member(:-` $`, params, 'pos') then
            params := params[1..pos-1];
        end if;

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

end module:

mla := "mdb.mla":
fremove(mla);
LibraryTools:-Save(mdb, mla);
