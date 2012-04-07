##DEFINE CMD Monitor
##PROCEDURE(help) \PKG[\CMD]
##HALFLINE set and query a monitor expression
##AUTHOR   Joe Riel
##DATE     Mar 2012
##CALLINGSEQUENCE
##- \CMD('prc', 'str' )
##PARAMETERS
##- 'prc'  : ::name:: or ::string::; identifies a procedure
##- 'str' : (optional) ::string::; monitor expression
##RETURNS
##- ::string:: or `NULL`
##DESCRIPTION
##- The `\CMD` command
##  sets and queries a monitor expression
##  for a procedure.
##  The monitor expression previously assigned for the procedure
##  is returned.
##
##- The 'prc' argument identifies the procedure.
##  It may be either the name of the procedure, or
##  a string that evaluates to the procedure.
##  Strings are useful for specifying local procedures.
##
##- If 'prc' is the string ~"all"~, the monitor expression
##  is active for all procedures.
##
##- The optional 'str' argument is a string
##  corresponding to a Maple expression
##  that is parsed and displayed when 'prc' is active during debugging.
##  If 'str' is the empty string,
##  the monitoring expression is removed.
##
##EXAMPLES
##> with(mdc):
##- Assign procedures `f` and `g`.
##>> f := proc(x)
##>> local i,y;
##>>     y := x;
##>>     for i to 10 do
##>>         y := g(x);
##>>         y := y+1;
##>>     end do;
##>>     y;
##>> end proc:
##>> g := proc(w)
##>> local x,z;
##>>     x := w+1;
##>>     z := x^2;
##>>     z;
##>> end proc:
##- Assign monitor expressions for the `f` and `g` procedures.
##> Monitor( f, "['i'=i, 'y'=y]" );
##> Monitor( g, "'z'=z");
##- Instrument `f`, then begin debugging.
##  Be sure to turn-on monitoring in the debugger (type **m**).
##> mdc(f,quiet);
##>(noexecute) f(1);
##- Assign a global monitor that displays the value of `x`.  It is
##  used for all procedures; its output appears before any local
##  monitor output.
##> Monitor("all", "[\"x\"=x, 'x'=x]");
##
##-(nolead) Note (above) that two nearly-identical equations are used,
##  the sole difference is that double-quotes are used around the `x`
##  in one, and single-quotes are used in the other.  Launch the
##  debugger and observe the difference in output when in the `f` and
##  `g` procedures.  In `f`, the single-quoted `x` appears as a
##  numeric value, while in `g` it appears as `x`.  The reason for
##  that is that `x` is a parameter of `f`.  As such, it will always
##  be fully evaluated in a monitored expression, whether or
##  not it has single-quotes.
##>(noexecute) f(1);
##
##SEEALSO
##- "mdc"
##- "mdc[package]"

$define IDENTIFIER {name,string}

Monitor := proc( prc :: IDENTIFIER, str :: string := NULL, $ )
local addr, expr, prev;

    if prc = "all" then
        addr := 0;
    else
        addr := addressof(getname(prc));
    end if;

    if assigned(monitor_expr[addr]) then
        prev := monitor_expr[addr];
    else
        prev := NULL;
    end if;

    if str <> NULL then
        expr := StringTools:-Trim(str);
        if expr = "" then
            monitor_expr[addr] := evaln(monitor_expr[addr]);
        else
            monitor_expr[addr] := expr;
        end if;
    end if;

    return prev;

end proc;

$undef IDENTIFIER

