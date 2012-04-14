##INCLUDE ../include/mpldoc_macros.mpi
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
##  sets and queries a *monitor expression* for a procedure.
##  When monitoring is enabled, and the selected procedure is current,
##  the expression is evaluated after each statement
##  and the result is displayed in the debugger output buffer.
##
##- The monitor expression previously set for the procedure is
##  returned.  If none has been assigned, `NULL` is returned.
##
##- The 'prc' argument identifies the procedure.
##  It may be either the name of the procedure, or
##  a string that evaluates to the procedure name.
##  The evaluation of a string is done with
##  _kernelopts(opaquemodules=false)_,
##  so strings can be used to specify procedures local to modules.
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
##- Assign procedures `f` and `g` used in the following examples.
##> f := proc(x)
##>> local i,y;
##>>     y := x;
##>>     for i to 10 do
##>>         y := g(x);
##>>         y := y+1;
##>>     end do;
##>>     y;
##>> end proc:
##> g := proc(w)
##>> local x,z;
##>>     x := w+1;
##>>     z := x^2;
##>>     z;
##>> end proc:
##
##- **Note:** In the following subsections, a few examples use
##  double-quotes inside a Maple string.  They are entered by
##  *escaping* them with a preceding backslash ~("...\\"...")~.
##  The Maple 16 help browser renders these properly,
##  however, if the help page is opened in a worksheet to execute the examples,
##  the conversion may omit the backslash.
##SUBSECTION Local Monitors
##- Assign monitor expressions for the `f` and `g` procedures.
##  A list is used for multiple expressions to keep them on one line
##  in the output.
##> Monitor( f, "['i'=i, 'y'=y]" );
##> Monitor( g, "'z'=z");
##- Instrument `f`, then begin debugging.
##  Be sure to turn-on monitoring in the debugger (type **m**).
##  The `quiet` option to `mdc` suppresses the greeting.
##>(noexecute) mdc(f,quiet);
##>(noexecute) f(1);
##ENDSUBSECTION
##SUBSECTION Global Monitor
##- Using ~"all"~ as the value of 'prc' assigns a global monitor
##  expression used by all procedures.  Here we use it to display the
##  value of `x`.  Global monitor expressions are evaluated and
##  displayed before local monitor expressions.
##
##> Monitor("all", "'x'=x");
##
##-(nolead) The forward quotes around the `x` are intended to protect
##  it from being evaluated.  It works as intended in `g`, but
##  is evaluated in `f`, where `x` is a parameter.  This is
##  a \"feature\" of the debugger kernel.
##
##>(noexecute) mdc(f,quiet);
##>(noexecute) f(1);
##
##ENDSUBSECTION
##SUBSECTION Conditional Monitors
##- The "`if`" function may be used to create a monitor expression
##  that is displayed only when a condition is met.  Note that `\CMD`
##  returns the previously assigned string.
##
##> Monitor( f, "`if`(3 < i, 'i'=i, NULL)" );
##>(noexecute) mdc(f,quiet);
##>(noexecute) f(1);
##ENDSUBSECTION
##
##SUBSECTION Advanced Monitors
##- A monitor expression can call procedures.
##  For example, here it is used with "mdc[Count]" to
##  store each call to the procedure in a table.
##
##> mon := proc() global S; S[Count()] := [args]; end proc:
##> Monitor( f, "mon(i,x,y)"):
##>(noexecute) mdc(f,quiet);
##>(noexecute) f(1)
##>(noexecute) seq(S[i], i=1..Count('value'));
##
##- Be judicious in the use of such monitors.  Do not call a procedure
##  that is being debugged.
##
##ENDSUBSECTION
##
##SEEALSO
##- "mdc"
##- "mdc[package]"
##- "kernelopts"

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

