##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE SUBMOD Debugger
##MODULE \MOD[\SUBMOD]
##HALFLINE replacement functions for Maple debugger
##AUTHOR   Joe Riel
##DATE     May 2011
##DESCRIPTION
##- The `\MOD` module exports two methods, "Replace" and "Restore",
##  that replace and restore, respectively, the (thankfully) global
##  procedures that are the library side of the Maple debugger.
##
##- The new procedures use the "Sockets" package to communicate with a
##  "Maple Debug Server".  They also insert tags into the stream that
##  indicate the purpose of each packet.  This simplifies the
##  parsing requirements of the server.

$define DEBUGGER_PROCS debugger, `debugger/printf`, `debugger/readline`, showstat, showstop, where

$define LOGFILE "readline.log"

Debugger := module()

#{{{ declarations

export GoBack
    ,  Monitor
    ,  Printf
    ,  Replace
    ,  Reset
    ,  Restore
    ,  RestoreBuiltins
    ,  ShowError
    ,  ShowException
    ,  ShowstatAddr
    ,  Skip
    ,  stopat
    ,  unstopat
    ;

global DEBUGGER_PROCS;

local _debugger
    , debugger_printf
    , debugger_readline
    , debugged_builtins
    , enter_procname := NULL
    , _showstat
    , _showstop
    , _where
    , _print
    , go_back := false
    , go_back_proc
    , go_back_state := 0
    , here_cnt := 0
    , here_proc
    , here_state
    , last_evalLevel
    , last_state
    , monitoring := false
    , monitor_addr := 0
    , monitor_expr
    , monitor_result := false
    , orig_print
    , orig_stopat
    , getname
    , replaced
$ifdef LOG_READLINE
    , logpid
$endif
    , parse_debugger
    , skip
    , ModuleLoad
    ;

    last_evalLevel := 0;

#}}}

#{{{ ModuleLoad

    ModuleLoad := proc()
    local ver;
        # Newer version of Maple, 14+, allow the keyword 'debugger'
        # passed to 'parse', which does something, not sure what.
        replaced := false;
        ver := kernelopts('version');
        ver := sscanf(ver, "%s %d")[2];
        if ver < 14 then
            parse_debugger := NULL;
        else
            parse_debugger := 'debugger';
        end if;
        NULL;
    end proc;

#}}}

#{{{ Replace

    Replace := proc()
        if replaced <> true then
            # Save these
            orig_print  := eval(print);
            orig_stopat := eval(:-stopat);
            # Reassign library debugger procedures
            unprotect('DEBUGGER_PROCS', :-stopat);
            :-debugger          := eval(_debugger);
            `debugger/printf`   := eval(debugger_printf);
            `debugger/readline` := eval(debugger_readline);
            :-showstat          := eval(_showstat);
            :-showstop          := eval(_showstop);
            :-stopat            := eval(stopat);
            :-where             := eval(_where);
            #print              := eval(_print);
            #printf             := eval(_printf);
            protect('DEBUGGER_PROCS', :-stopat);
            replaced := true;
$ifdef LOG_READLINE
            logpid := fopen(LOGFILE,'APPEND','TEXT');
$endif
        end if;
        return NULL;
    end proc;

#}}}
#{{{ Reset

    Reset := proc()
        last_state := NULL;
    end proc;

#}}}
#{{{ Restore

    Restore := proc()
        # Dave H. suggests using 'forget'
        if replaced = true then
            map( p -> kernelopts('unread' = p), ['DEBUGGER_PROCS', :-stopat] );
            replaced := false;
        end if;
        return NULL;
    end proc;

#}}}
#{{{ RestoreBuiltins

    RestoreBuiltins := proc()
    local pnam;
        if debugged_builtins :: table then
            for pnam in [indices(debugged_builtins,'nolist')] do
                proc(f) f := eval(debugged_builtins[pnam]); end proc(pnam);
                debugged_builtins[pnam] := evaln(debugged_builtins[pnam]);
            end do;
        end if;
        NULL;
    end proc;
#}}}

#{{{ Print and _printf

    Printf := proc()
        debugger_printf('MDC_PRINTF', _rest);
    end proc;

    # currently not used
    _print := proc()
        orig_print(_passed);
        debugger_printf('DBG_WARN', "print output does not display in debugger\n");
    end proc;

#}}}
#{{{ ShowError

    ShowError := proc()
    local err;
        err := debugopts('lasterror');
        if err = '`(none)`' then
            debugger_printf('DBG_ERROR', "%a\n", err);
        else
            debugger_printf('DBG_ERROR', "%s\n", StringTools:-FormatMessage(err));
        end if;
    end proc;

#}}}
#{{{ ShowException

    ShowException := proc()
    local except;
        except := debugopts('lastexception');
        if except = '`(none)`' then
            debugger_printf('DBG_EXCEPTION', "%a\n", except);
        else
            debugger_printf('DBG_EXCEPTION', "%s\n", StringTools:-FormatMessage(except[2..]));
        end if;
    end proc;

#}}}

# Debugger replacements

#{{{ debugger_printf

    debugger_printf := proc( tag :: name )
    local argList, rts;
    description `Used by debugger to produce output.`;

        argList := [_rest];
        try
            # suppress large rtables
            rts := map(x->x=`debugger/describe_rtable`(x)
                       , indets(argList,'debugger_large_rtable')
                      );
            if rts <> {} then argList := subs(rts,argList) end if;
        catch:
        end try;
        WriteTagf(tag, op(argList));

        return NULL;
    end proc:

#}}}
#{{{ debugger_readline

# Used for user-input to the debugger. This lets us easily change the input
# facilities to take advantage of special features of the Iris in future.

    debugger_readline := proc()
    local line,n;
    description `Used by debugger to obtain user-input.`;
        do
            debugger_printf('DBG_PROMPT', ">");
            try
                line := Read();
                n := -1;
                while line[n] = "\n" do
                    n := n-1;
                end do;
                return line[..n];
            catch "process %1 disconnected unexpectedly":
                error sprintf("%s; use mdc(reconnect) to reconnect"
                              , StringTools:-FormatMessage(lastexception[2..])
                             );
            catch:
                debugger_printf('DBG_ERR'
                                , "Error, %s\n"
                                , StringTools:-FormatMessage(lastexception[2..])
                               );
            end try;
        end do;
    end proc:

#}}}

$include <src/debugger.mm>

#{{{ showstat

    _showstat := proc( p::{name,`::`}, statnumoroverload::{integer,range}, statnum::{integer,range}, $ )
    option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
    description `Displays a procedure with statement numbers and breakpoints.`;
    local res;
    global showstat;
        if _npassed = 0 then map(procname,stopat())
        else
            if _npassed = 1 then
                res := debugopts('procdump'=p)
            elif _npassed = 2 then
                res := debugopts('procdump'=[p,statnumoroverload])
            elif _npassed = 3 then
                res := debugopts('procdump'=[p,statnumoroverload,statnum])
            fi;

            map[3](debugger_printf, 'DBG_SHOW_INACTIVE', "\n%s", [res]);

            # nonl probably means "no newline"
            if procname <> 'showstat[nonl]' then
                debugger_printf('DBG_NULL', "\n" )
            fi
        fi;
        NULL
    end proc:

#}}}
#{{{ ShowstatAddr

    ShowstatAddr := proc( addr :: integer
                          , {dead :: truefalse := false}
                          , $
                        )
$ifdef DONTUSE
    local prc, pstr;

        prc := pointto(addr);
        pstr := convert(debugopts('procdump' = prc),string);

        # Alas, this does not work.  Eval'ing (or op'ing)
        # the procedure can cause the debugger to execute.

        # Eval'ing (or op'ing) prc can cause the
        # debugger to run ahead the first time this is done
        # in a module local procedure.

        # Create option string
        opts := op(3, op(prc));
        if opts = NULL then
            opts := "";
        else
            opts := sprintf("option %q;\n", opts);
        end if;

        # Create description string
        desc := op(5, op(prc));
        if desc = NULL then
            desc := "";
        else
            desc := sprintf("description %a;\n", desc);
        end if;

        # Split at first statement and insert opts and desc
        pos := StringTools:-Search("\n   1", pstr);
        pstr := cat(pstr[..pos]
                    , opts
                    , desc
                    , pstr[pos+1..]
                   );
$endif
        WriteTagf(`if`(dead
                       , 'DBG_SHOW_INACTIVE'
                       , 'DBG_SHOW'
                      )
                  , "<%d>\n%A"
                  , addr
                  , debugopts('procdump' = pointto(addr))
                 );
        NULL;
    end proc;

#}}}
#{{{ showstop

    _showstop := proc( $ )
    option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
    description `Display a summary of all break points and watch points.`;
    local i, ls, val, width;
    global showstop;

        ls := stopat();
        if nops(ls) = 0 then debugger_printf('DBG_INFO', "\nNo breakpoints set.\n")
        else
            debugger_printf('DBG_INFO', "\nBreakpoints in:\n");
            for i in ls do debugger_printf('DBG_INFO', "   %a\n",i) od
        fi;
        ls := stopwhen();
        if nops(ls) = 0 then debugger_printf('DBG_INFO', "\nNo variables being watched.\n")
        else
            debugger_printf('DBG_INFO', "\nWatched variables:\n");
            for i in ls do
                if i :: list then
                    debugger_printf('DBG_INFO', "   %a in procedure %a\n",i[2],i[1])
                elif assigned(`debugger/watch_condition`[i]) then
                    val := sprintf('DBG_INFO', "%a",`debugger/watch_condition`[i]);
                    width := streamcall('INTERFACE_GET(screenwidth)');
                    width := `if`(width :: even, width/2, (width-1)/2);
                    if length(val) > width then
                        val := sprintf("%s ...", val[1..width])
                    fi;
                    debugger_printf('DBG_INFO', "   %a = %s\n",i,val)
                else
                    debugger_printf('DBG_INFO', "   %a\n",i)
                fi
            od
        fi;
        ls := stoperror();
        if nops(ls) = 0 then debugger_printf('DBG_INFO', "\nNo errors being watched.\n")
        else
            debugger_printf('DBG_WATCHED_ERRS', "\nWatched errors:\n");
            if member('all',ls) then
                if member('traperror',ls) then
                    debugger_printf('DBG_INFO', "   All errors\n")
                else
                    debugger_printf('DBG_INFO', "   All untrapped errors\n")
                fi
            else
                if member('traperror',ls) then
                    debugger_printf('DBG_INFO', "   All trapped errors\n")
                fi;
                for i in ls do
                    if i <> 'traperror' then debugger_printf('DBG_INFO', "   %a\n",i) fi
                od
            fi
        fi;
        if procname <> 'showstop[nonl]' then debugger_printf('DBG_INFO', "\n") fi;
        NULL
    end proc:

#}}}
#{{{ where

    _where := proc( n::integer, $ )
    local stack, i;
    option `Copyright (c) 1996 Waterloo Maple Inc. All rights reserved.`;
        if _npassed = 1 then
            if n < 1 then debugopts('callstack'=-1) fi;	# To force an error.
            stack := debugopts('callstack'=n+1)
        else
            stack := debugopts('callstack')
        fi;
        for i from nops(stack)-2 to 8 by -3 do
            debugger_printf('DBG_STACK1', "%a: %s\n\t%a\n",stack[i],stack[i+1],stack[i-1])
        od;
        if stack[5] = 'TopLevel' then
            debugger_printf('DBG_STACK2',"Currently at TopLevel.\n")
        else
            debugger_printf('DBG_STACK3',"Currently in %a.\n",stack[5])
        fi;
        NULL
    end proc:

#}}}

#{{{ stopat

##DEFINE CMD stopat
##PROCEDURE \MOD[\SUBMOD][\CMD]
##HALFLINE a fast method to instrument a procedure
##AUTHOR   Erik Postma
##DATE     May 2010
##CALLINGSEQUENCE
##- \CMD('p', 'n', 'cond')
##PARAMETERS
##- 'p'    : ::{name,string,list}::; procedure to instrument
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
##-- If 'p' is a list then _\CMD(op(p))_ is returned.

    stopat := proc(p :: {name,string,list}
                   , n :: posint
                   , cond :: uneval
                   , $ )
    local pnam,st;
        if _npassed = 0 then
            # this isn't cheap.  May want to "improve".
            return orig_stopat();
        end if;
        if p :: list then
            return procname(op(p));
        end if;
        pnam := getname(p);
        # debugbuiltins is a module local, which is a design flaw, but
        # avoiding it is tricky.  Passing it as a keyword parameter is
        # not possible because cond is optional yet declared as 'uneval'.
        if debugbuiltins and pnam :: 'builtin' then
            # These are used in the debugger, or have special
            # evaluation rules, so cannot be debugged.
            # NOTE: Some could be debugged by toggling
            # assignments when entering/exiting the debugger.
            # But that is overkill for little use.
            if member(pnam, '{ASSERT
                              , DEBUG
                              , SearchText
                              , `if`
                              , add
                              , addressof
                              , assigned
                              , debugopts
                              , eval
                              , evalf
                              , evalhf
                              , evaln
                              , has
                              , indets
                              , iolib
                              , kernelopts
                              , length
                              , map
                              , mul
                              , nops
                              , op
                              , pointto
                              , parse
                              , print
                              , seq
                              , streamcall
                              , subs
                              , subsop
                              , time
                              , timelimit
                              , traperror
                              , trunc
                              , type
                              , userinfo
                             }') then
                error "cannot debug '%1'", pnam;
            end if;

            debugged_builtins[pnam] := eval(pnam);

            unprotect(pnam);
            proc(f)
                f := subs(_f = eval(f), proc() _f(_passed) end proc);
            end proc(pnam);
            return procname(pnam, _passed[2..]);
        end if;

        st := `if`(_npassed=1,1,n);
        if _npassed <= 2 then debugopts('stopat'=[pnam, st])
        else                  debugopts('stopat'=[pnam, st, 'cond'])
        end if;
        return NULL;
    end proc:

#}}}
#{{{ unstopat

    unstopat := proc(p :: {name,string,list}
                     , n :: posint
                     , cond :: uneval
                     , $ )
    local pnam,st;
        if p :: list then
            return procname(op(p));
        end if;
        pnam := getname(p);
        st := `if`(_npassed=1,1,n);
        if _npassed <= 2 then debugopts('stopat'=[pnam, -st])
        else                  debugopts('stopat'=[pnam, -st, 'cond'])
        end if;
        if assigned(debugged_builtins[pnam]) then
            proc(f) f := eval(debugged_builtins[pnam]); end proc(pnam);
            debugged_builtins[pnam] := evaln(debugged_builtins[pnam]);
        end if;
        return NULL;
    end proc:

#}}}

#{{{ getname

    getname := proc(p :: {name,string}, $)
    local opacity, pn, pnm;
        try
            opacity := kernelopts('opaquemodules'=false);

            if p :: indexed then
                # Convert indexed to slashed name;
                # e.g. pkg[func] --> `pkg/func`
                pn := indexed2slashed(p);
                if not eval(pn)::procedure
                or (eval(p)::procedure and not has(eval(p),pn)) then
                    pn := p;
                end if;
            elif p :: string then
                pn := parse(p);
            else
                pn := p;
            end if;

            # Use eval in order to make sure everything is loaded from
            # the library. pnm is not returned (below) because
            # debugopts(stopat) needs a name if it is to return the
            # name on the lhs of the assignment in the string.
            pnm := eval(pn);
            while assigned(pnm[':-ModuleApply']) do
                pnm := eval(pnm:-ModuleApply);
            end do;
        finally
            kernelopts('opaquemodules'=opacity);
        end try;

        return pn;

    end proc;

#}}}

#{{{ GoBack

    GoBack := proc({ clear :: truefalse := false }, $ )
        if clear then
            go_back := false;
            go_back_state := 0;
            skip := false;
            return NULL;
        else
            if go_back_state = 0 then
                error "no previous state saved";
            end if;
            go_back := true;
            skip := true;
            return [go_back_proc, go_back_state];
        end if;
    end proc;
#}}}
#{{{ Skip

    Skip := proc( { clear :: truefalse := false }, $ )
        skip := not clear;
    end proc;

#}}}

#{{{ Monitor

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
            prev := assigned(monitor_expr[addr])
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

#}}}

end module;
