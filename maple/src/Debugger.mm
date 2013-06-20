##INCLUDE ../include/mpldoc_macros.mpi
##MODULE mdc[Debugger]
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

$define DEBUGGER_PROCS debugger, showstat, showstop, where

$define LOGFILE "readline.log"

Debugger := module()

#{{{ declarations

export CallStack
    ,  Enter
    ,  GoBack
    ,  Monitor
    ,  Printf
    ,  Replace
    ,  Reset
    ,  Restore
    ,  RestoreBuiltins
    ,  SetQuiet
    ,  SkipBefore
    ,  ShowError
    ,  ShowException
    ,  ShowstatAddr
    ,  Skip
    ,  stopat
    ,  unstopat
    ,  GetName
    ;

global DEBUGGER_PROCS;

local _debugger
    , debugger_printf
    , debugger_readline
    , debugged_builtins
    , enter_procname := NULL
    , _showstat
    , _showstop
    , _print
    , go_back := false        # flag, true skips to go_back_proc : go_back_state
    , go_back_addr            # address of procedure to go-back to.
    , go_back_state := 0      # statement number to go-back to
    , here_cnt := 0
    , here_addr
    , here_state
    , last_evalLevel
    , last_state
    , monitoring := false
    , monitor_addr := 0
    , monitor_expr
    , monitor_result := false
    , orig_print
    , orig_stopat
    , Quiet := false
    , Respond := false
    , replaced
    , skip_before := NULL
    , skip_before_halt := true
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
        # Expand CallStack so that it will not appear on the stack
        # when called during debugging.  Is this a general property of
        # inlined procedures?  That is, do they appear once, then not
        # again?
        CallStack(1,[0$4]);
        NULL;
    end proc;

#}}}

#{{{ Replace

##PROCEDURE mdc[Debugger][Replace]
##HALFLINE replace library debugger procedures
##AUTHOR   Joe Riel
##CALLINGSEQUENCE
##- Replace()
##DESCRIPTION
##- Replace the library debugger procedures
##  with custom procedures that communicate
##  with the Emacs debugger server.
##
##- Assign the module-local variable `replaced` to true.
##SEEALSO
##- "Restore"

    Replace := proc()
        if replaced <> true then
            # Save these
            orig_print  := eval(print);
            orig_stopat := eval(:-stopat);
            # Reassign library debugger procedures
            unprotect('DEBUGGER_PROCS', ':-stopat');
            :-debugger          := eval(_debugger);
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

##PROCEDURE mdc[Debugger][Restore]
##HALFLINE restore debugger procedures
##DESCRIPTION
##- Restore the debugger procedures
##  ~debugger~, ~showstat~, ~showstop~, ~stopat~, and ~where~.

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

##PROCEDURE mdc[Debugger][RestoreBuiltins]
##HALFLINE restore builtin procedures
##DESCRIPTION
##- Called by ~mdc:-ModuleUnload~, it restores the builtin procedures.

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
        debugger_printf(TAG_PRINTF, _rest);
    end proc;

    # currently not used
    _print := proc()
        orig_print(_passed);
        debugger_printf(TAG_WARN, "print output does not display in debugger\n");
    end proc;

#}}}
#{{{ ShowError

    ShowError := proc()
    local err;
        err := debugopts('lasterror');
        if err = '`(none)`' then
            debugger_printf(TAG_ERROR, "%a\n", err);
        else
            debugger_printf(TAG_ERROR, "%s\n", StringTools:-FormatMessage(err));
        end if;
    end proc;

#}}}
#{{{ ShowException

    ShowException := proc()
    local except;
        except := debugopts('lastexception');
        if except = '`(none)`' then
            debugger_printf(TAG_ERROR, "%a\n", except);
        else
            debugger_printf(TAG_ERROR, "%s\n", StringTools:-FormatMessage(except[2..-1]));
        end if;
    end proc;

#}}}

# Debugger replacements

#{{{ debugger_printf

    debugger_printf := proc( tag :: string )
    local argList, rts;
    description `Used by debugger to produce output.`;

        argList := subsindets([_rest], `record`
                              , proc(rec) 'Record'(exports(rec)) end proc
                             );

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

##PROCEDURE mdc[Debugger][debugger_readline]
##CALLINGSEQUENCE
##- debugger_readline('prompt')
##PARAMETERS
##- 'prompt' : ::truefalse::; true means send a prompt to the server
##RETURNS
##- ::string::
##DESCRIPTION
##- Read and return the input from the server.
##- If 'prompt' is true, send a prompt to the server before
##  attempting to read input.

    debugger_readline := proc( prompt :: truefalse )
    local line,n;
    description `Used by debugger to obtain user-input.`;
        do
            if prompt then
                debugger_printf(TAG_PROMPT);
            end if;
            try
                line := Sockets:-Read(sid);
            catch:
                debugger_printf(TAG_ERROR
                                , "Error, %s\n"
                                , StringTools:-FormatMessage(lastexception[2..-1])
                               );
            end try;
            if line = false then
                error "process %1 disconnected unexpectedly; use mdc(reconnect) to reconnect", sid;
            end if;
            # Remove trailing \n's.
            n := -1;
            while line[n] = "\n" do
                n := n-1;
            end do;
            return line[1..n];
        end do;
    end proc:

#}}}

$include <src/debugger.mm>

#{{{ showstat

    _showstat := proc( p::{name,`::`}, statnumoroverload::{integer,range}, statnum::{integer,range}, $ )
    option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
    description `Display a procedure with statement numbers and breakpoints.`;
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

            map[3](debugger_printf, TAG_SS_DEAD, "\n%s", [res]);

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
        pstr := cat(pstr[1..pos]
                    , opts
                    , desc
                    , pstr[pos+1..-1]
                   );
$endif
        WriteTagf(`if`(dead
                       , TAG_SS_DEAD
                       , TAG_SS_LIVE
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
        if nops(ls) = 0 then debugger_printf(TAG_INFO, "\nNo breakpoints set.\n")
        else
            debugger_printf(TAG_INFO, "\nBreakpoints in:\n");
            for i in ls do debugger_printf(TAG_INFO, "   %a\n",i) od
        fi;
        ls := stopwhen();
        if nops(ls) = 0 then debugger_printf(TAG_INFO, "\nNo variables being watched.\n")
        else
            debugger_printf(TAG_INFO, "\nWatched variables:\n");
            for i in ls do
                if i :: list then
                    debugger_printf(TAG_INFO, "   %a in procedure %a\n",i[2],i[1])
                elif assigned(`debugger/watch_condition`[i]) then
                    val := sprintf(TAG_INFO, "%a",`debugger/watch_condition`[i]);
                    width := streamcall('INTERFACE_GET(screenwidth)');
                    width := `if`(width :: even, width/2, (width-1)/2);
                    if length(val) > width then
                        val := sprintf("%s ...", val[1..width])
                    fi;
                    debugger_printf(TAG_INFO, "   %a = %s\n",i,val)
                else
                    debugger_printf(TAG_INFO, "   %a\n",i)
                fi
            od
        fi;
        ls := stoperror();
        if nops(ls) = 0 then debugger_printf(TAG_INFO, "\nNo errors being watched.\n")
        else
            debugger_printf(TAG_WATCHED, "\nWatched errors:\n");
            if member('all',ls) then
                if member('traperror',ls) then
                    debugger_printf(TAG_INFO, "   All errors\n")
                else
                    debugger_printf(TAG_INFO, "   All untrapped errors\n")
                fi
            else
                if member('traperror',ls) then
                    debugger_printf(TAG_INFO, "   All trapped errors\n")
                fi;
                for i in ls do
                    if i <> 'traperror' then debugger_printf(TAG_INFO, "   %a\n",i) fi
                od
            fi
        fi;
        if procname <> 'showstop[nonl]' then debugger_printf(TAG_INFO, "\n") fi;
        NULL
    end proc:

#}}}

#{{{ stopat

##DEFINE CMD stopat
##PROCEDURE mdc[Debugger][stopat]
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
    description `Set a breakpoint in a procedure`;
    local pnam,statenum;
        if _npassed = 0 then
            # this isn't cheap.  May want to "improve".
            return orig_stopat();
        end if;
        if p :: list then
            return procname(op(p));
        end if;
        pnam := GetName(p);
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
            return procname(pnam, _passed[2..-1]);
        end if;

        statenum := `if`(_npassed=1,1,n);
        if _npassed <= 2 then debugopts('stopat'=[pnam, statenum])
        else                  debugopts('stopat'=[pnam, statenum, 'cond'])
        end if;
        return NULL;
    end proc:

#}}}
#{{{ unstopat

##PROCEDURE mds[Debugger][unstopat]
##HALFLINE clear a breakpoint in a procedure
##AUTHOR   Joe Riel
##CALLINGSEQUENCE
##- unstopat('p','n','cond')
##PARAMETERS
##- 'p'    : ::{name,string,list}::; procedure to modify
##- 'n'    : (optional) ::posint::; statement number
##- 'cond' : (optional) ::uneval::; condition
##RETURNS
##- `NULL`
##DESCRIPTION
##- Clear a breakpoint in a procedure.

    unstopat := proc(p :: {name,string,list}
                     , n :: posint
                     , cond :: uneval
                     , $ )
    description `Clear a breakpoint in a procedure`;
    local pnam,st;
        if p :: list then
            return procname(op(p));
        end if;
        pnam := GetName(p);
        st := `if`(_npassed=1,1,n);
        if _npassed <= 2 then debugopts(':-stopat'=[pnam, -st])
        else                  debugopts(':-stopat'=[pnam, -st, ':-cond'])
        end if;
        if assigned(debugged_builtins[pnam]) then
            proc(f) f := eval(debugged_builtins[pnam]); end proc(pnam);
            debugged_builtins[pnam] := evaln(debugged_builtins[pnam]);
        end if;
        return NULL;
    end proc:

#}}}

#{{{ CallStack

    CallStack := proc(depth::posint,stk::list)
    option inline;
        stk[1+3*(depth-1)+1](op(stk[1+3*(depth-1)+3]));
    end proc;

#}}}
#{{{ GetName

    GetName := proc(p :: {name,string}, $)
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

#{{{ SetQuiet

    SetQuiet := proc( quiet :: truefalse
                      , { toggle :: truefalse := false }
                    )
        if toggle then
            Quiet := not Quiet;
        else
            Quiet := quiet;
        end if;
        NULL;
    end proc;

#}}}
#{{{ Enter

    Enter := proc(pname :: string := "")
        enter_procname := `if`(pname = ""
                               , NULL
                               , pname
                              );
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
            return [go_back_addr, go_back_state];
        end if;
    end proc;
#}}}
#{{{ Skip

    Skip := proc( { clear :: truefalse := false }, $ )
        skip := not clear;
    end proc;
#}}}

#{{{ SkipBefore

    SkipBefore := proc( target :: string )
    local prev;
        prev := skip_before;
        skip_before := target;
        skip_before_halt := true;
        prev;
    end proc;

#}}}

$include <src/Monitor.mm>

end module;
