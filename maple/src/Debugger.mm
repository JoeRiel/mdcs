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

$define DBG_EVAL1 DBG_EVAL
$define DBG_EVAL2 DBG_EVAL
$define DBG_EVAL3 DBG_EVAL
$define DBG_EVAL4 DBG_EVAL
$define LOGFILE "readline.log"

Debugger := module()

#{{{ declarations

export Printf
    ,  Replace
    ,  Restore
    ,  ShowError
    ,  ShowException
    ,  ShowstatAddr
    ,  stopat
    ,  unstopat
    ;

global DEBUGGER_PROCS;

local _debugger
    , debugger_printf
    , debugger_readline
    , _showstat
    , _showstop
    , _where
    , _print
    , last_state
    , orig_print
    , orig_stopat
    , getname
    , replaced
$ifdef LOG_READLINE
    , logpid
$endif
    , parse_debugger
    , ModuleLoad
    ;

#}}}

#{{{ ModuleLoad

    ModuleLoad := proc()
    local ver;
        # Newer version of Maple, 14+, allow a the keyword 'debugger'
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
        if not replaced then
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
#{{{ Restore

    Restore := proc()
        # Dave H. suggests using 'forget'
        if replaced then
            map( p -> kernelopts('unread' = p), ['DEBUGGER_PROCS', :-stopat] );
            replaced := false;
        end if;
        return NULL;
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

        # suppress large rtables
        rts := map(x->x=`debugger/describe_rtable`(x)
                   , indets(argList,'debugger_large_rtable')
                  );
        if rts <> {} then argList := subs(rts,argList) end if;

        WriteTagf(tag, op(argList));

        return NULL;
    end proc:

#}}}
#{{{ debugger_readline

# Used for user-input to the debugger. This lets us easily change the input
# facilities to take advantage of special features of the Iris in future.

    debugger_readline := proc( )
    option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
    description `Used by debugger to obtain input.`;
    local len, res, startp, endp, i, endcolon;
    global `debugger/default`;

        #{{{ Get response (from server)

        do
            debugger_printf('DBG_PROMPT', ">");
            try
                res := Read();
$ifdef LOG_READLINE
                fprintf(logpid, "[%s]\n", res);
                fflush(logpid);
$endif
                break
            catch "process %1 disconnected unexpectedly":
                error;
            catch:
                debugger_printf('DBG_ERR'
                                , "Error, %s\n"
                                , StringTools:-FormatMessage(lastexception[2..])
                               );
            end try;
        od;

        #}}}
        #{{{ Hande statement
        if sscanf(res, "%s") = ["statement"] then
            return res;
        end if;
        #}}}
        #{{{ Handle solo enter (repeat previous command)

        # If the user just pressed ENTER, use the value of the variable
        # `debugger/default`, which contains whatever debugger command the user
        # issued last time. If there is no previous command, return a command to
        # print a message pointing the user to the on-line help.
        if `debugger/isspace`( res ) then
            res := `if`(assigned(`debugger/default`)
                        , `debugger/default`
                        , "debugger_printf('DBG_HELP', \"See ?debugger for available commands\n\")"
                       );
        else
            `debugger/default` := res;
        fi;

        #}}}
        #{{{ Remove leading blanks, trailing comments, and colons.

        startp := 1;
        len := length(res);
        endp := len;
        endcolon := false;
        for i from 1 to len do
            if startp = i and res[i] = " " then
                startp := i+1
            elif res[i] = "#" then
                endp := i-1;
                break
            elif res[i] = ";" then
                endp := i-1;
                break
            elif res[i] = ":" then
                if res[i+1] = ":" then
                    # allow for `::` expressions in the debugger
                    i := i + 1;
                    next
                fi;
                if i = len or not member(res[i+1],{"=","-",":"}) then
                    endp := i-1;
                    endcolon := true;
                    break
                fi
            fi
        end do;

        #}}}
        #{{{ Record whether or not the command ended in a colon.

        if _npassed > 0 and type(_passed[1],name) then
            assign(_passed[1],endcolon)
        fi;

        #}}}
        #{{{ Check for characters after comment

        # Complain if there were any non-blank, non-comment
        #characters after the end of the typed command.
        if i < len and res[i] <> "#" then
            for i from i to len do
                if res[i] <> " " then
                    debugger_printf('DBG_WARN',"Warning, extra characters at end of parsed string\n");
                    debugger_printf('DBG_WARN',"Extra stuff: %q\n", res[i]);
                    break
                end if;
            end do;
        end if;

        #}}}
        #{{{ Strip trailing whitespace.
        while endp >= startp and res[endp] <= " " do endp := endp - 1 od;
        res := res[startp..endp];
        #}}}
        #{{{ Extraneous stuff

        # Means the user typed something which still ended up being nothing, such
        # as a single semicolon, just a comment, etc.
        if res = "" then
            res := "`debugger/printf`(\"See ?debugger for available commands\n\")"
        fi;

        #}}}

        return res;

    end proc:

#}}}
#{{{ debugger

# The debugger proper. This gets invoked after a call to the function debug()
# is encountered.

    _debugger := proc( )
    option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
    description
        `Invoked by Maple when a breakpoint or watchpoint is encountered.`,
        `Not intended to be called directly.`;
    local procName, statNumber, evalLevel, i, j, n, line, original, statLevel
        , state, pName, lNum, cond, cmd, err, module_flag;
    global showstat, showstop, `debugger/no_output`;

        evalLevel := kernelopts('level') - 21;

        n := _npassed;
        if n > 0
        and type(_passed[n],list)
        and nops(_passed[n]) > 0
        and _passed[n][1] = 'DEBUGINFO' then
            procName := _passed[n][2];
            statNumber := _passed[n][3];
            statLevel := _passed[n][4];
            n := n - 1;

            if skip then
                skip := not match_predicate(_passed[1..n]);
            end if;
        else
            procName := 0;
            statLevel := trunc(evalLevel / 5); # Approximately #
        fi;

        #{{{ remove indices in procName
        # Added by Joe Riel.  Indices are used by some procedures,
        # for example, map[3], but need to be removed.  Multiple
        # indices are possible.
        while procName :: 'And(indexed,Not(procedure))' do
            procName := op(0,procName);
        end do;
        #}}}
        #{{{ process args

        if not skip then
            for i from 1 to n do
                # Use addressof to prevent an object from overriding
                # equality.
                if addressof(_passed[i]) = addressof(lasterror) then
                    debugger_printf('MPL_ERR', "Error, %s\n"
                                    , StringTools:-FormatMessage(lastexception[2..]))
                elif type(_passed[i],list) and nops(_passed[i]) >= 1 then
                    if _passed[i][1] = 'DEBUGSTACK' then
                        j := nops(_passed[i]) - 2;
                        while j > 2 do
                            if _passed[i][j+1] = `` then
                                debugger_printf('DBG_STACK'
                                                , "<%d>\n%a\n"
                                                , addressof(_passed[i][j])
                                                , _passed[i][j]
                                               );
                            else
                                debugger_printf('DBG_WHERE'
                                                , "<%d>\n%a: %s\n"
                                                , addressof(_passed[i][j])
                                                , _passed[i][j]
                                                , _passed[i][j+1]
                                               );
                                if `debugger/no_output` <> true then
                                    debugger_printf('DBG_ARGS',"\t%a\n",_passed[i][j-1])
                                fi
                            fi;
                            j := j - 3
                        od
                    elif _passed[i][1] = 'DEBUGERROR' then
                        debugger_printf('DBG_ERR', "Error, %Q\n",op(_passed[i][2..-1]))
                    elif _passed[i][1] = 'DEBUGWATCH' then
                        if assigned(`debugger/watch_condition`[_passed[i][2]])
                        and [`debugger/watch_condition`[_passed[i][2]]] <> [op(_passed[i][3..-1])]
                        then
                            return
                        fi;
                        debugger_printf('DBG_WATCHED_CONDS', "%a := %q\n",_passed[i][2],op(_passed[i][3..-1]))
                    elif `debugger/no_output` <> true then
                        if i < n then
                            # list/set that is part of a continued sequence
                            debugger_printf('DBG_EVAL1', "%a,\n",_passed[i])
                        else
                            # list/set
                            debugger_printf('DBG_EVAL2', "%a\n",_passed[i])
                        fi
                    fi
                elif `debugger/no_output` <> true then
                    if i < n then
                        # expr that is part of a continued sequence
                        debugger_printf('DBG_EVAL3', "%a,\n",_passed[i])
                    else
                        # expr
                        debugger_printf('DBG_EVAL4', "%a\n",_passed[i])
                    fi
                fi
            od;

        end if;

        #}}}

        #{{{ print the debug status

        if procName <> 0 then
            if statNumber < 0 then
                if not skip then
                    # handle negative statement number (indicates multiple targets)
                    debugger_printf('DBG_WARN', "Warning, statement number may be incorrect\n");
                end if;
                statNumber := -statNumber
            end if;

            local dbg_state := debugopts('procdump'=[procName, 0..statNumber]);
            # Set module_flag true if next statement appears to
            # evaluate a module, which causes a debugger error if one
            # attempt to step into it.  The test is simple and uses
            # builtins to keep this fast.
            module_flag := evalb(SearchText("module ()", dbg_state)<>0);
            state := sprintf("<%d>\n%A", addressof(procName), dbg_state);
            if not skip then
                if state = last_state then
                    WriteTagf('DBG_SAME_STATE');
                else
                    last_state := state;
                    debugger_printf('DBG_STATE', "%s", state);
                end if;
            end if;
        end if;

        #}}}

        #{{{ handle skip
        # skip is true if 'skip_until' is in effect.
        if skip then
            if not module_flag then
                debugopts('steplevel'=999999999);
            else
                debugopts('steplevel'=evalLevel+6);
            end if;
            return NULL;
        end if;
        #}}}
        #{{{ command loop

        do
            line := `debugger/readline`('`debugger/no_output`');
            # If there's an assignment, make sure it is delimited by spaces.
            i := searchtext(":=",line);
            if i > 1 and searchtext(" := ",line) <> i-1 then
                line := cat(line[1..i-1]," := ",line[i+2..-1])
            fi;
            original := line;
            # Scan the line into tokens.
            cmd := op(traperror(sscanf(line,"%s")));
            line := traperror(sscanf(line,"%s %a %a %1000c"));
            # Convert an assignment into a call to the assign function.
            if line = "incorrect syntax in parse: %1 (near %-2 character of parsed string)"
            and lastexception[3] = "`:=` unexpected" then
                line := traperror(sscanf(original,"%a := %1000c"));
                if line <> lasterror and nops(line) = 2 then
                    if member(line[1],{anames('environment')}) then
                        line := [cmd,sprintf("%a",line[1]),traperror(parse(line[2],parse_debugger))];
                        cmd := "setenv"
                    else
                        original := sprintf("assign('%a',%a)",line[1]
                                            , traperror(parse(line[2],parse_debugger)));
                        cmd := ""
                    fi
                fi
            fi;
            err := NULL;

            #{{{ parse cmd (else is arbitrary expression)

            if cmd = "cont" then
                return
            elif cmd = "next" then
                debugopts('steplevel'=evalLevel);
                return
            elif cmd = "step" and not module_flag then
                debugopts('steplevel'=999999999);
                return
            elif cmd = "into" or cmd = "step" and module_flag then
                debugopts('steplevel'=evalLevel+6);
                return
            elif cmd = "outfrom" then
                debugopts('steplevel'=evalLevel-2);
                return
            elif cmd = "return" then
                debugopts('steplevel'=evalLevel-statLevel*5);
                return
            elif cmd = "level" then
                debugopts('steplevel' = Level);
                return
            elif cmd = "quit" or cmd = "done" or cmd = "stop" then
                # debugger_printf('DBG_STOP',"stopping\n");
                # ssystem("sleep 1"); FIXME: may need to delay here.
                debugopts('interrupt'=true)
            elif cmd = "where" then
                if nops(line) = 1 then
                    return 'debugopts'('callstack')
                else
                    return 'debugopts'('callstack'=line[2])
                fi
            elif cmd = "showstack" then
                n := debugopts('callstack');
                n := [op(1,n),op(5..-1,n)];
                n := subsop(op(map(`=`,[seq(i*3,i=1..(nops(n)+1)/3)],``)),n);
                return n;
            elif cmd = "stopat" then
                if nops(line) = 4 then
                    try
                        parse(line[4],parse_debugger);
                        line := [line[1],line[2],line[3],err];
                    catch:
                        err := lasterror;
                    end try;
                fi;
                if err <> lasterror then
                    pName := procName;
                    lNum := 1;
                    cond := NULL;
                    for i from 2 to nops(line) do
                        if i <= nops(line) then
                            if type(line[i],name) then pName := line[i]
                            elif type(line[i],{integer,list(integer)}) then
                                lNum := line[i]
                            else cond := line[i]
                            fi
                        fi
                    od;
                    if nops(line) > 1 then
                        try
                            for n in lNum do
                                # Can't call stopat() procedure here because cond
                                # will remain unevaluated (as the name 'cond').
                                debugopts('stopat'=[pName,n,cond])
                            od
                        catch:
                            err := lasterror
                        end
                    fi;
                    if err <> lasterror then return []; (* stopat() *) fi
                fi
            elif cmd = "unstopat" then
                pName := procName;
                lNum := NULL;
                for i from 2 to nops(line) do
                    if type(line[i],name) then pName := line[i]
                    else lNum := line[i]
                    fi
                od;
                try
                    unstopat(pName,lNum);
                catch:
                    err := lasterror;
                end try;
                if err <> lasterror then return err fi
            elif cmd = "showstat" or cmd = "list" then
                if procName = 0 then
                    debugger_printf('DBG_WARN',"Error, not currently in a procedure\n");
                elif nops(line) = 1 and cmd = "list" then
                    i := statNumber - 5;
                    if i < 1 then i := 1 fi;
                    try
                        showstat['nonl'](procName,i..statNumber+1);
                    catch:
                        err := lasterror;
                    end try;
                else
                    pName := procName;
                    lNum := NULL;
                    for i from 2 to nops(line) do
                        if type(line[i],name) then pName := line[i]
                        else lNum := line[i]
                        fi
                    od;
                    try
                        showstat['nonl'](pName,lNum);
                    catch:
                        err := lasterror;
                    end try;
                fi
            elif cmd = "showstop" then
                try
                    showstop['nonl']();
                catch:
                    err := lasterror;
                end try;
            elif cmd = "stopwhen" then
                return 'stopwhen'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "stopwhenif" then
                return 'stopwhenif'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "unstopwhen" then
                return 'unstopwhen'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "stoperror" then
                try
                    line := sscanf(original,"%s %1000c");
                    return 'stoperror'(seq(line[i],i=2..nops(line)))
                catch:
                    err := lasterror;
                end try;
            elif cmd = "unstoperror" then
                try
                    line := sscanf(original,"%s %1000c");
                    return 'unstoperror'(seq(line[i],i=2..nops(line)));
                catch:
                    err := lasterror;
                end try;
            elif cmd = "help" or cmd = "?" then
                try
                    help('debugger');
                catch:
                    err := lasterror;
                end try;
            elif cmd = "showerror" then
                return ['debugopts'('lasterror')]
            elif cmd = "showexception" then
                return ['debugopts'('lastexception')]
            elif cmd = "setenv" then
                return 'debugopts'('setenv'=[line[2],line[3]])
            elif cmd = "_skip" then
                skip := true;
                return line;
            elif cmd = "statement" then
                # Must be an expression to evaluate globally.
                original := original[searchtext("statement",original)+9..-1];
                try
                    line := parse(original,'statement',parse_debugger);
                    # *** Avoid returning `line` unevaluated (due to LNED) by
                    # evaluating if line refers to a procedure. Note that the check
                    # for type procedure also evaluates line if it happens to be a
                    # TABLEREF, which can mess up MEMBER binding, so don't check
                    # for type procedure if it is a TABLEREF (i.e. type indexed).
                    if not line :: indexed and line :: procedure then
                        return eval(line);
                    else
                        return line;
                    fi;
                catch:
                    err := lasterror;
                end try
            else
                try
                    # Must be an expression to evaluate.
                    line := parse(original,parse_debugger);
                    # See *** comment in 'cmd = "statement"' case above.
                    if not line :: indexed and line :: procedure then
                        return eval(line);
                    else
                        return line;
                    fi;
                catch:
                    err := lasterror;
                end try;
            fi;

            #}}}
            #{{{ handle error

            if err = lasterror then
                debugger_printf('DBG_PARSE_ERR', "Error, %s\n"
                                , StringTools:-FormatMessage(lastexception[2..])
                               );
            fi;

            #}}}

        od;

        #}}}

    end proc:

#}}}

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
    local i, ls, val;
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
                if type(i,list) then
                    debugger_printf('DBG_INFO', "   %a in procedure %a\n",i[2],i[1])
                elif assigned(`debugger/watch_condition`[i]) then
                    val := sprintf('DBG_INFO', "%a",`debugger/watch_condition`[i]);
                    if length(val) > interface('screenwidth') / 2 then
                        val := cat(val[1..round(interface('screenwidth')/2)]," ...")
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
        # not possible because cond is optional yet is declared as
        # 'uneval'.
        if debugbuiltins and pnam :: 'builtin' then
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

$undef DBG_EVAL1
$undef DBG_EVAL2
$undef DBG_EVAL3
$undef DBG_EVAL4

end module;
