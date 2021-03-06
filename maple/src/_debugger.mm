#LINK mdc.mpl
#LINK Debugger.mm

##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE mdc[Debugger][_debugger]
##HALFLINE replacement for the Maple debugger
##AUTHOR   Joe Riel
##DATE     Apr 2012
##CALLINGSEQUENCE
##- _debugger()

# The debugger proper. This gets invoked after a call to the function debug()
# is encountered.

_debugger := proc( )
option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
description
    `Invoked by Maple when a breakpoint or watchpoint is encountered.`,
    `Not intended to be called directly.`;
local addr, dbg_state, depth, procName, statNumber, evalLevel, i, j, n, line
    , original, prompt, statLevel, state, pName, lNum, cond, cmd, err
    , module_flag, pred, statement, tag;
global showstat, showstop;

    # Note: 21 appears to be the amount that kernelopts('level')
    # increases when entering a procedure.

    evalLevel := kernelopts('level') - 21;

    n := _npassed;
    if n > 0
    and _passed[n] :: list
    and nops(_passed[n]) > 0
    and _passed[n][1] = 'DEBUGINFO' then
        procName := _passed[n][2];   # name of procedure
        statNumber := _passed[n][3]; # state number in procedure
        statLevel := _passed[n][4];  (* state level; a posint, starting at 1,
                                        incremented with each "indentation" level *)
        n := n - 1;

        addr := addressof(procName);

        #{{{ handle skipping

        # skip is a module-local; it's value is retained between calls.

        if skip then
            if go_back then
                if addr = go_back_addr
                and statNumber = go_back_state then
                    go_back := false;
                    skip := false;
                    debugger_printf(TAG_CLEAR_MSG);
                end if;
            elif here_cnt > 0 then
                if here_addr = addr
                and here_state = statNumber then
                    if here_cnt > 1 then
                        here_cnt := here_cnt-1;
                    else
                        skip := false;
                        debugger_printf(TAG_CLEAR_MSG);
                        here_cnt := 0;
                    end if;
                end if;
            elif enter_procname <> NULL then
                if SearchText(enter_procname
                              , sprintf("%a",procName)
                             ) <> 0 then
                    skip := false;
                    enter_procname := NULL;
                    debugger_printf(TAG_CLEAR_MSG);
                end if;
            elif skip_before <> NULL then
                if skip_before_halt then
                    dbg_state := debugopts('procdump'=[procName, 0..abs(statNumber)]);
                    if SearchText(skip_before, dbg_state, SearchText("\n",dbg_state)+1..-2) <> 0 then
                        skip := false;
                        skip_before_halt := false;
                        debugger_printf(TAG_CLEAR_MSG);
                    end if;
                else
                    skip_before_halt := true;
                end if;
            else
                pred := match_predicate[procName,statNumber](_passed[1..n]);
                if pred <> false then
                    skip := false;
                    if SkipIndicateMatch then
                        if pred = true then
                            debugger_printf(TAG_WARN, "skip predicate satisfied\n");
                        else
                            debugger_printf(TAG_WARN, "skip predicate satisfied: %Q\n", pred);
                        end if;
                    end if;
                end if;
                if SkipCheckStack then
                    if skip and evalLevel > last_evalLevel+5 then
                        skip := not match_predicate(op([7,1..-1], debugopts('callstack')));
                    end if;
                    last_evalLevel := evalLevel;
                end if;
            end if;

        end if;

        #}}}
    else
        # Joe asks: Is this branch ever executed?
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
    #{{{ send result to server

    if not skip and (Quiet implies Respond ) then
        Respond := false;
        if monitor_result then
            tag := TAG_MONITOR
        else
            tag := TAG_EVAL
        end if;
        for i from 1 to n do

            if print_to_maple_flag then
                print(_passed[i]);
            end if;

            # Use addressof to prevent an object from overriding
            # equality.
            if addressof(_passed[i]) = addressof(lasterror) then
                local last_exception := lastexception[2..-1];
                if last_exception :: indexed then
                    last_exception := "use showexception (x) or showerror (X)";
                end if;
                debugger_printf(TAG_ERROR, "Error, %s\n", last_exception);
            elif _passed[i] :: list and nops(_passed[i]) >= 1 then
                if _passed[i][1] = 'DEBUGSTACK' then
                    j := nops(_passed[i]) - 5; # this skips the last three, which is always toplevel
                    while j > 1 do
                        if _passed[i][j+1] = `` then
                            debugger_printf(TAG_STACK
                                            , "<%d>\n%a\n"
                                            , addressof(_passed[i][j]) # proc address of called procedure
                                            , _passed[i][j]            # name of called procedure
                                           );
                        else
                            debugger_printf(TAG_STACK
                                            , "<%d>\n%a: %s\n<<%d>>%a\n"
                                            , addressof(_passed[i][j]) # proc address of called procedure
                                            , _passed[i][j]            # name of called procedure
                                            , _passed[i][j+1]          # current statement in proc
                                            , addressof(_passed[i][j+2][]) # address of args
                                            , _passed[i][j+2]          # list of arguments
                                           );
                        fi;
                        j := j - 3
                    end do;
                elif _passed[i][1] = 'DEBUGERROR' then
                    debugger_printf(TAG_ERROR, "Error, %Q\n",op(_passed[i][2..-1]))
                elif _passed[i][1] = 'DEBUGWATCH' then
                    if assigned(`debugger/watch_condition`[_passed[i][2]])
                    and [`debugger/watch_condition`[_passed[i][2]]] <> [op(_passed[i][3..-1])]
                    then
                        return
                    fi;
                    debugger_printf(TAG_WATCHED, "%a := %q\n",_passed[i][2],op(_passed[i][3..-1]))
                elif i < n then
                    # list/set that is part of a continued sequence
                    debugger_printf(tag, "%a,\n",_passed[i])
                else
                    # list/set
                    debugger_printf(tag, "%a\n",_passed[i])
                fi
            elif i < n then
                # expr that is part of a continued sequence
                debugger_printf(tag, "%a,\n",_passed[i])
            else
                # expr
                debugger_printf(tag, "%a\n",_passed[i])
            fi
        end do;

    end if;

    #}}}

    #{{{ send debug status to server

    if procName <> 0 then
        if statNumber = 0 then
            debugger_printf(TAG_WARN, ( "Warning, cannot determine statement number; "
                                        "procedure may have changed in-place\n" ));
            dbg_state := nprintf("%a:\n   0  (* unknown *)\n", procName);
            module_flag := false;
        else
            if statNumber < 0 then
                if not skip then
                    # handle negative statement number (indicates multiple targets)
                    debugger_printf(TAG_WARN, "Warning, statement number may be incorrect\n");
                end if;
                statNumber := -statNumber
            end if;
            dbg_state := debugopts('procdump'=[procName, 0..statNumber]);
            # Set module_flag true if next statement appears to
            # evaluate to a module, which causes a debugger error if one
            # attempts to step into it.  The test is simple and uses
            # builtins to keep this fast.
            if SearchText("module ()", dbg_state) = 0 then
                module_flag := false;
            else
                module_flag := true;
            end if;
        end if;

        if not skip then
            state := sprintf("<%d>\n%A", addr, dbg_state);
            try
                depth := iquo(numelems(debugopts(':-callstack'))-7,3);
            catch:
                # There are weird situations where the above call
                # raises an error; I don't know why and didn't record
                # what the error is.  This is a hack to partially
                # workaround it by assigning some value to depth.
                depth := last_depth;
            end try;
            if state = last_state and depth = last_depth then
                # WriteTagf(TAG_SAME);
            else
                last_state := state;
                last_depth := depth;
                local src_pos := LineInfo:-Get(addr, statNumber);
                if src_pos = NULL
                or src_pos[1] = 0
                or src_pos[2] = -1 then
                    debugger_printf(TAG_STATE, "0 0 0 %d 0:%s"
                                    , depth
                                    , state
                                   );
                else
                    debugger_printf(TAG_STATE, "%s %d %d %d %d%s:%s"
                                    , src_pos # filename, lineno, charbeg, charend
                                    , depth
                                    , LineInfo:-Breakpoints(addr)
                                    , state
                                   );
                end if;
            end if;
        end if;
    end if;

    #}}}

    #{{{ handle skip/monitor

    # skip is true if 'skip_until' is in effect.
    if skip then
        if module_flag then
            debugopts('steplevel'=evalLevel+6);
        else
            debugopts('steplevel'=999999999);
        end if;
        return NULL;
    elif monitoring then
        if monitor_result = true then
            monitor_result := false;
        else
            addr := addressof(procName);
            if assigned(monitor_expr[0])
            or assigned(monitor_expr[addr]) then
                monitor_result := true;
                # parse the statement in the debugger context
                # and send to Maple
                line := NULL;
                if assigned(monitor_expr[0]) then
                    line := (line,parse(monitor_expr[0], parse_debugger));
                end if;
                if assigned(monitor_expr[addr]) then
                    line := (line, parse(monitor_expr[addr], parse_debugger));
                end if;
                if line = NULL then
                    line := 'NULL';
                end if;
                return line;
            end if;
        end if;
    end if;

    #}}}
    #{{{ command loop

    prompt := true;

    do
        line := debugger_readline( prompt );
        if line[1] = "!" then
            Respond := true;
            line := line[2..];
        else
            Respond := false;
        end if;
        # If there's an assignment, make sure it is delimited by spaces.
        i := SearchText(":=",line);
        if i > 1 and SearchText(" := ",line) <> i-1 then
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
                    cmd := "";
                fi
            fi
        fi;
        err := NULL;

        #{{{ parse cmd

        try

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
            elif cmd = "quit" or cmd = "done" or cmd = "stop" then
                debugopts('interrupt'=true)
            elif cmd = "where" then
                if nops(line) = 1 then
                    return 'debugopts(callstack)'
                else
                    # this form is not documented in ?debugopts.
                    # line[2] should be an integer.
                    return 'debugopts'('callstack'=line[2]);
                fi
            elif cmd = "showstack" then
                n := debugopts('callstack');
                n := [op(1,n),op(5..-1,n)];
                n := subsop(seq(3*i=``,i=1..(nops(n)+1)/3),n);
                return n;
            elif cmd = "stopat" then
                #{{{ stopat

                if nops(line) = 4 then
                    cond := parse(line[4],parse_debugger);
                    line := [line[1],line[2],line[3],cond];
                fi;
                pName := procName;
                lNum := 1;
                cond := NULL;
                for i from 2 to nops(line) do
                    if line[i] :: name then
                        pName := line[i];
                    elif line[i] :: '{integer,list(integer)}' then
                        lNum := line[i];
                    else
                        cond := line[i];
                    fi
                od;
                # Set the breakpoint(s); lNum is either an integer or list of integer.
                if nops(line) > 1 then
                    for n in lNum do
                        debugopts('stopat'=[pName,n,cond])
                    od;
                fi;
                return [];

                #}}}
            elif cmd = "unstopat" then
                #{{{ unstopat
                pName := procName;
                lNum := NULL;
                for i from 2 to nops(line) do
                    if line[i] :: name then pName := line[i]
                    else lNum := line[i]
                    fi
                od;
                unstopat(pName,lNum);
                if err <> lasterror then return err fi
                #}}}
            elif cmd = "showstat" or cmd = "list" then
                #{{{ showstat

                if procName = 0 then
                    debugger_printf(TAG_WARN,"Error, not currently in a procedure\n");
                elif nops(line) = 1 and cmd = "list" then
                    i := statNumber - 5;
                    if i < 1 then i := 1 fi;
                    showstat(procName,i..statNumber+1);
                else
                    pName := procName;
                    lNum := NULL;
                    for i from 2 to nops(line) do
                        if line[i] :: name then pName := line[i]
                        else lNum := line[i]
                        fi
                    od;
                    showstat(pName,lNum);
                fi

                #}}}
            elif cmd = "showstop" then
                showstop['nonl']();
            elif cmd = "stopwhen" then
                return 'stopwhen'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "stopwhenif" then
                return 'stopwhenif'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "unstopwhen" then
                return 'unstopwhen'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "stoperror" then
                #{{{ stoperror
                line := sscanf(original,"%s %1000c");
                return 'stoperror'(seq(line[i],i=2..nops(line)))
                #}}}
            elif cmd = "unstoperror" then
                #{{{ unstoperror
                line := sscanf(original,"%s %1000c");
                return 'unstoperror'(seq(line[i],i=2..nops(line)));
                #}}}
            elif cmd = "help" or cmd = "?" then
                help('debugger');
            elif cmd = "showerror" then
                return ['debugopts'('lasterror')]
            elif cmd = "showexception" then
                return ['debugopts'('lastexception')]
            elif cmd = "setenv" then
                return 'debugopts'('setenv'=[line[2],line[3]])
            elif cmd = "_mds_skip" then
                #{{{ _skip
                go_back_addr := addr;
                go_back_state := statNumber;
                skip := true;
                return line;
                #}}}
            elif cmd = "_mds_here" then
                #{{{ _here
                line := sscanf(original, "%s %d %d %d");
                here_cnt := line[2];
                here_addr := line[3];
                here_state := line[4];
                if here_state = statNumber then
                    here_cnt := here_cnt+1;
                end if;
                go_back_addr := addr;
                go_back_state := statNumber;
                skip := true;
                return 'NULL';
                #}}}
            elif cmd = "_mds_enter" then
                #{{{ _mds_enter

                line := sscanf(original, "%s %s");
                enter_procname := line[2];
                go_back_addr := addr;
                go_back_state := statNumber;
                skip := true;
                return 'NULL';

                #}}}
            elif cmd = "_mds_goback_save" then
                #{{{ _goback_save
                line := sscanf(original, "%s %d %d");
                go_back_state := line[2];
                go_back_addr := line[3];
                return 'NULL'
                #}}}
            elif cmd = "_mds_monitor" then
                #{{{ _monitor

                line := sscanf(original, "%s %s %d %1000c");
                cmd := line[2];
                if cmd = "toggle" then
                    monitoring := not monitoring;
                    return `if`(monitoring
                                , "monitoring enabled"
                                , "monitoring disabled"
                               );
                elif cmd = "define" then
                    addr := line[3];
                    if numelems(line) = 3 then
                        monitor_expr[addr] := evaln(monitor_expr[addr]);
                        return 'NULL'
                    else
                        monitor_expr[addr] := line[4];
                        line[4];
                    end if;
                else
                    return 'NULL';
                end if;

                #}}}
            elif cmd = "_mds_request" then
                #{{{ _mds_request
                local expr,val;
                line := sscanf(original, "%s %s %s");
                expr := line[2];
                val  := traperror(eval(parse(expr)));
                if nops(line) = 3 and line[3] = "unlimited" then
                    debugger_printf(TAG_UNLIMITED, "%Q\n", val);
                else
                    debugger_printf(TAG_RESULT, "%Q\n", val);
                end if;
                prompt := false;
                next;
                #}}}
            elif cmd = "_mds_unlimited" then
                #{{{ _mds_unlimited
                unlimited_flag := true;
                line := original[16..-1];
                original := line;
                cmd := op(traperror(sscanf(line,"%s")));
                #}}}
            elif cmd = "inspect" then
                #{{{ inspect

                local index := NULL;
                if nops(line) = 1 then
                    n := debugopts('inspect'=[debugopts('calldepth')-1]);
                elif nops(line) = 2 or line[3] :: '{integer, integer..integer}' then
                    n := debugopts('inspect'=[line[2]]);
                else
                    local baseName := line[3];
                    if baseName :: indexed then
                        index := op(baseName);
                        baseName := op(0,baseName);
                    fi;
                    n := debugopts('inspect' = [line[2], baseName]);
                fi;
                if nops(line) <= 2 then
                    # inspect stackLevel
                    # n is [procname, statnum]
                    i := n[2] - 5;
                    if i < 1 then i := 1 fi;
                    # showsource['nonl','nowarn'](n[1],i..n[2]+1)
                    showstat(n[1],i..n[2]+1);
                elif line[3] :: {integer, integer..integer} then
                    # inspect stackLevel lineNum [.. lineNum]
                    # showsource['nonl','nowarn'](n[1],line[3]);
                    showstat(n[1],line[3]);
                    # n is [procname, statnum]
                else
                    # inspect stackLevel varName
                    # n is [0, paramValue] or [1, localInstance]
                    if nops(n) = 1 then
                        return 'NULL';
                    elif n[1] = 0 then
                        if nops(n) > 2 then
                            if index <> NULL then
                                return n[2..][index];
                            else
                                return op(2..,n);
                            fi;
                        else
                            if index <> NULL then
                                return n[2][index];
                            else
                                return n[2];
                            fi;
                        fi;
                    elif n[1] = 1 then
                        # return value of parameter/local, possibly with index
                        if index <> NULL then
                            return eval(n[2][index]);
                        else
                            return eval(n[2]);
                        fi;
                    elif n[1] = 2 then
                        debugger_printf(TAG_RESULT, "%A\n",n[2]);
                    fi;
                fi;

                #}}}
            end if;

            if cmd = "statement" then
                original := original[11..-1];
                statement := ':-statement';
            else
                statement := NULL;
            end if;

            line := parse(original,statement,parse_debugger);
            # *** Avoid returning `line` unevaluated (due to LNED) by
            # evaluating if line refers to a procedure. Note that the check
            # for type procedure also evaluates line if it happens to be a
            # TABLEREF, which can mess up MEMBER binding, so don't check
            # for type procedure if it is a TABLEREF (i.e. type indexed).
            if not line :: indexed and line :: procedure then
                return eval(line);
            elif line = NULL then
                return 'NULL';
            else
                return line;
            end if;
        catch:
            err := lasterror;
        end try;

        #}}}

        #{{{ handle error

        if err = lasterror then
            debugger_printf(TAG_ERROR, "Error, %s\n"
                            , StringTools:-FormatMessage(lastexception[2..-1])
                           );
        fi;

        #}}}

        prompt := true;

    end do;

    #}}}

end proc:

