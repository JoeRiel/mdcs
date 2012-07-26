##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE SUBMOD Debugger
##DEFINE PROC _debugger
##PROCEDURE \PKG[\SUBMOD][\PROC]
##HALFLINE replacement for the Maple debugger
##AUTHOR   Joe Riel
##DATE     Apr 2012
##CALLINGSEQUENCE
##- \PROC()
##DESCRIPTION
##- blah blah
##
##SUBSECTION Tags
##- CLEAR_ECHO : clear message in echo area
##- DBG_ERR : A debugger error occurred
##- DBG_EVAL : computed result (user or procedure)
##- DBG_INFO : output from "showstop"
##- DBG_SAME : state has not changed
##- DBG_STACK : short stack output (can be merged with DBG_WHERE)
##- DBG_STATE : debugger statement
##- DBG_STOP : never sent
##- DBG_WARN : used to indicate improper action (but also when skip is satisfied)
##- DBG_WHERE : long stack output
##- LINE_INFO : sending line info (filename lineno beg end:...)
##- MONITOR : message is result of a monitored expression
##- MDC_PRINTF : pretty-printed output
##- DBG_ERR : a Maple error occurred
##- SHOW_EXCEPTION : called from "ShowException"
##- WATCHED_CONDS: reports watches
##- WATCHED_ERRS : called from "_showstop"
##ENDSUBSECTION

# The debugger proper. This gets invoked after a call to the function debug()
# is encountered.

_debugger := proc( )
option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
description
    `Invoked by Maple when a breakpoint or watchpoint is encountered.`,
    `Not intended to be called directly.`;
local addr, dbg_state, procName, statNumber, evalLevel, i, j, n, line
    , original, prompt, statLevel, state, pName, lNum, cond, cmd, err
    , module_flag, pred, tag;
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

        #{{{ handle go_back/here/enter_procname/match_predicate

        if skip then

            if go_back then
                if procName = go_back_proc
                and statNumber = go_back_state then
                    go_back := false;
                    skip := false;
                    debugger_printf(TAG_CLEAR_MSG);
                end if;
            elif here_cnt > 0 then
                if here_proc = procName
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
                if statNumber = 1
                and SearchText(enter_procname
                               , sprintf("%a",procName)
                               #, -length(enter_procname)..-1
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
                    if pred = true then
                        debugger_printf(TAG_WARN, "skip predicate satisfied\n");
                    else
                        debugger_printf(TAG_WARN, "skip predicate satisfied: %Q\n", pred);
                    end if;
                end if;
                if SkipCheckStack then
                    if skip and evalLevel > last_evalLevel+5 then
                        skip := not match_predicate(op([7,..], debugopts('callstack')));
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
    #{{{ send result to emacs


    if not skip and ( Quiet implies Respond ) then
        Respond := false;
        if monitor_result then
            tag := TAG_MONITOR
        else
            tag := TAG_EVAL
        end if;
        for i from 1 to n do
            # Use addressof to prevent an object from overriding
            # equality.
            if addressof(_passed[i]) = addressof(lasterror) then
                debugger_printf(TAG_ERROR, "Error, %s\n"
                                , StringTools:-FormatMessage(lastexception[2..]))
            elif _passed[i] :: list and nops(_passed[i]) >= 1 then
                if _passed[i][1] = 'DEBUGSTACK' then
                    j := nops(_passed[i]) - 2;
                    while j > 2 do
                        if _passed[i][j+1] = `` then
                            debugger_printf(TAG_STACK
                                            , "<%d>\n%a\n"
                                            , addressof(_passed[i][j])
                                            , _passed[i][j]
                                           );
                        else
                            debugger_printf(TAG_WHERE
                                            , "<%d>\n%a: %s\n"
                                            , addressof(_passed[i][j])
                                            , _passed[i][j]
                                            , _passed[i][j+1]
                                           );
                        fi;
                        j := j - 3
                    od
                elif _passed[i][1] = 'DEBUGERROR' then
                    debugger_printf(TAG_ERROR, "Error, %Q\n",op(_passed[i][2..-1]))
                elif _passed[i][1] = 'DEBUGWATCH' then
                    if assigned(`debugger/watch_condition`[_passed[i][2]])
                    and [`debugger/watch_condition`[_passed[i][2]]] <> [op(_passed[i][3..-1])]
                    then
                        return
                    fi;
                    debugger_printf('WATCHED_CONDS', "%a := %q\n",_passed[i][2],op(_passed[i][3..-1]))
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

    #{{{ send debug status to Emacs

    if procName <> 0 then
        if statNumber < 0 then
            if not skip then
                # handle negative statement number (indicates multiple targets)
                debugger_printf(TAG_WARN, "Warning, statement number may be incorrect\n");
            end if;
            statNumber := -statNumber
        end if;

        dbg_state := debugopts('procdump'=[procName, 0..statNumber]);
        # Set module_flag true if next statement appears to
        # evaluate a module, which causes a debugger error if one
        # attempts to step into it.  The test is simple and uses
        # builtins to keep this fast.
        if SearchText("module ()", dbg_state) = 0 then
            module_flag := false;
        else
            module_flag := true;
        end if;
        if not skip then
            addr := addressof(procName);
            state := sprintf("<%d>\n%A", addr, dbg_state);
            if state = last_state then
                WriteTagf(TAG_SAME);
            else
                last_state := state;
                local src_pos := LineInfo:-Get(addr, statNumber);
                if src_pos = NULL
                or src_pos[1] = 0 then
                    debugger_printf(TAG_STATE, "%s", state);
                else
                    debugger_printf(TAG_LINE_INFO, "%s %d %d %d%s:%s"
                                    , src_pos                     # file, lineno, beg, end
                                    , LineInfo:-Breakpoints(addr) # breakpoints, as a string
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
        line := `debugger/readline`( prompt );
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
            n := subsop(seq(3*i=``,i=1..(nops(n)+1)/3),n);
            # n := subsop(op(map(`=`,[seq(i*3,i=1..(nops(n)+1)/3)],``)),n);
            return n;
        elif cmd = "stopat" then
            #{{{ stopat

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
                        if line[i] :: name then pName := line[i]
                        elif line[i] ::  '{integer,list(integer)}' then
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
            try
                unstopat(pName,lNum);
            catch:
                err := lasterror;
            end try;
            if err <> lasterror then return err fi
            #}}}
        elif cmd = "showstat" or cmd = "list" then
            #{{{ showstat

            if procName = 0 then
                debugger_printf(TAG_WARN,"Error, not currently in a procedure\n");
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
                    if line[i] :: name then pName := line[i]
                    else lNum := line[i]
                    fi
                od;
                try
                    showstat['nonl'](pName,lNum);
                catch:
                    err := lasterror;
                end try;
            fi

            #}}}
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
            #{{{ stoperror
            try
                line := sscanf(original,"%s %1000c");
                return 'stoperror'(seq(line[i],i=2..nops(line)))
            catch:
                err := lasterror;
            end try;
            #}}}
        elif cmd = "unstoperror" then
            #{{{ unstoperror
            try
                line := sscanf(original,"%s %1000c");
                return 'unstoperror'(seq(line[i],i=2..nops(line)));
            catch:
                err := lasterror;
            end try;
            #}}}
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
            #{{{ _skip
            go_back_proc := procName;
            go_back_state := statNumber;
            skip := true;
            return line;
            #}}}
        elif cmd = "_here" then
            #{{{ _here
            line := sscanf(original, "%s %d %d %d");
            here_cnt := line[2];
            here_proc := pointto(line[3]);
            here_state := line[4];
            if here_state = statNumber then
                here_cnt := here_cnt+1;
            end if;
            go_back_proc := procName;
            go_back_state := statNumber;
            skip := true;
            return 'NULL';
            #}}}
        elif cmd = "_enter" then
            #{{{ _enter
            line := sscanf(original, "%s %s");
            enter_procname := line[2];
            go_back_proc := procName;
            go_back_state := statNumber;
            skip := true;
            return 'NULL';
            #}}}
        elif cmd = "_goback_save" then
            #{{{ _goback_save
            line := sscanf(original, "%s %d %d");
            go_back_state := line[2];
            # go_back_proc := procName;
            go_back_proc := pointto(line[3]);
            return 'NULL'
            #}}}
        elif cmd = "_monitor" then
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
            line := sscanf(original, "%s %s");
            expr := line[2];
            val  := traperror(eval(parse(expr)));
            debugger_printf(TAG_RESULT, "%q\n", val);
            prompt := false;
            next;
            #}}}
        elif cmd = "statement" then
            #{{{ statement
            # Must be an expression to evaluate globally.
            original := original[SearchText("statement",original)+9..-1];
            try
                line := parse(original,'statement',parse_debugger);
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
                fi;
            catch:
                err := lasterror;
            end try
            #}}}
        else
            #{{{ expression
            try
                Respond := true;
                # Must be an expression to evaluate.
                line := parse(original,parse_debugger);
                # See *** comment in 'cmd = "statement"' case above.
                if not line :: indexed and line :: procedure then
                    return eval(line);
                elif line = NULL then
                    return 'NULL';
                else
                    return line;
                fi;
                # catch "invalid expression":
                #     try
                #         # this will generate a warning if saving
                #         # locals.  But warning does not go to debugger
                #         # output,  it shows up in tty stream.
                #         parse(original,'statement',parse_debugger);
                #     catch:
                #         err := lasterror;
                #     end try
            catch:
                err := lasterror;
            end try;
            #}}}
        fi;

        #}}}
        #{{{ handle error

        if err = lasterror then
            debugger_printf(TAG_ERROR, "Error, %s\n"
                            , StringTools:-FormatMessage(lastexception[2..])
                           );
        fi;

        #}}}

    end do;

    #}}}

end proc:

