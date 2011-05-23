# -*- mode:mpldoc -*-

##DEFINE MOD EmacsDebugger
##MODULE \MOD
##HALFLINE appliable module for communicating with the Emacs Maple debugger
##AUTHOR   Joe Riel
##DATE     May 2011
##CALLINGSEQUENCE
##- \MOD()

$define DEBUGGER_PROCS debugger, `debugger/printf`, `debugger/readline`

unprotect('EmacsDebugger'):

module EmacsDebugger()

global DEBUGGER_PROCS;

export ModuleApply;

# Using fixed pipes prevents multiple instances.
# Need to assign them automatically.

local ModuleLoad
    , ModuleUnload
    , Procs := 'DEBUGGER_PROCS'
    , _debugger
    , debugger_printf
    , debugger_readline
    , printf_to_emacs
    , replaceProcs
    , restoreProcs
    , sid
    , EmacsDebuggerPort := 10\000
    ;

#{{{ ModuleLoad

    ModuleLoad := proc()
        replaceProcs();
        sid := Sockets:-Open("localhost", EmacsDebuggerPort);
        return NULL;
    end proc;

#}}}
#{{{ ModuleUnload

    ModuleUnload := proc()
        Sockets:-Close( sid );
        # hmm, maybe not.  Check if already done.
        restoreProcs();
    end proc;

#}}}
#{{{ ModuleApply

    ModuleApply := proc( load :: truefalse := true
                         , $)
        if not load then
            restoreProcs();
        end if;
    end proc;

#}}}

#{{{ restoreProcs

    restoreProcs := proc()
        map( p -> kernelopts('unread' = p), [Procs] );
        return NULL;
    end proc;

#}}}
#{{{ replaceProcs

    replaceProcs := proc()
        # Reassign library debugger procedures
        unprotect(Procs);
        debugger := eval(_debugger);
        `debugger/printf` := eval(debugger_printf);
        `debugger/readline` := eval(debugger_readline);
        protect(Procs);

        return NULL;

    end proc;

#}}}

#{{{ debugger_printf

    debugger_printf := proc( )

    local argList, rts;
    description `Used by debugger to produce output.`;
        argList := [_passed];

        # suppress large rtables
        rts := map(x->x=`debugger/describe_rtable`(x)
                   , indets(argList,'debugger_large_rtable')
                  );
        if rts <> {} then argList := subs(rts,argList) end if;

        # fprintf('INTERFACE_DEBUG',op(argList));
        printf_to_emacs(op(argList));

        return NULL;
    end proc:

#}}}
#{{{ debugger_readline

# Used for user input to the debugger. This lets us easily change the input
# facilities to take advantage of special features of the Iris in future.

    debugger_readline := proc( )
    option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
    description `Used by debugger to obtain input.`;
    local len, res, startp, endp, i, endcolon;
    global `debugger/default`;

        `debugger/printf`("\n");
        do
            #res := traperror(readline(-2));
            #res := traperror(readline(pipe_to_maple));
            res := traperror(Sockets:-Read(sid));
            if res <> lasterror then break fi;
            printf("Error, %s\n",StringTools:-FormatMessage(lastexception[2..]))
        od;

        # If the user just pressed ENTER, use the value of the variable
        # `debugger/default`, which contains whatever debugger command the user
        # issued last time. If there is no previous command, return a command to
        # print a message pointing the user to the on-line help.
        if `debugger/isspace`( res ) then
            res := `if`(assigned(`debugger/default`),
                        `debugger/default`,
                        "`debugger/printf`(""See ?debugger for available commands\n"")")
        else
            `debugger/default` := res;
        fi;

        # Remove leading blanks, trailing comments, and colons.
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
        od;

        # Record whether or not the command ended in a colon.
        if _npassed > 0 and type(_passed[1],name) then
            assign(_passed[1],endcolon)
        fi;

        # Complain if there were any non-blank, non-comment characters after the
        # end of the typed command.
        if i < len and res[i] <> "#" then
            for i from i to len do
                if i <> " " then
                    `debugger/printf`("Warning, extra characters at end of parsed string\n");
                    break
                fi
            od
        fi;

        # Strip trailing whitespace.
        while endp >= startp and res[endp] <= " " do endp := endp -1 od;
        res := res[startp..endp];

        # Means the user typed something which still ended up being nothing, such
        # as a single semicolon, just a comment, etc.
        if res = "" then
            res := "`debugger/printf`(""See ?debugger for available commands\n"")"
        fi;

        res
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
    local procName, statNumber, evalLevel, i, j, n, line, original, statLevel,
        pName, lNum, cond, cmd, err;
    global showstat, showstop, `debugger/no_output`;

        evalLevel := kernelopts('level') - 21;
        n := _npassed;
        if n > 0 and type(_passed[n],list) and nops(_passed[n]) > 0
        and _passed[n][1] = 'DEBUGINFO' then
            procName := _passed[n][2];
            statNumber := _passed[n][3];
            statLevel := _passed[n][4];
            n := n - 1
        else
            procName := 0;
            statLevel := trunc(evalLevel / 5); # Approximately #
        fi;

# Joe Riel: remove optional indices, they cannot be used in the name.
        while procName :: 'And(indexed,Not(procedure))' do
            procName := op(0,procName);
        end do;

        for i from 1 to n do
            if _passed[i] = lasterror then
                `debugger/printf`("Error, %s\n",
                                  StringTools:-FormatMessage(lastexception[2..]))
            elif type(_passed[i],list) and nops(_passed[i]) >= 1 then
                if _passed[i][1] = 'DEBUGSTACK' then
                    j := nops(_passed[i]) - 2;
                    while j > 2 do
                        if _passed[i][j+1] = `` then
                            `debugger/printf`("%a\n",_passed[i][j])
                        else
                            `debugger/printf`("%a: %s\n",_passed[i][j],_passed[i][j+1]);
                            if `debugger/no_output` <> true then
                                `debugger/printf`("\t%a\n",_passed[i][j-1])
                            fi
                        fi;
                        j := j - 3
                    od
                elif _passed[i][1] = 'DEBUGERROR' then
                    `debugger/printf`("Error, %Q\n",op(_passed[i][2..-1]))
                elif _passed[i][1] = 'DEBUGWATCH' then
                    if assigned(`debugger/watch_condition`[_passed[i][2]])
                    and [`debugger/watch_condition`[_passed[i][2]]] <> [op(_passed[i][3..-1])]
                    then
                        return
                    fi;
                    `debugger/printf`("%a := %q\n",_passed[i][2],op(_passed[i][3..-1]))
                elif `debugger/no_output` <> true then
                    if i < n then
                        `debugger/printf`("%a,\n",_passed[i])
                    else
                        `debugger/printf`("%a\n",_passed[i])
                    fi
                fi
            elif `debugger/no_output` <> true then
                if i < n then
                    `debugger/printf`("%a,\n",_passed[i])
                else
                    `debugger/printf`("%a\n",_passed[i])
                fi
            fi
        od;
        if procName <> 0 then
            if statNumber < 0 then
                `debugger/printf`("Warning, statement number may be incorrect\n");
                statNumber := -statNumber
            fi;
            `debugger/printf`("%s",debugopts('procdump'=[procName,0..statNumber]))
        fi;
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
                        line := [cmd,sprintf("%a",line[1]),traperror(parse(line[2],'debugger'))];
                        cmd := "setenv"
                    else
                        original := sprintf("assign('%a',%a)",line[1],
                                            traperror(parse(line[2],'debugger')));
                        cmd := ""
                    fi
                fi
            fi;
            err := NULL;
            if cmd = "cont" then
                return
            elif cmd = "next" then
                debugopts('steplevel'=evalLevel);
                return
            elif cmd = "step" then
                debugopts('steplevel'=999999999);
                return
            elif cmd = "into" then
                debugopts('steplevel'=evalLevel+6);
                return
            elif cmd = "outfrom" then
                debugopts('steplevel'=evalLevel-2);
                return
            elif cmd = "return" then
                debugopts('steplevel'=evalLevel-statLevel*5);
                return
            elif cmd = "quit" or cmd = "done" or cmd = "stop" then
                printf_to_emacs("stopping\n");
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
                    err := traperror(parse(line[4],'debugger'));
                    line := [line[1],line[2],line[3],err]
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
                    if err <> lasterror then return stopat() fi
                fi
            elif cmd = "unstopat" then
                pName := procName;
                lNum := NULL;
                for i from 2 to nops(line) do
                    if type(line[i],name) then pName := line[i]
                    else lNum := line[i]
                    fi
                od;
                err := traperror(unstopat(pName,lNum));
                if err <> lasterror then return err fi
            elif cmd = "showstat" or cmd = "list" then
                if procName = 0 then
                    `debugger/printf`("Error, not currently in a procedure\n");
                elif nops(line) = 1 and cmd = "list" then
                    i := statNumber - 5;
                    if i < 1 then i := 1 fi;
                    err := traperror(showstat['nonl'](procName,i..statNumber+1))
                else
                    pName := procName;
                    lNum := NULL;
                    for i from 2 to nops(line) do
                        if type(line[i],name) then pName := line[i]
                        else lNum := line[i]
                        fi
                    od;
                    err := traperror(showstat['nonl'](pName,lNum))
                fi
            elif cmd = "showstop" then
                err := traperror(showstop['nonl']())
            elif cmd = "stopwhen" then
                return 'stopwhen'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "stopwhenif" then
                return 'stopwhenif'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "unstopwhen" then
                return 'unstopwhen'(`debugger/list`(seq(line[i],i=2..nops(line))))
            elif cmd = "stoperror" then
                line := traperror(sscanf(original,"%s %1000c"));
                return 'stoperror'(seq(line[i],i=2..nops(line)))
            elif cmd = "unstoperror" then
                line := traperror(sscanf(original,"%s %1000c"));
                return 'unstoperror'(seq(line[i],i=2..nops(line)))
            elif cmd = "help" or cmd = "?" then
                err := traperror(help('debugger'))
            elif cmd = "showerror" then
                return ['debugopts'('lasterror')]
            elif cmd = "showexception" then
                return ['debugopts'('lastexception')]
            elif cmd = "setenv" then
                return 'debugopts'('setenv'=[line[2],line[3]])
            elif cmd = "statement" then
                # Must be an expression to evaluate globally.
                original := original[searchtext("statement",original)+9..-1];
                line := traperror(parse(original,'statement','debugger'));
                if line <> lasterror then
                    # *** Avoid returning `line` unevaluated (due to LNED) by
                    # evaluating if line refers to a procedure. Note that the check
                    # for type procedure also evaluates line if it happens to be a
                    # TABLEREF, which can mess up MEMBER binding, so don't check
                    # for type procedure if it is a TABLEREF (i.e. type indexed).
                    if not line :: indexed and line :: procedure then
                        return eval(line)
                    fi;
                    return line
                fi;
                err := line;
            else
                # Must be an expression to evaluate.
                line := traperror(parse(original,'debugger'));
                if line <> lasterror then
                    # See *** comment in 'cmd = "statement"' case above.
                    if not line :: indexed and line :: procedure then
                        return eval(line)
                    fi;
                    return line
                fi;
                err := line;
            fi;
            if err = lasterror then
                `debugger/printf`("Error, %s\n"
                                  , StringTools:-FormatMessage(lastexception[2..])
                                 );
            fi
        od;
    end proc:

#}}}
#{{{ printf_to_emacs

    printf_to_emacs := proc()
    local msg;
        msg := sprintf(_passed);
        Sockets:-Write(sid, msg);
        return NULL;
    end proc;

#}}}

end module:

protect('EmacsDebugger'):

$undef DEBUGGER_PROCS

#{{{ Write to Maple Archive

mla := "EmacsDebugger.mla":
if FileTools:-Exists(mla) then
    fremove(mla);
end if;
LibraryTools:-Save(EmacsDebugger, mla);

#}}}
