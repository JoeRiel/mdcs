# -*- mpldoc -*-
##INCLUDE ../include/mpldoc_macros.mi
##DEFINE MOD mdc
##MODULE \MOD
##HALFLINE appliable module for communicating with a Maple Debugger Server
##AUTHOR   Joe Riel
##DATE     May 2011
##CALLINGSEQUENCE
##- \MOD('opts')
##PARAMETERS
##param_opts(\MOD)
##RETURNS
##- `NULL`
##DESCRIPTION
##- The `\MOD` module is an appliable module
##  that implements a *Maple Debugger Client*.
##  It communicates with a *Maple Debugger Server*.
##OPTIONS
##opt(beep,truefalse)
##  If `true`, emit a tri-tone beep
##  when succesfully connecting.
##  Calls the system function `beep`.
##  The default is `true`.
##opt(config,maplet or string)
##  Specifies a method for configuring the client.
##  If a string, then the configuration is read
##  from that filename.  If the symbol is `maplet`,
##  then a "Maplet" is launched that queries
##  for the configuration.
##  If 'config' is not specified, then all defaults
##  are used.
##opt(connection,socket|pipe|ptty)
##  Specifies the connection type.
##  Currently only `socket` is supported.
##  The default is `socket`.
##opt(host,string)
##  The name of the host.
##  The default is _"localhost"_.
##opt(label,string)
##  Label passed to server for convenient identication
##  of this client.
##  The default is the return value of _kernelopts('username')_.
##opt(maxlength,nonnegint)
##  Limits the length of string the client will transmit.
##  If a string is longer than that, it is replaced
##  with a message indicating the problem and the original length.
##  0 means no limit.
##  The default is 10000.
##opt(port,posint)
##  The port (socket) used when _connection=socket_.
##  The default is 10000.
##opt(timeout,nonnegint)
##  Specifies ...
##opt(view,truefalse)
##  If `true`, then the remote debugging session
##  is echoed on the local machine.
##  The default is originally`false`,
##  but becomes whatever was last used.
##opt(exit,truefalse)
##  If `true`, then
##NOTES


$define DEBUGGER_PROCS debugger, `debugger/printf`, `debugger/readline`, showstat, showstop, where
$define MDS_PORT 10\000
$define END_OF_MSG "---EOM---"

unprotect('mdc'):
module mdc()

global DEBUGGER_PROCS;

export ModuleApply
    ,  Format
    ,  Authenticate
    ;

$include <src/Format.mm>

#{{{ local declarations

local Connect
    , Disconnect
    , ModuleUnload
    , createID
    , debugger_procs := 'DEBUGGER_PROCS' # macro
    , _debugger
    , debugger_printf
    , debugger_readline
    , _showstat
    , _showstop
    , _where

    , max_length := 10\000 # too small?
    , Port := MDS_PORT
    , Host := "localhost"

    , printf_to_server
    , replaced := false
    , replaceProcs
    , restoreProcs
    , sid := NULL
    , view_flag := false
    ;

#}}}

#{{{ Connect

##DEFINE PROC Connect
##PROCEDURE \MOD[\PROC]
##HALFLINE initiate a connection to a Maple debugger server

    Connect := proc(host :: string
                    , port :: posint
                    , id :: string
                    , { beep :: truefalse := true }
                    , { greeting :: string := "" }
                    , $
                   )
        if sid <> NULL then
            Sockets:-Close(sid);
        end if;
        if beep then
            ssystem("beep");
        end if;
        sid := Sockets:-Open(host, port);
        Host := host;
        Port := port;
        if greeting <> "" then
            printf_to_server(GREET, greeting);
        end if;
        return NULL;
    end proc;

#}}}
#{{{ Disconnect

##DEFINE PROC Disconnect
##PROCEDURE \MOD[\PROC]
##HALFLINE terminate connection to Maple debugger server

    Disconnect := proc()
        if sid <> NULL then
            Sockets:-Close(sid);
            sid := NULL;
        end if;
    end proc;

#}}}

#{{{ createID

##DEFINE PROC createID
##PROCEDURE \MOD[\PROC]
##HALFLINE create a formatted ID that identifies the client
##CALLINGSEQUENCE
##- \PROC('label')
##PARAMETERS
##- 'label' : ::string::; user-friendly label
##RETURNS
##- ::string::
##DESCRIPTION
##- The `\PROC` procedure
##  returns a string that can be used by the server
##  to identify this client.
##
##- Currently the format is ~:label:platform:pid:~,
##  where label is the argument, and `platform` and `pid`
##  are the corresponding outputs of "kernelopts".
##
##NOTES
##- The format will probably be expanded once this is in place.
##  Note that a different machine could generate the same ID.
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdc:-createID):
## VerifyTools:-AddVerification(
##      label=proc(s1,s2) evalb( s1 = sprintf("%s%d:",s2,kernelopts('pid')) )
##   end proc):
## macro(LA='verify,label', NLA='verify,Not(label)', TE=testerror);
## Try[LA]("1.1", FUNC("label"), ":label:unix:");
## Try[LA]("1.2", FUNC("abc12"), ":abc12:unix:");
## Try[LA]("1.3", FUNC("abc-1_2_"), ":abc-1_2_:unix:");
## Try[NLA]("2.1", FUNC("abc-1_2_"), ":abc-1_2_:unix:0");
## Try[TE]("10.0", FUNC(""), "label cannot be empty");
## msg := "invalid characters in label":
## Try[TE]("10.1", FUNC("+"), msg);
## Try[TE]("10.2", FUNC(" "), msg);
## Try[TE]("10.3", FUNC("\n"), msg);


    createID := proc(label :: string,$)
        if length(label) = 0 then
            error "label cannot be empty";
        elif not StringTools:-RegMatch("^[A-Za-z0-9_-]+$", label) then
            error "invalid characters in label '%1'", label;
        end if;
        return sprintf(":%s:%s:%d:", label, kernelopts('platform,pid') );
    end proc;

#}}}
#{{{ ModuleUnload

    ModuleUnload := proc()
        try
            Sockets:-Close( sid );
        catch:
        end try;
        restoreProcs();
    end proc;

#}}}
#{{{ ModuleApply

    ModuleApply := proc( (* no positional parameters *)
                         { beep :: truefalse := true }
                         , { config :: {string,identical(maplet)} := NULL }
                         , { connection :: identical(socket,pipe,ptty) := 'socket' }
                         , { greeting :: string := "" }
                         , { host :: string := Host }
                         , { maxlength :: nonnegint := max_length }
                         , { password :: string := "" }
                         , { port :: posint := Port }
                         #, { timeout :: nonnegint := 0 }
                         , { label :: string := kernelopts('username') }
                         , { view :: truefalse := view_flag }
                         , { exit :: truefalse := false }
                         , $ )

    global `debugger/width`;

        if connection <> 'socket' then
            error "currently only a socket connection is supported"
        elif config <> NULL then
            error "currently the 'config' option is disabled.  Use optional parameters."
        end if;

        if exit then
            restoreProcs();
            Disconnect();
            return NULL;
        end if;

        view_flag := view;
        max_length := maxlength;

        if max_length > 0 then
            `debugger/width` := max_length;
        end if;

        replaceProcs();

        try
            Connect(host, port, createID(label), _options['beep','greeting'] );
        catch:
            restoreProcs();
            error;
        end try;

        return NULL;

    end proc;

#}}}

#{{{ replaceProcs

    replaceProcs := proc()
        if not replaced then
            # Reassign library debugger procedures
            unprotect(debugger_procs);
            debugger            := eval(_debugger);
            `debugger/printf`   := eval(debugger_printf);
            `debugger/readline` := eval(debugger_readline);
            showstat            := eval(_showstat);
            showstop            := eval(_showstop);
            where               := eval(_where);
            protect(debugger_procs);
            replaced := true;
        end if;
        return NULL;
    end proc;

#}}}
#{{{ restoreProcs

    restoreProcs := proc()
        # Dave H. suggests using 'forget'
        if replaced then
            map( p -> kernelopts('unread' = p), [debugger_procs] );
            replaced := false;
        end if;
        return NULL;
    end proc;

#}}}

#{{{ debugger_printf

    debugger_printf := proc( tag )
    local argList, rts;
    description `Used by debugger to produce output.`;
        argList := [_rest];

        # suppress large rtables
        rts := map(x->x=`debugger/describe_rtable`(x)
                   , indets(argList,'debugger_large_rtable')
                  );
        if rts <> {} then argList := subs(rts,argList) end if;

        printf_to_server(tag, op(argList));

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

        # `debugger_printf`("\n");

        #{{{ Get response (from emacs)
        do
            #res := traperror(readline(-2));
            #res := traperror(readline(pipe_to_maple));
            res := traperror(Sockets:-Read(sid));
            if res <> lasterror then break fi;
            printf("Error, %s\n",StringTools:-FormatMessage(lastexception[2..]))
        od;
        #}}}
        #{{{ Handle solo enter (repeat previous command)

        # If the user just pressed ENTER, use the value of the variable
        # `debugger/default`, which contains whatever debugger command the user
        # issued last time. If there is no previous command, return a command to
        # print a message pointing the user to the on-line help.
        if `debugger/isspace`( res ) then
            res := `if`(assigned(`debugger/default`)
                        , `debugger/default`
                        , "debugger_printf(DBG_INFO, \"See ?debugger for available commands\n\")"
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
        od;
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
                if i <> " " then
                    debugger_printf(DBG_WARN,"Warning, extra characters at end of parsed string\n");
                    break
                fi
            od
        fi;
        #}}}
        #{{{ Strip trailing whitespace.
        while endp >= startp and res[endp] <= " " do endp := endp -1 od;
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

        #{{{ remove indices in procName
        # Added by Joe Riel
        while procName :: 'And(indexed,Not(procedure))' do
            procName := op(0,procName);
        end do;
        #}}}
        #{{{ process args

        for i from 1 to n do
            if _passed[i] = lasterror then
                debugger_printf(MPL_ERR, "Error, %s\n"
                                , StringTools:-FormatMessage(lastexception[2..]))
            elif type(_passed[i],list) and nops(_passed[i]) >= 1 then
                if _passed[i][1] = 'DEBUGSTACK' then
                    j := nops(_passed[i]) - 2;
                    while j > 2 do
                        if _passed[i][j+1] = `` then
                            debugger_printf(DBG_CALL, "%a\n",_passed[i][j])
                        else
                            debugger_printf(DBG_CALL, "%a: %s\n",_passed[i][j],_passed[i][j+1]);
                            if `debugger/no_output` <> true then
                                debugger_printf(DBG_ARGS,"\t%a\n",_passed[i][j-1])
                            fi
                        fi;
                        j := j - 3
                    od
                elif _passed[i][1] = 'DEBUGERROR' then
                    debugger_printf(DBG_ERR, "Error, %Q\n",op(_passed[i][2..-1]))
                elif _passed[i][1] = 'DEBUGWATCH' then
                    if assigned(`debugger/watch_condition`[_passed[i][2]])
                    and [`debugger/watch_condition`[_passed[i][2]]] <> [op(_passed[i][3..-1])]
                    then
                        return
                    fi;
                    debugger_printf(DBG_WATCH, "%a := %q\n",_passed[i][2],op(_passed[i][3..-1]))
                elif `debugger/no_output` <> true then
                    if i < n then
                        debugger_printf(DBG_C, "%a,\n",_passed[i])
                    else
                        debugger_printf(DBG_C, "%a\n",_passed[i])
                    fi
                fi
            elif `debugger/no_output` <> true then
                if i < n then
                    debugger_printf(DBG_EVAL, "%a,\n",_passed[i])
                else
                    debugger_printf(DBG_EVAL, "%a\n",_passed[i])
                fi
            fi
        od;

        #}}}
        #{{{ handle negative statement number
        if procName <> 0 then
            if statNumber < 0 then
                debugger_printf(DBG_WARN, "Warning, statement number may be incorrect\n");
                statNumber := -statNumber
            fi;
            debugger_printf(DBG_STATE,"%s",debugopts('procdump'=[procName,0..statNumber]))
        fi;
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

            #{{{ parse cmd (else is arbitrary expression)

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
                printf_to_server("stopping\n");
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
                    debugger_printf(DBG_WARN,"Error, not currently in a procedure\n");
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
                    err := traperror(showstat['nonl'](pName,lNum));
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

            #}}}
            #{{{ handle error

            if err = lasterror then
                debugger_printf(DBG_ERR, "Error, %s\n"
                                  , StringTools:-FormatMessage(lastexception[2..])
                                 );
            fi

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

            map[3](debugger_printf, DBG_SHOW, "\n%s", [res]);
            if procname <> 'showstat[nonl]' then
                debugger_printf(DBG_NULL, "\n" )
            fi
        fi;
        NULL
    end proc:

#}}}
#{{{ showstop

    _showstop := proc( $ )
    option `Copyright (c) 1994 by Waterloo Maple Inc. All rights reserved.`;
    description `Display a summary of all break points and watch points.`;
    local i, ls, val;
    global showstop;
        ls := stopat();
        if nops(ls) = 0 then debugger_printf(DBG_INFO, "\nNo breakpoints set.\n")
        else
            debugger_printf(DBG_INFO, "\nBreakpoints in:\n");
            for i in ls do debugger_printf(DBG_INFO, "   %a\n",i) od
        fi;
        ls := stopwhen();
        if nops(ls) = 0 then debugger_printf(DBG_INFO, "\nNo variables being watched.\n")
        else
            debugger_printf(DBG_INFO, "\nWatched variables:\n");
            for i in ls do
                if type(i,list) then
                    debugger_printf(DBG_INFO, "   %a in procedure %a\n",i[2],i[1])
                elif assigned(`debugger/watch_condition`[i]) then
                    val := sprintf(DBG_INFO, "%a",`debugger/watch_condition`[i]);
                    if length(val) > interface('screenwidth') / 2 then
                        val := cat(val[1..round(interface('screenwidth')/2)]," ...")
                    fi;
                    debugger_printf(DBG_INFO, "   %a = %s\n",i,val)
                else
                    debugger_printf(DBG_INFO, "   %a\n",i)
                fi
            od
        fi;
        ls := stoperror();
        if nops(ls) = 0 then debugger_printf(DBG_INFO, "\nNo errors being watched.\n")
        else
            debugger_printf(DBG_WATCH, "\nWatched errors:\n");
            if member('all',ls) then
                if member('traperror',ls) then
                    debugger_printf(DBG_INFO, "   All errors\n")
                else
                    debugger_printf(DBG_INFO, "   All untrapped errors\n")
                fi
            else
                if member('traperror',ls) then
                    debugger_printf(DBG_INFO, "   All trapped errors\n")
                fi;
                for i in ls do
                    if i <> 'traperror' then debugger_printf(DBG_INFO, "   %a\n",i) fi
                od
            fi
        fi;
        if procname <> 'showstop[nonl]' then debugger_printf(DBG_INFO, "\n") fi;
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
            debugger_printf(DBG_STACK, "%a: %s\n\t%a\n",stack[i],stack[i+1],stack[i-1])
        od;
        if stack[5] = 'TopLevel' then
            debugger_printf(DBG_STACK,"Currently at TopLevel.\n")
        else
            debugger_printf(DBG_STACK,"Currently in %a.\n",stack[5])
        fi;
        NULL
    end proc:

#}}}

#{{{ printf_to_server

    printf_to_server := proc(tag)
    local msg,len;
        msg := sprintf(_rest);
        if 0 < max_length then
            len := length(msg);
            if max_length < len then
                msg := sprintf("---output too long (%d bytes)---\n", len);
            end if;
        end if;
        # Sockets:-Write(sid, sprintf("<%a>",tag));
        Sockets:-Write(sid, msg);
        Sockets:-Write(sid, sprintf("</%a>",tag));
        Sockets:-Write(sid, END_OF_MSG);
        if view_flag then
            fprintf('INTERFACE_DEBUG',_passed);
        end if;
        return NULL;
    end proc;

#}}}

end module:

protect('mdc'):

#savelib('mdc'):

