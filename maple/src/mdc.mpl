# -*- mpldoc -*-

##INCLUDE ../include/mpldoc_macros.mpi

#{{{ mpldoc

##DEFINE MOD mdc
##DEFINE CMD mdc
##MODULE(help) \MOD
##HALFLINE Overview of the Maple Debugger Client Module
##AUTHOR   Joe Riel
##DATE     May 2011
##DESCRIPTION
##- The `\MOD` module implements a *Maple Debugger Client*,
##  which is half of a *Maple Debugger Client/Server* pair.
##  This client/server architecture provides several
##  significant benefits:
##
##-- A common, full-featured, debugger interface that can be used
##  whether running Maple from the GUI or the command-line.
##
##-- Remote debugging; the client (Maple) can be run on one machine,
##  the server on another. Communication is via standard TCP.
##
##-- Concurrent debugging; Maple processes can be debugged
##  simultaneously. This permits interactively comparing the actions
##  of different versions of code, or comparing code run on machines
##  with different operating systems. It also permits independently or
##  synchronously stepping through separate processes in a "Grid"
##  application.
##
##- The `\MOD` module is an appliable module
##  that launches the Maple Debugger Client.
##  See "\MOD[\MOD]" for details.
##
##- The submodule "\MOD[Grid]" provides several exports
##  for using "Grid" with the `\MOD`.
##
##- The user interface of the *Maple Debugger Server*,
##  which controls the debugger, is described in its
##  *info* pages. If this package has been properly installed,
##  and environmental variables configured, the mds info
##  pages can be read by typing *info mds* in a shell.
##  An html version of the mds info pages is available
##  in the **doc** subdirectory of the installation.
##  On Windows machines, where installation of the info page
##  is a bit more complicated, the html version can be
##  opened with the command "HelpMDS".
##
##SEEALSO
##- "debugger"
##- "\MOD[\MOD]"
##- "\MOD[Grid]"
##- "\MOD[Count]"
##- "\MOD[HelpMDS]"
##- "\MOD[SkipUntil]"
##- "Grid"
##ENDMPLDOC


##PROCEDURE(help) \MOD[mdc]
##HALFLINE Maple Debugger Client
##AUTHOR   Joe Riel
##DATE     Jun 2011
##CALLINGSEQUENCE
##- \CMD('opts')
##PARAMETERS
##param_opts(\CMD)
##RETURNS
##- `NULL`
##DESCRIPTION
##- The `\CMD` command launches the "Maple Debugger Client".
##  If the client successfully connects to a *Maple Debugger Server*,
##  it replaces the standard Maple "debugger" with
##  procedures that transfer debugging control to the server.
##
##- Debugging is invoked in the usual way, by instrumenting a target
##  procedure with "stopat", "stopwhen", or "stoperror", then executing
##  code that calls the procedure. In the standard Maple GUI the
##  debugger may also be invoked by clicking the *debug icon* on the
##  toolbar during a running computation.
##
##- The target procedures can also be instrumented by passing the
##  `stopat`, `stoperror`, `stopwhen`, and `stopwhenif` options to
##  `\CMD`.  These, as well as configuration options, are described in
##  the *Options* section.
##
##OPTIONS
##
##SUBSECTION(collapsed) Instrumentation Options
##-(lead="indent")
##  These options are used to instrument procedures for debugging.
##  Most correspond to a standard Maple procedure of the same name.
##  The corresponding procedure can also be used;
##  these options may provide additional functionality.
##  The details section describes these options.
##- `stopat` : set a watchpoint on a specified procedure
##- `stoperror` : set a watchpoint on a specified error
##- `stopwarning` : set a watchpoint on a specified warning
##- `stopwhen` : set a watchpoint on a specified variable
##- `stopwhenif` : set a conditional watchpoint on a specified variable
##- `traperror` : stop at a trapped error
##- `unstopat` : clear a procedure watchpoint
##- `unstoperror` : clear an error watchpoint
##- `unstopwhen` : clear a conditional watchpoint
##ENDSUBSECTION
##
##SUBSECTION(collapsed) Configuration Options
##-(lead="indent")
##  These options configure the debugger.
##  They are *sticky*, meaning they remain
##  in effect until changed by an explicit
##  use of the option.
##  The details section describes these options.
##
##-(lead="indent")
##  The initial value of these options can be overridden by assigning
##  an entry to the global table 'mdc_default', using the name of the
##  option as the index.  For example, to override the default for
##  `port` to 12345, assign ~mdc_default['port'] := 12345~. Entering
##  this assignment in a "Maple initialization file" makes it
##  available for all sessions.
##
##- `debug_builtins` : allows debugging built-in procedures
##- `emacs` : executable for starting Emacs
##- `host` : id of debugger server
##- `ignoretester` : used with the Maplesoft test environment
##- `launch_emacs` : enables auto-launch of Emacs
##- `level` : sets level for tracing
##- `maxlength` : maximum string length sent to server
##- `port` : TCP port number
##- `showoptions` : displays `options` and `description` statements in procedure listings
##- `view` : enables echoing of commands
##ENDSUBSECTION
##
##SUBSECTION(collapsed) Option Details
##opt(debug_builtins,truefalse)
##  If true then the `stopat` option can be used to debug built-in
##  procedure.   The built-in procedure is replaced with a wrapper
##  procedure that calls the original procedure.
##  The default value is false.
##
##opt(emacs,string)
##  Executable used to launch emacs.
##  Only used if `launch_emacs` is true.
##  The default value is ~"emacs"~; this option is sticky.
##
##opt(exit,truefalse)
##  If true, shutdown the TCP connection
##  and restore the original debugger procedures.
##  The default value is false.
##
##opt(host,string)
##  The name of the host machine that is running the Maple Debugger Server.
##  The default value is _"localhost"_; this option is sticky.
##
##opt(ignoretester,truefalse)
##  If true, then do nothing when called from the Maplesoft tester.
##  This is intended for internal use.
##  The default value is true; this option is sticky.
##
##opt(label,string)
##  Label passed to server for identification and grouping of the client.
##  If the basename of two or more labels, from independent
##  clients, are identical, the clients are grouped in the server.
##  The basename is the substring of `label` that matches
##  the first group in the regular expression ~^([^-]+)-[0-9]+$~,
##  that is, everything before a hyphen followed by digits
##  that terminate the string.  For example, ~"foo-1"~ and ~"foo-2"~
##  share a common basename, ~"foo"~, and so would be grouped together.
##  The default label is the return value of _kernelopts('username')_.
##
##opt(launch_emacs,truefalse)
##  If true and unable to connect to a Maple Debugger Server,
##  then launch emacs and start a Maple Debugger Server.
##  See the `emacs` option.
##  The default value is false; this option is sticky.
##
##opt(level,posint)
##  Sets the level used when tracing with the *level* mode.
##  Analogous to "printlevel", but the scale differs.
##  The default value is 75; this option is sticky.
##
##opt(maxlength,nonnegint)
##  Limits the length of string the client sends to the server.
##  If a string is longer than `maxlength`, it is replaced
##  with a message indicating the problem and the original length.
##  0 means no limit.
##  The default value is 10000; this option is sticky.
##
##opt(port,posint)
##  Assigns the TCP port used for communication.
##  Must match the value used by the server.
##  The default value is 10000; this option is sticky.
##
##opt(showoptions,truefalse)
##  When true, the **options** and **description** statements
##  in procedures are displayed in the showstat listing.
##  *Because of a debug kernel deficiency, when this option
##  is in effect, the debugger may occasionally continue to
##  the end when first debugging a procedure.*
##  The second time the debugger enters the procedure, it should work properly.
##  The initial default is false; this option is sticky.
##
##opt(skip_until,anything)
##  Specifies a predicate that can be used to skip
##  all code until a condition is met.  If `skip_until` is
##  a procedure, it is used as the predicate.
##  The result of each executed statement is passed to the predicate,
##  which must return true or false.
##  When true is returned, debugging recommences.
##
##  If `skip_until` is not a procedure, the following predicate is used:
##  ~proc() has([_passed],skip_until) end proc~.
##  See "mdc[SkipUntil]" for a procedure that assigns the predicate
##  and has additional options.
##
##  To use the skip predicate, execute the `_skip` debugger command
##  inside the debugger.  That command is bound to the `S` key.
##
##opt(stopat, name\comma string\comma list\comma or set of same)
##  Specifies the procedures to instrument.
##  Strings are parsed with ~kernelopts(opaquemodules=false)~,
##  so this provides a convenient means to instrument local procedures of a module.
##  A list is used to specify a procedure, a statement number, and (optionally) a condition.
##  See the `unstopat` option.
##  Using this option may be considerably faster than
##  calling the "stopat" procedure.
##
##opt(stoperror,truefalse\comma string\comma or set of strings)
##  If false, ignore.
##  If true, stop at any error.
##  If a string, stop at that error message.
##  If a set of strings, stop at any of those error messages.
##  The default value is false.
##
##opt(stopwarning, string\comma set of strings\comma or truefalse)
##  Assign strings ("regular expressions") that stop the debugger when a matching warning
##  occurs.  The "WARNING" procedure is replaced with one that matches
##  each regular expression against the formatted warning message.  If
##  a match occurs, execution stops inside WARNING.
##  If `stopwarning` is true, or passed by itself,
##  the set of regular expressions is assigned ~{""}~,
##  which matches any warning.
##  If `stopwarning` is false, `WARNING` is restored.
##  The default action is to not change the previous condition.
##
##opt(stopwhen,name\comma list\comma or set)
##  Causes the debugger to halt when a specified variable changes.
##  A name corresponds to a global variable.
##  A list corresponds to variable local to a procedure,
##  the first element is the procedure name, the second the variable name.
##  A set is mapped over.
##  This is equivalent to calling the "stopwhen" command.
##  To clear, see the `unstopwhen` option, below.
##
##opt(stopwhenif, list\comma or set of lists)
##  Causes the debugger to halt when a specified global variable
##  is assigned a specified value.
##  The first element of a list is the global variable,
##  the second is the value that stops the debugger.
##  A set of lists may be used to specify conditions for multiple
##  variables (but a specific variable can only have one halting value).
##  This is equivalent to calling the "stopwhenif" command.
##  To clear, see the `unstopwhen` option, below.
##
##opt(traperror,truefalse\comma string\comman or set of string)
##  If false, ignore.
##  If true, stop at any trapped error.
##  If a string, stop at a trapped error error with that message.
##  If a set of strings, stop at a trapped error with any of those error messages.
##  The default value is false.
##
##opt(unstopat, name\comma string\comma list\comma or set of same)
##  Specifies procedures from which to remove instrumentation.
##  Strings are parsed with ~kernelopts(opaquemodules=false)~.
##  A list is used to specify a procedure and statement number.
##  See the `stopat` option.
##
##opt(unstoperror,truefalse\comma string\comma or set of strings)
##  Clear stops set by "stoperror".
##  If false, ignore.
##  If true, clear all stoperrors.
##  If a string, clear that error message.
##  If a set of strings, clear those error messages.
##  The default value is false.
##
##opt(unstopwhen,name\comma list\comma or set)
##  Clears one or more `stopwhen` or `stopwhenif` triggers.
##  This is equivalent to calling the "unstopwhen" command.
##  See the `stopwhen` and `stopwhenif` options, above.
##
##opt(unstoperror,truefalse\comma string\comma or set of strings)
##  Clear stops set by the "traperror" option.
##  If false, ignore.
##  If true, clear all trapped errors.
##  If a string, clear that error message.
##  If a set of strings, clear those error messages.
##  The default value is false.
##
##opt(usegrid,truefalse)
##  If true, append the "Grid" node-number to the label.
##  This option is  added by the "mdc[Grid]" exports to instrument
##  procedures for use with Grid.
##  The default value is false.
##
##opt(view,truefalse)
##  If true, the remote debugging session is echoed on the client machine.
##  This only has an effect with command-line maple.
##  The default value is false; this option is sticky.
##
##ENDSUBSECTION
##
##EXAMPLES(noexecute)
##- Launch the Maple debugger client, instrumenting "int".
##  Assume the Maple Debugger Server is running on a different
##  machine, named `gauss`.
##> mdc(stopat=int, host="gauss"):
##SET(lead=indent)
##- ~Welcome joe~
##UNSET
##- Now launch the debugger.
##> int(x^2,x);
##
##- When finished debugging, shutdown the client, restoring the debugger procedures.
##> mdc(exit):
##
##- Assign a procedure that computes the fibonacci function
##>> fib := proc(n::nonnegint)
##>> option remember;
##>>    if n < 2 then n
##>>    else fib(n-2) + fib(n-1)
##>>    end if;
##>> end proc:
##- Set a conditional breakpoint so that debugging begins
##  when `fib` is called with argument `n` equal to 10.
##> mdc(stopat=[fib,1,'n=10']):
##>(noexecute) fib(20);
##- Clear the conditional breakpoint.
##> mdc(unstopat=[fib,1]):
##- Use "mdc[Count]" in a conditional breakpoint to begin debugging on the eighth call to fib.
##  Forward quotes are used to prevent premature evaluation.
##> mdc(stopat=[fib,1,'mdc:-Count()=8']):
##>(noexecute) fib(10);
##
##- Stop on any warning.
##> mdc(stopwarning=true);
##>(noexecute) int(1/x, x=a..1);
##- Clear the stop-on-warning.
##> mdc(stopwarning=false);
##
##- Use the `skip_until` option to stop the debugger during an
##  integration of _x^2_ at the place where _x^3_ first appears.
##
##> mdc(stopat=int, skip_until=x^3);
##> int(x^2, x);
##
##XREFMAP
##- "Maple Debugger Client" : Help:mdc
##- "Maple initialization file" : Help:worksheet,reference,initialization
##- "regular expressions" : Help:Regular_Expressions
##
##SEEALSO
##- "\MOD"
##- "debugger"
##- "\MOD[Grid]"
##- "\MOD[Count]"
##ENDMPLDOC

#}}}

$define MDC # to allow minting w/out include path
$define MDS_DEFAULT_PORT 10\000
$define END_OF_MSG "---EOM---"

#$define LOG_READLINE # for debugging (need to close file)

unprotect('mdc'):
module mdc()

export Authenticate
    ,  Count
    ,  Debugger
    ,  Format
    ,  Grid
    ,  InstallPatch
    ,  Sleep
    ,  SkipUntil
    ,  mdc
    ,  Version
    ,  HelpMDS
    ;

#{{{ local declarations

local Connect
    , Disconnect
    , CreateID
    , GetDefault
    , ModuleApply
    , ModuleLoad
    , ModuleUnload
    , Read
    , Write
    , WriteTagf

    # Module-local variables.  Seems unlikely that the debugger
    # will ever be thread-safe, so this is not serious.

    , cnt
    , debugbuiltins := false
    , Host
    , Level
    , match_predicate := () -> true
    , max_length
    , Port
    , sid := -1
    , view_flag
    , show_options_flag
    , skip
    , Warnings
    ;

#}}}

$ifdef BUILD_MLA
$include <src/Debugger.mm>
$include <src/Format.mm>
$include <src/Grid.mm>
$include <src/HelpMDS.mm>
$include <src/InstallPatch.mm>
$endif

    ModuleApply := mdc;
    Warnings := {};

#{{{ mdc

    mdc := proc( (* no positional parameters *)
                 { debug_builtins :: truefalse := debugbuiltins }
                 , { emacs :: string := GetDefault(':-emacs', "emacs") }
                 , { exit :: truefalse := false }
                 , { host :: string := GetDefault(':-host',"localhost") }
                 , { ignoretester :: truefalse := GetDefault(':-ignoretester',true) }
                 , { label :: string := kernelopts('username') }
                 , { launch_emacs :: truefalse := GetDefault(':-launch_emacs',false) }
                 , { level :: posint := GetDefault(':-level',75) }
                 , { maxlength :: nonnegint := GetDefault(':-maxlength',10\000) }
                 , { port :: posint := GetDefault(':-port',MDS_DEFAULT_PORT) }
                 , { showoptions :: {truefalse,identical(ignore)} := GetDefault(':-showoptions','ignore') }
                 , { stopat :: {string,name,list,set({string,name,list})} := "" }
                 , { stoperror :: {truefalse,string,set} := false }
                 , { stopwarning :: {string,set(string),truefalse} := NULL }
                 , { stopwhen :: { name, list, set } := NULL }
                 , { stopwhenif :: { list, set(list) } := NULL }
                 , { skip_until := NULL }
                 , { traperror :: {truefalse,string,set} := false }
                 , { unstopat :: {string,name,list,set(string,name,list)} := "" }
                 , { unstoperror :: {truefalse,string,set,identical(true)} := false }
                 , { unstopwhen :: { name, list, set } := NULL }
                 , { untraperror :: {truefalse,string,set,identical(true)} := false }
                 , { usegrid :: truefalse := false }
                 , { view :: truefalse := GetDefault(':-view',false) }
                 , $
               )

    global `debugger/width`, WARNING;
    local lbl,str;

        #{{{ exit
        if exit then
            Debugger:-Restore();
            Disconnect();
            return NULL;
        end if;
        #}}}
        #{{{ ignoretester
        if ignoretester then
            if assigned('TESTER_SOURCEDIR') then
                return NULL;
            end if;
        end if;
        #}}}

        debugbuiltins := debug_builtins;

        view_flag := view and not IsWorksheetInterface();
        max_length := maxlength;

        Level := level;

        #{{{ max_length

        if max_length > 0 then
            `debugger/width` := max_length;
        end if;

        #}}}
        #{{{ usegrid
        if usegrid then
            lbl := sprintf("%s-%d", label, :-Grid:-MyNode());
        else
            lbl := label;
        end if;
        #}}}
        #{{{ replace debugger

        Debugger:-Replace();
        #}}}
        #{{{ connect to server
        if sid = -1 then
            try
                Connect(host, port, CreateID(lbl)
                        , _options['launch_emacs']
                        , _options['emacs']
                       );
            catch:
                Debugger:-Restore();
                error;
            end try;
        end if;
        #}}}
        #{{{ showoptions
        if showoptions :: truefalse then
            show_options_flag := showoptions;
        end if;
        #}}}
        #{{{ stopat
        if stopat :: set then
            map(Debugger:-stopat, stopat);
        elif stopat <> "" then
            Debugger:-stopat(stopat);
        end if;
        #}}}
        #{{{ unstopat
        if unstopat :: set then
            map(Debugger:-unstopat, unstopat);
        elif unstopat <> "" then
            Debugger:-unstopat(unstopat);
        end if;
        #}}}
        #{{{ stoperror
        if stoperror = true then
            :-stoperror('all');
        elif stoperror :: string then
            :-stoperror(stoperror)
        elif stoperror :: set then
            map(:-stoperror, stoperror);
        end if;
        #}}}
        #{{{ stopwarning
        if stopwarning <> NULL then
            unprotect('WARNING');
            if stopwarning = false then
                WARNING := proc(msg::{string, symbol})
                    print(INTERFACE_WARN(1,msg,args[2 .. -1]))
                end proc(args);
                Warnings := {};
            else
                WARNING := proc()
                local msg, WARNING;
                    msg := StringTools:-FormatMessage(_passed);
                    if ormap(StringTools:-RegMatch, Warnings, msg) then
                        WARNING := proc(msg::{string, symbol})
                            DEBUG();
                            print(INTERFACE_WARN(1,msg,args[2 .. -1]))
                        end proc;
                        WARNING(args);
                    end if;
                end proc;
                Warnings := `if`(stopwarning :: set
                                 , stopwarning
                                 , `if`(stopwarning = true
                                        , {""}
                                        , {stopwarning}
                                       )
                                );
            end if;
            protect('WARNINGS');
        end if;
        #}}}
        #{{{ unstoperror
        if unstoperror = true then
            debugopts('delerror' = 'all');
        elif unstoperror :: string then
            :-unstoperror(unstoperror);
        elif unstoperror :: set then
            map(:-unstoperror, unstoperror);
        end if;
        #}}}
        #{{{ unstoperror
        if untraperror = true then
            :-unstoperror(':-traperror');
        elif untraperror :: string then
            :-unstoperror(':-traperror'[untraperror]);
        elif untraperror :: set then
            for str in untraperror do
                :-unstoperror(':-traperror'[str]);
            end do;
        end if;
        #}}}
        #{{{ stopwhen
        if stopwhen :: '{name,list}' then
            :-stopwhen(stopwhen);
        elif stopwhen :: set then
            map(:-stopwhen, stopwhen);
        end if;
        #}}}
        #{{{ stopwhenif
        if stopwhenif :: list then
            proc(x,v) :-stopwhenif(x,v) end proc(op(stopwhenif));
        elif stopwhenif :: set(list) then
            map( l -> :-stopwhenif(op(l)), stopwhenif);
        end if;
        #}}}
        #{{{ unstopwhen
        if unstopwhen :: '{name,list}' then
            :-unstopwhen(unstopwhen);
        elif unstopwhen :: set then
            map(:-unstopwhen, unstopwhen);
        end if;
        #}}}
        #{{{ traperror
        if traperror = true then
            :-stoperror(':-traperror');
        elif traperror :: string then
            :-stoperror(':-traperror'[traperror] );
        elif traperror :: set then
            for str in traperror do
                :-stoperror(':-traperror'[str]);
            end do;
        end if;
        #}}}
        #{{{ skip_until

        if skip_until <> NULL then
            SkipUntil(skip_until);
        end if;
        skip := false;

        #}}}


        return NULL;

    end proc;

#}}}

#{{{ ModuleLoad

    ModuleLoad := proc()
        debugbuiltins := GetDefault(':-debug_builtins', false);
    end proc;

#}}}
#{{{ ModuleUnload

    ModuleUnload := proc()
        if sid <> -1 then
            try
                Sockets:-Close( sid );
            catch:
            end try;
        end if;
        restoreProcs();
    end proc;

#}}}

#{{{ Connect

##DEFINE PROC Connect
##PROCEDURE \MOD[\PROC]
##HALFLINE initiate a connection to a Maple debugger server

    Connect := proc(host :: string
                    , port :: posint
                    , id :: string
                    , { verbose :: truefalse := false }
                    , { launch_emacs :: truefalse := false }
                    , { emacs :: string := "emacs" }
                    , $
                   )
    local cmd,connected,line,sys;
        if sid <> -1 then
            Sockets:-Close(sid);
        end if;
        try
            sid := Sockets:-Open(host, port);
        catch:
            if launch_emacs then
                sys := kernelopts('platform');
                if sys = "windows" then
                    cmd := sprintf("start /b %s --funcall mds", emacs);
                elif sys = "dos" then
                    error "cannot launch emacs from dos";
                else
                    cmd := sprintf("%s --funcall mds &", emacs);
                end if;
                if system(cmd) <> 0 then
                    error "problem launching emacs"
                end if;
                to 5 do
                    try
                        sid := Sockets:-Open(host, port);
                        connected := true;
                        break;
                    catch:
                        Sleep(1);
                    end try;
                end do;
                if connected <> true then
                    error "could not connect to Maple Debugger Server";
                end if;
            end if;
        end try;
        Host := host;
        Port := port;
        # handle login (hack for now)
        try
            line := Sockets:-Read(sid);
        catch "invalid socket ID":
            error "cannot connect to Debugger server.  Server may not be running."
        end try;
        # printf("%s\n", line);
        if line = "userid: " then
            Sockets:-Write(sid, id);
            line := Sockets:-Read(sid);
            printf("%s\n", line);
            if verbose then
                printf("Connected to %s on port %d, with id %s\n"
                       , host, port, id );
            end if;
            return NULL;
        end if;
        # error "could not connect";
    end proc;

#}}}
#{{{ Disconnect

##DEFINE PROC Disconnect
##PROCEDURE \MOD[\PROC]
##HALFLINE terminate connection to Maple debugger server

    Disconnect := proc()
        if sid <> -1 then
            printf("goodbye\n");
            Sockets:-Close(sid);
            sid := -1;
        end if;
    end proc;

#}}}

#{{{ Read

    Read := proc()
    local res;
        res := Sockets:-Read(sid);
        if res = false then
            error "process %1 disconnected unexpectedly", sid;
        end if;
        return res;
    end proc;

#}}}
#{{{ Write

    Write := proc(msg :: string)
        if sid <> -1 then
            Sockets:-Write(sid, msg);
        end if;
    end proc;

#}}}
#{{{ WriteTagf

##DEFINE PROC WriteTagf
##PROCEDURE \MOD[\PROC]
##AUTHOR   Joe Riel
##DATE     Nov 2011
##CALLINGSEQUENCE
##- \PROC('tag','rest')
##PARAMETERS
##- 'tag'  : ::name::
##- 'rest' : (optional) arguments to "sprintf"
##RETURNS
##- `NULL`

    WriteTagf := proc(tag)
    uses Write = Sockets:-Write;
    local msg,len;
        if _npassed = 1 then
            msg := "";
        else
            msg := sprintf(_rest);
        end if;
        if  tag <> 'DBG_SHOW'
        and tag <> 'DBG_SHOW_INACTIVE'
        and 0 < max_length then
            len := length(msg);
            if max_length < len then
                msg := sprintf("%s... ---output too long (%d bytes)---\n", msg[1..100],len);
            end if;
        end if;
        # hack for now
        Write(sid, sprintf("<%a>",tag));
        Write(sid, msg);
        Write(sid, sprintf("</%a>",tag));
        Write(sid, END_OF_MSG);
        if view_flag then
            fprintf('INTERFACE_DEBUG',_rest);
        end if;
        return NULL;
    end proc;

#}}}
#{{{ CreateID

##DEFINE PROC CreateID
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
## AssignFUNC(mdc:-CreateID):
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


    CreateID := proc(label :: string,$)
        if length(label) = 0 then
            error "label cannot be empty";
        elif not StringTools:-RegMatch("^[][A-Za-z0-9_-]+$", label) then
            error "invalid characters in label '%1'", label;
        end if;
        return sprintf(":%s:%s:%d:", label, kernelopts('platform,pid') );
    end proc;

#}}}

#{{{ Version

    Version := "1.9.3";

#}}}

#{{{ Sleep

##DEFINE CMD Sleep
##PROCEDURE(help) \MOD[\CMD]
##HALFLINE pause execution of the engine
##AUTHOR   Joe Riel
##DATE     Aug 2011
##CALLINGSEQUENCE
##- \CMD('t')
##PARAMETERS
##- 't' : ::nonnegint::; number of seconds to sleep
##DESCRIPTION
##- The `\CMD` command pauses the execution of the Maple engine
##  a specified length of time.  While paused it does not use CPU
##  resources.
##
##- The 't' parameter is the duration to pause, in seconds.
##SEEALSO
##- "Threads[Sleep]"

    Sleep := proc( t :: nonnegint )
    local cmd,sys;
        try
            Threads:-Sleep( t )
        catch:
            sys := kernelopts('platform');
            if sys = "windows" or sys = "dos" then
                cmd := sprintf("timeout \t %d \nobreak", t);
            else
                cmd := sprintf("sleep %d", t);
            end if;
            system(cmd);
        end try;
        return NULL;
    end proc;

#}}}
#{{{ SkipUntil

##DEFINE CMD SkipUntil
##PROCEDURE(help) \MOD[\CMD]
##HALFLINE assign the skip predicate
##AUTHOR   Joe Riel
##DATE     Jan 2012
##CALLINGSEQUENCE
##- \CMD('ex','opts')
##PARAMETERS
##- 'ex' : ::anything::
##param_opts(\CMD)
##RETURNS
##- `NULL`
##DESCRIPTION
##- The `\CMD` command
##  assigns the predicate used when skipping.
##  This is equivalent to using the `skip_until` option to `mdc`.
##
##- If 'ex' is a procedure, it is used as the predicate,
##  otherwise, the predicate is the procedure
##  ~proc() has([_passed],ex) end proc~.
##
##- When skipping, the result of each executed statement is
##  passed to the predicate.  The predicate must return
##  true or false.  When it returns true, skipping is terminated
##  and debugging recommences.  The last expression is
##  displayed in the debugger output.
##
##- The arguments passed to the predicate consist of
##  the result of each executed statement.
##
##- This procedure can be called from inside the debugger to
##  assign a predicate during a debugging session.
##
##
##OPTIONS
##opt(exact, truefalse)
##  When true, the predicate is ~proc() evalb(_passed = ex) end proc~.
##  An exact match is efficient.
##  The default is false.
##opt(usehastype,truefalse)
##  When true, the predicate is ~proc() hastype([_passed],ex) end proc~.
##  The default value is false.
##
##EXAMPLES(notest,noexecute)
##- Assign the predicate to stop when _x^2_ is computed.
##> mdc:-SkipUntil(x^2):
##> mdc(stopat=int):
##> int(x,x);
##
##- Use the `usehastype` option to stop skipping when a square appears.
##> mdc:-SkipUntil(anything^2, usehastype):
##> forget(int):
##> int(x,x);
##
##- Create a predicate that uses an exact match.
##> mdc:-SkipUntil(proc() evalb(_passed=1/2*x^2) end proc):
##> forget(int):
##> int(x,x);
##
##- Create a predicate with "patmatch".
##  Note that we first verify that ~_npassed=1~; without that
##  a statement that returns NULL generates an error because
##  the wrong number of arguments are passed to `patmatch`.
##> mdc:-SkipUntil(() -> _npassed=1 and patmatch(_passed,a::algebraic*x^2)):
##> forget(int):
##> int(x,x);
##
##SEEALSO
##- "mdc"
##- "mdc[mdc]"

    SkipUntil := proc(ex
                      , { usehastype :: truefalse := false }
                      , { exact :: truefalse := false }
                     )
        if exact then
            match_predicate := proc() evalb(_passed = ex) end proc;
        elif usehastype then
            if not ex :: type then
                error "argument must be a type when using hastype, received '%1'", ex;
            end if;
            match_predicate := proc() hastype([_passed],ex) end proc;
        elif ex :: procedure then
            match_predicate := eval(ex);
        else
            match_predicate := proc() has([_passed],ex) end proc;
        end if;
        return NULL;
    end proc;

#}}}
#{{{ Count

##DEFINE CMD Count
##PROCEDURE(help) \MOD[\CMD]
##HALFLINE increment a counter
##AUTHOR   Joe Riel
##DATE     Sep 2011
##CALLINGSEQUENCE
##- \CMD('indices','opts')
##PARAMETERS
##- 'indices' : (optional) arguments used to identify counter
##param_opts(\CMD)
##DESCRIPTION
##- The `\CMD` command increments a counter and returns the result.
##  It is intended to be used with the conditional form of the
##  `stopat` option to "mdc[mdc]" to stop the debugger inside a
##  procedure after a specified number of calls.
##
##- The counter incremented is local to the "mdc" module.  Different
##  counters can be specified by passing arbitrary arguments
##  ('indices') to \CMD.  These arguments are used to index a table
##  that stores the counters.
##
##OPTIONS
##opt(reset,truefalse)
##  If true, clear all the counters.
##  The default value is false, which does not clear counters.
##
##EXAMPLES
##- Exercise a couple of counters.
##> mdc:-Count(), mdc:-Count();
##> mdc:-Count(12), mdc:-Count(), mdc:-Count(12);
##- Reset all counters.
##> mdc:-Count(reset):
##- Assign a procedure that calls itself endlessly.
##> f := proc(x) procname(x+1) end proc:
##- Configure debugging to begin at statement 1 of the 23rd call to f.
##  Forward-quotes are used to prevent premature evaluation of the condition.
##>(noexecute) mdc(stopat=[f, 1, 'mdc:-Count()=23']);
##>(noexecute) f(0);
##SEEALSO
##- "mdc[mdc]"
##- "stopat"

    Count := proc( { reset :: truefalse := false } )
        if reset then
            cnt := table();
            return NULL;
        end if;
        if assigned(cnt[_passed]) then
            cnt[_passed] := cnt[_passed] + 1;
        else
            cnt[_passed] := 1;
        end if;
    end proc:

#}}}
#{{{ GetDefault

    GetDefault := proc( opt :: name, default := NULL, $ )
    global mdc_default;
    description "Read the global table 'mdc_default' for default values.";
        if assigned(mdc_default[opt]) then
            return mdc_default[opt];
        else
            return default;
        end if;
    end proc;
#}}}

end module:

protect('mdc'):

LibraryTools:-Save('mdc', "mdc.mla");
