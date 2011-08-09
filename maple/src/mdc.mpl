# -*- mpldoc -*-

#{{{ mpldoc

##INCLUDE ../include/mpldoc_macros.mpi

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
##  and environmental variables configured, the "mds info"
##  pages can be read by typing *info mds* in a shell.
##
##SEEALSO
##- "debugger"
##- "\MOD[\MOD]"
##- "\MOD[Grid]"
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
##  procedure with "stopat", "stopwhen", or "stoperror", the executing
##  code that calls the procedure. In the standard Maple GUI the
##  debugger may also be invoked by clicking the *debug icon* on the
##  toolbar during a running computation.
##
##- The target procedures can also be instrumented by passing the
##  `stopat`, `stopwhen`, and `stoperror` options to `\CMD`.  These,
##  as well as configuration are, options described in the *Options*
##  section.
##
##OPTIONS
##-(lead="indent")
##  Some of the following keyword options have defaults that can
##  be overridden by assigning an entry to the global table
##  'mdc_default', using the name of the option as the index.
##  For example, to override the default for `port` to 12345,
##  assign ~mdc_default['port'] := 12345~. Entering this assignment
##  in a "Maple initialization file" makes it available for
##  all sessions.
##
##opt(emacs,string)
##  Executable used to launch emacs.
##  Only used if `launch_emacs` is true.
##  Default is ~"emacs"~; it can be overridden.
##
##opt(exit,truefalse)
##  If true, shutdown the TCP connection
##  and restore the original debugger procedures.
##  The default is false.
##
##opt(host,string)
##  The name of the host machine that is running the Maple Debugger Server.
##  The default is _"localhost"_; it can be overridden.
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
##  The default is false; it can be overridden.
##
##opt(maxlength,nonnegint)
##  Limits the length of string the client sends to the server.
##  If a string is longer than `maxlength`, it is replaced
##  with a message indicating the problem and the original length.
##  0 means no limit.
##  The default is 10000; it can be overridden.
##
##opt(port,posint)
##  Assigns the TCP port used for communication.
##  Must match the value used by the server.
##  The default is 10000; it can be overridden.
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
##opt(stoperror,truefalse)
##  If true, stop at any error.
##  The default is false; it can be overridden.
##
##opt(traperror,truefalse)
##  If true, stop at trapped errors.
##  The default is false; it can be overridden.
##
##opt(unstopat, name\comma string\comma list\comma or set of same)
##  Specifies procedures from which to remove instrumentation.
##  Strings are parsed with ~kernelopts(opaquemodules=false)~.
##  A list is used to specify a procedure and a statement number.
##  See the `stopat` option.
##
##opt(usegrid,truefalse)
##  If true, append the "Grid" node-number to the label.
##  This option is  added by the "mdc[Grid]" exports to instrument
##  procedures for use with Grid.
##  The default is false.
##
##opt(view,truefalse)
##  If true, the remote debugging session is echoed on the client machine.
##  This only has an effect with command-line maple.
##  The default is false; it can be overridden.
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
##XREFMAP
##- "Maple Debugger Client" : Help:mdc
##- "Maple initialization file" : Help:worksheet,reference,initialization
##- "mds info" : file://{HOME}/maple/lib/mds.html
##
##SEEALSO
##- "\MOD"
##- "debugger"
##- "\MOD[Grid]"
##ENDMPLDOC

#}}}

$define MDC # to allow minting w/out include path
$define MDS_DEFAULT_PORT 10\000
$define END_OF_MSG "---EOM---"

#$define LOG_READLINE # for debugging (need to close file)

unprotect('mdc'):
module mdc()

export Authenticate
    ,  Debugger
    ,  Format
    ,  Grid
    ,  Sleep
    ,  mdc
    ,  Version
    ;

#{{{ local declarations

local Connect
    , Disconnect
    , CreateID
    , GetDefault
    , ModuleApply
    , ModuleUnload
    , Read
    , Write
    , WriteTagf

    # Module-local variables.  Seems unlikely that the debugger
    # will ever be thread-safe, so this is not serious.

    , Host
    , max_length
    , Port
    , sid := -1
    , view_flag
    ;

#}}}

$ifdef BUILD_MLA
$include <src/Debugger.mm>
$include <src/Format.mm>
$include <src/Grid.mm>
$endif

    ModuleApply := mdc;

#{{{ mdc

    mdc := proc( (* no positional parameters *)
                 { exit :: truefalse := false }
                 , { host :: string := GetDefault(':-host',"localhost") }
                 , { label :: string := kernelopts('username') }
                 , { maxlength :: nonnegint := GetDefault(':-maxlength',10\000) }
                 , { launch_emacs :: truefalse := GetDefault(':-launch_emacs',false) }
                 , { emacs :: string := GetDefault(':-emacs', "emacs") }
                 , { port :: posint := GetDefault(':-port',MDS_DEFAULT_PORT) }
                 , { stopat :: {string,name,list,set({string,name,list})} := "" }
                 , { stoperror :: truefalse := GetDefault(':-stoperror',false) }
                 , { traperror :: truefalse := GetDefault(':-traperror',false) }
                 , { unstopat :: {string,name,list,set(string,name,list)} := "" }
                 , { unstoperror :: truefalse := false }
                 , { usegrid :: truefalse := false }
                 , { view :: truefalse := GetDefault(':-view',false) }
                 , $
               )

    global `debugger/width`;
    local lbl;

        if exit then
            Debugger:-Restore();
            Disconnect();
            return NULL;
        end if;

        view_flag := view and not IsWorksheetInterface();
        max_length := maxlength;

        if max_length > 0 then
            `debugger/width` := max_length;
        end if;

        if usegrid then
            lbl := sprintf("%s-%d", label, :-Grid:-MyNode());
        else
            lbl := label;
        end if;

        Debugger:-Replace();

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

        if stopat :: set then
            map(Debugger:-stopat, stopat);
        elif stopat <> "" then
            Debugger:-stopat(stopat);
        end if;

        if unstopat :: set then
            map(Debugger:-unstopat, unstopat);
        elif unstopat <> "" then
            Debugger:-unstopat(unstopat);
        end if;

        if stoperror then
            :-stoperror('all');
        end if;

        if unstoperror then
            debugopts('delerror' = 'all');
        end if;

        if traperror then
            :-stoperror(':-traperror');
        end if;

        return NULL;

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
            line := Sockets:-Read(sid);
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

    WriteTagf := proc(tag)
    uses Write = Sockets:-Write;
    local msg,len;
        msg := sprintf(_rest);
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

    Version := "0.1.1.11";

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

#{{{ GetDefault

    GetDefault := proc( opt :: name, default, $ )
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
