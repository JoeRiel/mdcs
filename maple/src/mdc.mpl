# -*- mpldoc -*-

#{{{ mpldoc

##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE CMD mdc
##MODULE(help) \CMD
##HALFLINE Maple Debugger Client
##AUTHOR   Joe Riel
##DATE     May 2011
##CALLINGSEQUENCE
##- \CMD('opts')
##PARAMETERS
##param_opts(\CMD)
##RETURNS
##- `NULL`
##DESCRIPTION
##- The `\CMD` command launches a *Maple Debugger Client*,
##  which connects to a *Maple Debugger Server*.
##  When debugging commences, the action is controlled
##  by the server.
##  This architecture has several benefits:
##
##-- A common, full-featured debugger interface can be used
##  whether running Maple from the the GUI or command-line.
##
##-- Remote debugging is possible.  The client can be
##  run on one machine, the server on another.
##  Communication is via standard TCP.
##
##-- Multiple Maple processes can debugged concurrently.  This permits
##  directly comparing the actions of different versions of code, or
##  comparing code run on machines with different operating
##  systems. It also permits stepping through separate processes in a
##  "Grid" application.
##
##- Debugging is invoked in the usual way, by instrumenting a
##  procedure with "stopat", "stopwhen" or "stoperror", then running
##  the procedure. It may also be invoked in the the GUI by clicking
##  the debug icon on the toolbar during a running computation.
##
##  The '\CMD' command provides convenient options for
##  instrumenting a procedure; see the 'stopat' and 'stoperror'
##  options, below.
##
##- The user interface of the *Maple Debugger Server*,
##  which controls the debugger, is described in its
##  info pages.
##
##
##OPTIONS
##opt(beep,truefalse)
##  If `true`, emit a beep
##  when succesfully connecting.
##  Disabled when in the GUI because it does not work there.
##  The default is `true`.
##opt(config,maplet or string)
##  Specifies a method for configuring the client.
##  If a string, then the configuration is read
##  from that filename.  If the symbol is `maplet`,
##  then a "Maplet" is launched that queries
##  for the configuration.
##  If 'config' is not specified, then all defaults
##  are used.
##opt(exit,truefalse)
##  If `true`, shutdown the TCP connection.
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
##opt(stopat,name or string)
##  Identifies the procedures to instrument.
##  May be a name, a string, or a set of names or strings.
##  Strings are parsed with ~kernelopts(opaquemodules=false)~,
##  so this provides a convenient means to instrument
##  local procedures of a module.
##
##opt(stoperror,truefalse)
##  If `true`, stop at any error.
##  The default is `false`.
##opt(traperror,truefalse)
##  If `true`, stop at trapped errors.
##  The default is false.
##opt(unstopat, procedures to stopat)
##  Identifies procedures from which to remove instrumentation.
##  May be a name, a string, or a set of names or strings.
##  Strings are parsed with ~kernelopts(opaquemodules=false)~.
##
##opt(usegrid,truefalse)
##  If `true`, append the "Grid" node number
##  to the label.  This option is automatically
##  added when "ProcToString" is used to instrument
##  procedures for use with Grid.
##  The default is `false`.
##opt(view,truefalse)
##  If `true`, the remote debugging session is echoed on the client machine.
##  This only has an effect with command-line maple.
##  The default is `false`,
##EXAMPLES(noexecute)
##- Launch the Maple debug client, instrumenting "int".
##  The `verbose` option
##> mdc(stopat=int);
##> int(x,x);
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
    ,  ModuleApply
    ,  Version
    ;

#{{{ local declarations

local Connect
    , Disconnect
    , CreateID
    , ModuleUnload
    , Read
    , Write
    , WriteTagf

    # Module-local variables.  Seems unlikely that the debugger
    # will ever be thread-safe, so this is not serious.

    , max_length := 10\000 # too small?
    , Port := MDS_DEFAULT_PORT
    , Host := "localhost"

    , sid := -1
    , view_flag := false
    ;

#}}}

$ifdef BUILD_MLA
$include <src/Debugger.mm>
$include <src/Format.mm>
$include <src/Grid.mm>
#$include <src/Sample.mm>
$endif

#{{{ ModuleApply

    ModuleApply := proc( (* no positional parameters *)
                         { beep :: truefalse := true }
                         , { config :: {string,identical(maplet)} := NULL }
                         #, { enter :: truefalse := false }
                         , { exit :: truefalse := false }
                         , { host :: string := Host }
                         , { label :: string := kernelopts('username') }
                         , { maxlength :: nonnegint := max_length }
                         #, { password :: string := "" }
                         , { port :: posint := Port }
                         , { stopat :: {string,name,set(string,name)} := "" }
                         , { stoperror :: truefalse := false }
                         , { traperror :: truefalse := false }
                         , { unstopat :: {string,name,set(string,name)} := "" }
                         , { usegrid :: truefalse := false }
                         #, { usethreads :: truefalse := false }
                         , { verbose :: truefalse := false }
                         , { view :: truefalse := false }
                         , $
        )

    global `debugger/width`;
    local lbl, Port, self;

        if connection <> 'socket' then
            error "currently only a socket connection is supported"
        elif config <> NULL then
            error "currently the 'config' option is disabled.  Use optional parameters."
        end if;

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

        if usethreads then
            self := :-Threads:-Self();
            lbl := sprintf("%s[%d]", lbl, self);
            Port := port + self; # FIXME: not robust
        else
            Port := port;
        end if;


        Debugger:-Replace();

        try
            Connect(host, Port, CreateID(lbl), _options['beep'], _options['verbose'] );
        catch:
            Debugger:-Restore();
            error;
        end try;

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

        if traperror then
            :-stoperror(':-traperror');
        end if;

        if enter then DEBUG();
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
                    , { beep :: truefalse := true }
                    , { verbose :: truefalse := true }
                    , $
                   )
    local line;
        if sid <> -1 then
            Sockets:-Close(sid);
        end if;
        if beep and not IsWorksheetInterface() then
            # This doesn't work properly in the gui, it prints a box
            # and doesn't make a tone.  It works in the tty interface.
            printf("\a");
        end if;
        sid := Sockets:-Open(host, port);
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
        error "could not connect";
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
        Sockets:-Read(sid);
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
        if tag <> 'DBG_SHOW' and 0 < max_length then
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

    Version := "0.1";

#}}}

end module:

protect('mdc'):

LibraryTools:-Save('mdc', "mdc.mla");
