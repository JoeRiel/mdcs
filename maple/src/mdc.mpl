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
##opt(config,maplet or string)
##  Specifies a method for configuring the client.
##  If a string, then the configuration is read
##  from that filename.  If the symbol is `maplet`,
##  then a "Maplet" is launched that queries
##  for the configuration.
##  If 'config' is not specified, then all defaults
##  are used.
##
##opt(exit,truefalse)
##  If *true*, shutdown the TCP connection
##  and restore the original debugger procedures.
##  The default is *false*.
##
##opt(host,string)
##  The name of the host machine that is running the Maple Debugger Server.
##  The default is _"localhost"_.
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
##opt(maxlength,nonnegint)
##  Limits the length of string the client will transmit.
##  If a string is longer than `maxlength`, it is replaced
##  with a message indicating the problem and the original length.
##  0 means no limit.
##  The default is 10000.
##
##opt(port,posint)
##  Assigns the TCP port used for communication.
##  Must match the value used the server.
##  The default is 10000.
##
##opt(stopat, name\comma string\comma or set of names and strings)
##  Specifies the procedures to instrument.
##  Strings are parsed with ~kernelopts(opaquemodules=false)~,
##  so this provides a convenient means to instrument
##  local procedures of a module.  See the `unstopat` option.
##  Using this option may be considerably faster than
##  calling the "stopat" procedure.
##
##opt(stoperror,truefalse)
##  If *true*, stop at any error.
##  The default is *false*.
##
##opt(traperror,truefalse)
##  If *true*, stop at trapped errors.
##  The default is *false*.
##
##opt(unstopat, name\comma string\comma or set of names and strings)
##  Specifies procedures from which to remove instrumentation.
##  Strings are parsed with ~kernelopts(opaquemodules=false)~.
##  See the `stopat` option.
##
##opt(usegrid,truefalse)
##  If *true*, append the "Grid" node number to the label.
##  This option is  added by the "mdc[Grid]" exports to instrument
##  procedures for use with Grid.
##  The default is *false*.
##
##opt(view,truefalse)
##  If *true*, the remote debugging session is echoed on the client machine.
##  This only has an effect with command-line maple.
##  The default is *false*.
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
    ,  mdc
    ,  Version
    ;

#{{{ local declarations

local Connect
    , Disconnect
    , CreateID
    , ModuleApply
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
$endif

    ModuleApply := mdc;

#{{{ mdc

    mdc := proc( (* no positional parameters *)
                 { config :: {string,identical(maplet)} := "" }
                 , { exit :: truefalse := false }
                 , { host :: string := Host }
                 , { label :: string := kernelopts('username') }
                 , { maxlength :: nonnegint := max_length }
                 #, { password :: string := "" }
                 , { port :: posint := Port }
                 , { stopat :: {string,name,set({string,name})} := "" }
                 , { stoperror :: truefalse := false }
                 , { traperror :: truefalse := false }
                 , { unstopat :: {string,name,set(string,name)} := "" }
                 , { unstoperror :: truefalse := false }
                 , { usegrid :: truefalse := false }
                 #, { usethreads :: truefalse := false }
                 , { verbose :: truefalse := false }
                 , { view :: truefalse := false }
                 , $
               )

    global `debugger/width`;
    local lbl;

        if config <> "" then
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

        Debugger:-Replace();

        if sid = -1 then
            try
                Connect(host, port, CreateID(lbl));
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
                    , $
                   )
    local line;
        if sid <> -1 then
            Sockets:-Close(sid);
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

    Version := "0.1.1.9";

#}}}

end module:

protect('mdc'):

LibraryTools:-Save('mdc', "mdc.mla");
