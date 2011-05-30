# -*- mpldoc -*-

#{{{ mpldoc

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

#}}}

$define DEBUGGER_PROCS debugger, `debugger/printf`, `debugger/readline`, showstat, showstop, where
$define MDS_DEFAULT_PORT 10\000
$define END_OF_MSG "---EOM---"

unprotect('mdc'):
module mdc()

global DEBUGGER_PROCS;

export ModuleApply
    ,  Debugger
    ,  Format
    ,  Authenticate
    ;

#{{{ local declarations

local Connect
    , Disconnect
    , ModuleUnload
    , createID
    , max_length := 10\000 # too small?
    , Port := MDS_DEFAULT_PORT
    , Host := "localhost"

    , printf_to_server
    , sid := -1
    , view_flag := false
    ;

#}}}

#$include <src/Format.mm>
#$include <src/Debugger.mm>

#{{{ ModuleApply

    ModuleApply := proc( (* no positional parameters *)
                         { beep :: truefalse := true }
                         , { config :: {string,identical(maplet)} := NULL }
                         , { connection :: identical(socket,pipe,ptty) := 'socket' }
                         , { greeting :: string := "" }
                         , { host :: string := Host }
                         , { label :: string := kernelopts('username') }
                         , { maxlength :: nonnegint := max_length }
                         , { password :: string := "" }
                         , { port :: posint := Port }
                         #, { timeout :: nonnegint := 0 }
                         , { stopat :: string := "" }
                         , { stoperror :: truefalse := false }
                         , { traperror :: truefalse := false }
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
            Debugger:-restoreProcs();
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
            Debugger:-restoreProcs();
            error;
        end try;

        if stopat <> "" then
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
                    , { greeting :: string := "" }
                    , $
                   )
        if sid <> -1 then
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
        if sid <> -1 then
            Sockets:-Close(sid);
            sid := -1;
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


end module:

protect('mdc'):

LibraryTools:-Save('mdc',"mdc.mla");
