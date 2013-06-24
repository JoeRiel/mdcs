# -*- mpldoc -*-

##INCLUDE ../include/mpldoc_macros.mpi

#{{{ mpldoc

##DEFINE MOD mdc
##DEFINE CMD mdc
##PACKAGE(help) mdc
##TITLE mdc
##HALFLINE Overview of the Maple Debugger Client package
##AUTHOR   Joe Riel
##DATE     May 2011
##DESCRIPTION
##- The `\MOD` package implements a *Maple Debugger Client*,
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
##  See "mdc[ModuleApply]" for details.
##
##- The submodule "mdc[Grid]" provides several exports
##  for using the "Grid" package with `\MOD`.
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
##SUBSECTION Exports
##- "mdc[Grid]" : submodule for debugging "Grid" processes.
##- "mdc[Count]" : implements a counter
##- "mdc[ModuleApply]" : main export
##- "mdc[HelpMDS]" : display help page for debugger server (Windows only)
##- "mdc[Monitor]" : set/query monitor expressions
##- "mdc[Skip]" : set skip options
##- "mdc[Sleep]" : utility routine for sleeping
##ENDSUBSECTION
##
##SEEALSO
##- "mdc[ModuleApply]"
##- "debugger"
##- "Grid"
##ENDMPLDOC


##PROCEDURE(help) mdc[ModuleApply]
##HALFLINE Maple Debugger Client
##AUTHOR   Joe Riel
##DATE     Jun 2011
##CALLINGSEQUENCE
##- \CMD('stopats', 'opts')
##PARAMETERS
##- 'stopats' : (optional) ::seq({string,name,list})::; procedures to instrument
##param_opts(\CMD)
##RETURNS
##- first procedure in 'stopats'
##DESCRIPTION
##- The `\CMD` command is an appliable module that
##  launches the "Maple Debugger Client".
##  For package details, see "mdc".
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
##- The optional 'stopats' parameter specifies the procedures to
##  instrument, it can be used in place of the `stopat` option.
##  See the `stopat` option for the usage.
##  The first procedure instrumented via the 'stopats' parameter
##  is returned.  That makes it convenient to instrument and call
##  a procedure by doing ~mdc(someproc)(...)~.
##
##- The target procedures can also be instrumented by passing the
##  `stopat`, `stoperror`, `stopwhen`, and `stopwhenif` options to
##  `\CMD`.  These, as well as configuration options, are described in
##  the *Options* section.
##
##
##SUBSECTION Skipping
##desc_skipping
##- The skip predicate, a Maple procedure that is passed the computed
##  output of each executed Maple statement in the running code, is
##  defined by using the `skip_until` option, or via the "mdc:-Skip"
##  command.
##
##- The `skip_check_stack` option causes the top of the call stack to
##  be passed to the skip predicate (the computed output is also
##  passed to the predicate, in a separate call).  This can be useful
##  because an expression may appear on the stack without being the
##  output of a statement.  *This option is memory intensive*.
##ENDSUBSECTION
##
##SUBSECTION Tracing
##-(nolead)
##  *Tracing* is the automatic execution and display of the debugged
##  code.  The debugger server has commands for enabling tracing.
##  Tracing generally continues until the debugger exits,
##  though it can be interrupted manually.
##ENDSUBSECTION
##
##SUBSECTION Monitoring
##-(nolead)
##  *Monitoring* is the continuous display of selected Maple
##  expressions as the debugger steps through code.
##  See "mdc[Monitor]" for a procedure to setup monitoring.
##ENDSUBSECTION
##
##OPTIONS
##
##SUBSECTION Instrumentation Options
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
##SUBSECTION Configuration Options
##-(lead="indent")
##  These options configure the debugger.
##  Most are *sticky*, meaning they remain
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
##- `maxlength` : maximum string length sent to server
##- `port` : TCP port number
##- `print_to_maple` : print each result to Maple
##- `quiet` : suppress greeting from server
##- `reconnect` : reconnect to the debugger server
##- `skip_before` : assigns string used for skipping
##- `skip_check_stack` : enables stack checking during skip
##- `skip_until` : assigns predicate used for skipping (cf. "Skip")
##- `showoptions` : displays `options` and `description` statements in procedure listings
##- `view` : enables echoing of commands
##ENDSUBSECTION
##
##SUBSECTION Option Details
##opt(debug,truefalse)
##  True means launch the debugger immediately; it does so by calling
##  "DEBUG".  This may be used inside a procedure.
##  The default is false.
##opt(debug_builtins,truefalse)
##  True mans attempt to debug a built-in procedure
##  by reassigning it with a wrappe that calls the original.
##  An error is raised if attempting to debug builtins used by the debugger.
##  The default value is false.
##
##opt(emacs,string or procedure)
##  Executable used to launch Emacs.
##  If a string, it is passed to "system".
##  If a procedure, the procedure is called with no arguments;
##  it should launch Emacs and start the Maple Debugger Server.
##  This option is only used if `launch_emacs` is true.
##  The default value is ~"emacs"~; this option is sticky.
##
##opt(exit,truefalse)
##  True means shutdown the TCP connection
##  and restore the original debugger procedures.
##  The default value is false.
##
##opt(goback,truefalse)
##  Activates a previously saved *goback* state.
##  When debugging begins, Maple skips to the
##  saved state.
##
##opt(host,string)
##  The name of the host machine that is running the Maple Debugger Server.
##  The default value is _"localhost"_; this option is sticky.
##
##opt(ignoretester,truefalse)
##  True means do nothing when called from the Maplesoft tester.
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
##  True means if unable to connect to a Maple Debugger Server,
##  launch emacs and start a Maple Debugger Server.
##  See the `emacs` option.
##  The default value is false; this option is sticky.
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
##opt(print_to_maple,truefalse)
##  True means print the result of each debugged statement
##  to the Maple session.  If the debugger is launched from Standard Maple,
##  the results are displayed prettyprinted in the worksheet.
##  The default is false.
##
##opt(quiet,truefalse)
##  True means do not print the greeting/farewell from the server.
##  The default is false; this option is sticky.
##
##opt(reconnect,truefalse)
##  True means reconnect to the debugger server.
##  This may be useful if the server had to be reset.
##  The default is false.
##
##opt(showoptions,truefalse)
##  True means the **options** and **description** statements
##  in procedures are displayed in the showstat listing.
##  *Because of a debug kernel deficiency, when this option
##  is in effect, the debugger may occasionally continue to
##  the end when first debugging a procedure.*
##  The second time the debugger enters the procedure, it should work properly.
##  The default is false; this option is sticky.
##
##opt(skip_before,string)
##  The string is matched against the next statement;
##  if it matches, skipping is halted.
##  This is equivalent to calling "Skip" with option `before`.
##
##opt(skip_check_stack,truefalse)
##  True means, when *skipping*, pass the arguments of the top procedure
##  on the stack to the predicate.  This is done in a separate call to
##  the predicate (in addition to the call that passes the result of
##  each statement).  Doing so is memory intensive, so this option
##  should not normally be used.  The content of the stack is the list
##  generated by ~debugopts(callstack)~.  The default is false; this
##  option is sticky.
##
##opt(skip_indicate_match,truefalse)
##  True means print a message in the debugger output buffer
##  indicating a match has been made while skipping.
##  Setting this to false reduces repetitive statements
##  when using the `_skip` trace mode.
##  The default is true.
##
##opt(skip_until,anything)
##  Specifies a predicate that can be used to skip
##  all code until a condition is met.
##  If `skip_until` is a procedure (but not a name),
##  it is used as the predicate.
##  The result of each executed statement is passed to the predicate,
##  which must return true or false.
##  When true is returned, debugging recommences.
##
##  If `skip_until` is not a procedure, or is the name of a procedure,
##  the following predicate is used:
##  ~proc() has([_passed],skip_until) end proc~.
##  See "mdc[Skip]" for a procedure that assigns the predicate
##  and has additional options.
##
##  To use the skip predicate, execute the `_skip` debugger command
##  inside the debugger, where it is bound to the `S` key.
##
##opt(skip_until[alloc],posint)
##  Similar to `skip_until`, but uses the `bytesalloc` option to "Skip".
##  Skipping stops when _kernelopts(bytesalloc)_ increases by
##  the specified value.
##
##opt(skip_until[exact],anything)
##  Similar to `skip_until`, but passes the `exact` option to "Skip".
##  Skipping stops when a computed result exactly matches the value.
##
##opt(skip_until[level],posint)
##  Similar to `skip_until`, but uses the `stacklevel` option to "Skip".
##  Skipping stops when _kernelopts(level)_ exceeds the value.
##
##opt(skip_until[loc],anything)
##  Similar to `skip_until`, but passes the `matchlocals` option to "Skip".
##  Skipping stops when the value appears in a computed result that
##  has all local variables replaced with the global equivalent.
##
##opt(skip_until[type],anything)
##  Similar to `skip_until`, but passes the `usehastype` option to "Skip".
##  Skipping stops when the given type appears in a computed result.
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
##opt(traperror,truefalse\comma string\comma or set of string)
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
##opt(view,truefalse)
##  If true, the remote debugging session is echoed on the client machine.
##  This only has an effect with command-line maple.
##  The default value is false; this option is sticky.
##
##ENDSUBSECTION
##
##EXAMPLES(noexecute,notest)
##- Launch the Maple debugger client, instrumenting "int".
##  Assume the Maple Debugger Server is running on a different
##  machine, named `gauss`.
##> mdc(int, host="gauss"):
##SET(lead=indent)
##- ~Welcome joe~
##UNSET
##- Now, call _int_, which invokes the debugger.
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
##> mdc([fib,1,'n=10']):
##>(noexecute) fib(20);
##- Clear the conditional breakpoint.
##> mdc(unstopat=[fib,1]):
##- Use "mdc[Count]" in a conditional breakpoint to begin debugging on the eighth call to fib.
##  Forward quotes are used to prevent premature evaluation.
##> mdc([fib,1,'mdc:-Count()=8']):
##>(noexecute) fib(10);
##
##- Stop on any warning.
##> mdc(stopwarning=true);
##>(noexecute) int(1/x, x=a..1);
##- Clear the stop-on-warning.
##> mdc(stopwarning=false);
##
##- Debug a builtin procedure.  This is occasionally be useful for verifying
##  the arguments passed to a builtin procedure.
##
##> mdc(convert, 'debug_builtins'):
##> convert(<a,b,c>, list);
##
##- Using the 'unstopat' option reverts the builtin to its original assignment.
##> mdc(unstopat=convert);
##
##- Not all builtin procedures can be instrumented for debugging.
##  Those with special evaluation rules, and those used by the
##  debugger, cannot be debugged.
##
##
##
##SUBSECTION Skipping
##- Use the `skip_until` option to stop the debugger during an
##  integration of _x^2_ at the place where _x^3_ first appears
##  as the output of a statement.
##
##> mdc(int, skip_until=x^3);
##> int(x^2, x);
##
##- The `skip_check_stack` option, which is used with `skip_until`,
##  causes expressions on the stack to be passed to the predicate.
##  This can locate expressions that are not otherwise
##  part of a computed result.  Consider the following example.
##
##> f := x -> g(2*x)*x:
##> g := x -> x^2:
##
##- Use `skip_until` to search for the _2*y_ expression.
##
##> mdc(f, 'skip_until' = 2*y):
##> f(y);
##
##- Normal skipping does not locate _2*y_.
##  Using `skip_check_stack` does.
##> mdc('skip_check_stack'):
##> f(y);
##
##- See "mdc[Skip]" for more examples using the skip feature.
##
##ENDSUBSECTION
##
##XREFMAP
##- "Maple Debugger Client" : Help:mdc
##- "Maple initialization file" : Help:worksheet,reference,initialization
##- "regular expressions" : Help:Regular_Expressions
##- "Skip" : Help:mdc,Skip
##
##SEEALSO
##- "mdc"
##- "debugger"
##- "mdc[Grid]"
##- "mdc[Count]"
##- "mdc[Monitor]"
##- "mdc[Skip]"
##ENDMPLDOC

#}}}

$define MDC # to allow minting w/out include path
$define MDS_DEFAULT_PORT 10\000

$include <src/tags.mm>

#$define LOG_READLINE # for debugging (need to close file)

unprotect('mdc'):
module mdc()

option package;

#{{{ exports
export Count
    ,  DataFile
    ,  Debugger
    ,  Format
    ,  Grid
    ,  InstallPatch
    ,  LineInfo
    ,  Monitor
    ,  Sleep
    ,  Skip
    ,  Version
    ,  HelpMDS
    ,  _pexports
    ;
#}}}

#{{{ local declarations

local Connect
    , Disconnect
    , CreateID
    , GetDefault
    , ModuleApply
    , ModuleLoad
    , ModuleUnload
    , PrintRtable   # module to "fix" `print/rtable`
    , WriteTagf

    , indexed2slashed
    , printf  # local versions so that globals can be 'debugged'
    , sprintf

    # Module-local variables.  Seems unlikely that the debugger
    # will ever be thread-safe, so this is not serious.

    , cnt
    , debugbuiltins := false
    , Host
    , match_predicate := () -> true
    , max_length
    , Port
    , Quiet := false
    , sid := -1
    , view_flag
    , show_options_flag
    , print_to_maple_flag
    , SkipCheckStack
    , SkipIndicateMatch
    , unlimited_flag := false
    , Warnings
    ;
#}}}

#{{{ _pexports

    _pexports := proc()
    local sys, excludes;
        sys := kernelopts('platform');
        excludes := [NULL
                     , ':-Debugger'
                     , ':-Format'
                     , ':-InstallPatch'
                     , ':-_pexports'
                     , `if`(sys = "windows" or sys = "dos"
                            , NULL
                            , ':-HelpMDS'
                           )
                    ];
        remove(member, [exports(':-mdc')], excludes);
    end proc;

#}}}
#{{{ Replace builtins: printf, sprintf

    printf := proc()
        iolib(9,'default',args);  # fprintf('default',args)
        iolib(23,'default');      # fflush('default')
        NULL;
    end proc;

    sprintf := proc() iolib(10,args) end proc;

#}}}

    Warnings := {}; # initialize


$include <src/DataFile.mm>
$include <src/Debugger.mm>
$include <src/Format.mm>
$include <src/Grid.mm>
$include <src/HelpMDS.mm>
$include <src/InstallPatch.mm>
$include <src/indexed2slashed.mm>
$include <src/LineInfo.mm>
$include <src/PrintRtable.mm>

#{{{ ModuleApply

    ModuleApply := proc( stopats :: seq({string,name,list})
                         , { debug :: truefalse := false }
                         , { debug_builtins :: truefalse := debugbuiltins }
                         , { emacs :: {string,procedure} := GetDefault(':-emacs', "emacs") }
                         , { exit :: truefalse := false }
                         , { goback :: truefalse := false }
                         , { host :: string := GetDefault(':-host',"localhost") }
                         , { ignoretester :: truefalse := GetDefault(':-ignoretester',true) }
                         , { label :: string := kernelopts('username') }
                         , { launch_emacs :: truefalse := GetDefault(':-launch_emacs',false) }
                         , { maxlength :: nonnegint := GetDefault(':-maxlength',10\000) }
                         , { port :: posint := GetDefault(':-port',MDS_DEFAULT_PORT) }
                         , { print_to_maple :: truefalse := GetDefault(':-print_to_maple',false) }
                         , { quiet :: truefalse := GetDefault(':-quiet',Quiet) }
                         , { reconnect :: truefalse := false }
                         , { showoptions :: {truefalse,identical(ignore)} := GetDefault(':-showoptions','ignore') }
                         , { stopat :: {string,name,list,set({string,name,list})} := "" }
                         , { stoperror :: {truefalse,string,set} := false }
                         , { stopwarning :: {string,set(string),truefalse} := NULL }
                         , { stopwhen :: { name, list, set } := NULL }
                         , { stopwhenif :: { list, set(list) } := NULL }
                         , { skip_before :: string := NULL }
                         , { skip_until := NULL }
                         , { `skip_until[alloc]` :: posint := NULL }
                         , { `skip_until[exact]` := NULL }
                         , { `skip_until[level]` :: posint := NULL }
                         , { `skip_until[loc]` := NULL }
                         , { `skip_until[type]` := NULL }
                         , { skip_check_stack :: truefalse := SkipCheckStack }
                         , { skip_indicate_match :: truefalse := SkipIndicateMatch }
                         , { traperror :: {truefalse,string,set} := false }
                         , { unstopat :: {string,name,list,set(string,name,list)} := "" }
                         , { unstoperror :: {truefalse,string,set,identical(true)} := false }
                         , { unstopwhen :: { name, list, set } := NULL }
                         , { untraperror :: {truefalse,string,set,identical(true)} := false }
                         , { view :: truefalse := GetDefault(':-view',false) }
                         , $
                       )

    global `debugger/width`, WARNING;
    local lbl,str,stp;

        #{{{ exit

        if exit then
            ModuleUnload();
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
        Quiet := quiet;
        SkipCheckStack := skip_check_stack;
        SkipIndicateMatch := skip_indicate_match;

        view_flag := view; # and not IsWorksheetInterface();
        max_length := maxlength;

        #{{{ print_to_maple
        print_to_maple_flag := print_to_maple;
        #}}}
        #{{{ max_length

        if max_length > 0 then
            `debugger/width` := max_length;
        end if;

        #}}}
        #{{{ assign lbl

        lbl := label;

        #}}}
        #{{{ showoptions
        if showoptions :: truefalse then
            show_options_flag := showoptions;
        end if;
        #}}}
        #{{{ skip_until (clear skip)

        if skip_until <> NULL then
            Skip(skip_until);
        elif `skip_until[alloc]` <> NULL then
            Skip('bytesalloc' = `skip_until[alloc]`);
        elif `skip_until[level]` <> NULL then
            Skip('stacklevel' = `skip_until[level]`);
        elif `skip_until[loc]` <> NULL then
            Skip(`skip_until[loc]`, 'matchlocals');
        elif `skip_until[type]` <> NULL then
            Skip(`skip_until[type]`, 'usehastype');
        elif `skip_until[exact]` <> NULL then
            Skip(`skip_until[exact]`, 'exact');
        end if;

        Debugger:-Skip('clear');

        #}}}
        #{{{ skip_before

        if skip_before <> NULL then
            Skip(skip_before, 'before');
        end if;

        #}}}
        #{{{ replace debugger

        Debugger:-Replace();

        #}}}

        #{{{ connect to server

        if sid = -1 or reconnect then

            try
                Connect(host, port, CreateID(lbl)
                        , _options['emacs']
                        , _options['launch_emacs']
                        , _options['quiet']
                       );
            catch:
                Debugger:-Restore();
                error;
            end try;

        end if;

        #}}}

        if goback then
            Debugger:-GoBack();
        end if;

        #{{{ stopat

        if stopat :: set then
            map(Debugger:-stopat, stopat);
        elif stopat <> "" then
            Debugger:-stopat(stopat);
        end if;

        if stopats <> NULL then
            for stp in [stopats] do
                Debugger:-stopat(stp);
            end do;
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
                    print('INTERFACE_WARN'(1,msg,args[2 .. -1]))
                end proc;
                Warnings := {};
            else
                WARNING := proc()
                local msg, WARNING;
                    msg := StringTools:-FormatMessage(_passed);
                    if ormap(StringTools:-RegMatch, Warnings, msg) then
                        WARNING := proc(msg::{string, symbol})
                            # This is done to prevent showstop from
                            # detecting this as a stop point.
                            ``||"DEBUG"();
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

        #{{{ return first procedure

        if stopats = NULL then
            if debug then
                DEBUG();
            end if;
        else
            stp := [stopats][1];
            if stp :: list then
                stp := stp[1];
            end if;
            return Debugger:-GetName(stp);
        end if;
        #}}}


    end proc;

#}}}

#{{{ ModuleLoad

ModuleLoad := proc()
    debugbuiltins := GetDefault(':-debug_builtins', false);
    SkipCheckStack := GetDefault(':-skip_check_stack', false);
    SkipIndicateMatch := GetDefault(':-skip_indicate_match', true);
    TypeTools:-AddType('synonym'
                       , proc(x,nm)
                             x::symbol and length(x)=length(nm) and SearchText(x,nm)=1;
                         end proc
                      );
    if IsWorksheetInterface('Standard') then
        PrintRtable:-Replace();
    end if;
end proc;

#}}}
#{{{ ModuleUnload

ModuleUnload := proc()
    Disconnect(':-quiet' = Quiet);
    TypeTools:-RemoveType('synonym');
    Debugger:-RestoreBuiltins();
    Debugger:-Restore();
    PrintRtable:-Restore();
    return NULL;
end proc;

#}}}

#{{{ Connect

##DEFINE PROC Connect
##PROCEDURE mdc[Connect]
##HALFLINE initiate a connection to a Maple debugger server

Connect := proc(host :: string
                , port :: posint
                , id :: string
                , { emacs :: {string,procedure} := "emacs" }
                , { launch_emacs :: truefalse := false }
                , { quiet :: truefalse := false }
                , { verbose :: truefalse := false }
                , $
               )
local cmd,connected,line;

    Debugger:-Reset();
    Disconnect(_options['quiet']);

    try
        sid := Sockets:-Open(host, port);
    catch:
        if launch_emacs then
            if emacs :: procedure then
                emacs();
            else
                cmd := sprintf("%s", emacs);
                try
                    system['launch'](cmd);
                catch:
                    error "problem launching emacs";
                end try;
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
    catch "invalid socket ID", "argument does not refer to an open socket connection":
        error "cannot connect to Debugger server.  Server may not be running."
    end try;

    if line = "userid: " then
        Sockets:-Write(sid, id);
        line := Sockets:-Read(sid);
        if not quiet then
            printf("%s\n", line);
        end if;
        if verbose then
            printf("Connected to %s on port %d, with id %s\n"
                   , host, port, id );
        end if;
        return NULL;
    end if;


end proc;

#}}}
#{{{ Disconnect

##DEFINE PROC Disconnect
##PROCEDURE mdc[Disconnect]
##HALFLINE terminate connection to Maple debugger server

Disconnect := proc( { quiet :: truefalse := false } )
    if sid <> -1 then
        if not quiet then
            printf("goodbye\n");
        end if;
        try
            Sockets:-Close(sid);
        catch "argument does not refer to an open socket connection":
        end try;
        sid := -1;
    end if;
end proc;

#}}}

#{{{ WriteTagf

##DEFINE PROC WriteTagf
##PROCEDURE mdc[WriteTagf]
##AUTHOR   Joe Riel
##DATE     Nov 2011
##CALLINGSEQUENCE
##- \PROC('tag','rest')
##PARAMETERS
##- 'tag'  : ::name::
##- 'rest' : (optional) arguments to "sprintf"
##RETURNS
##- `NULL`
##DESCRIPTION
##- Send a tagged message to the server.
##- The string that is sent has the form
##  ~tag lenlen [length msg]~,
##  where 'tag' is a single character identifying the message type,
##  `lenlen` is a single digit specifying the length of the LENGTH field,
##  `length` is a field of digits specifying the length of MSG,
##  and 'msg' is the message.
##
##- The message (`msg`) is created by passing the optional arguments to "sprintf".
##  If the length of the message exceeds ~max_length~, a short message
##  indicating what has been done, is substituted.

WriteTagf := proc(tag)
uses Write = Sockets:-Write;
local msg,len,lenlen;
    if _npassed = 1 then
        msg := "";
    else
        msg := sprintf(_rest);
    end if;
    len := length(msg);
    if unlimited_flag then
        unlimited_flag := false;
    elif tag <> TAG_SS_LIVE
    and tag <> TAG_SS_DEAD
    and tag <> TAG_UNLIMITED
    and 0 < max_length
    and max_length < len then
        msg := sprintf("%s... ---output too long (%d bytes)---\n", msg[1..100],len);
        len := length(msg);
    end if;
    lenlen := length(len);
    Sockets:-Write(sid, cat(`if`(tag=TAG_UNLIMITED
                                 , TAG_RESULT
                                 , tag
                                )
                            , lenlen
                            , `if`(lenlen=0
                                   , NULL
                                   , len
                                  )));
    Sockets:-Write(sid, msg);
    if view_flag then
        fprintf('INTERFACE_DEBUG',_rest);
    end if;
    return NULL;
end proc;

#}}}
#{{{ CreateID

##DEFINE PROC CreateID
##PROCEDURE mdc[CreateID]
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
##- Currently the format is ~:label:release:platform:pid:~, where
##  'label' is the argument, `release` is the major Maple release (a
##  posint), and `platform` and `pid` are the corresponding outputs of
##  "kernelopts".
##
##OPTIONS
##opt(pid,integer)
##  The process identifier.  The default is ~kernelopts(pid)~.
##opt(platform,string)
##  The platform.  The default is ~kernelopts(platform)~.
##opt(release,integer)
##  The major Maple release
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
## Try[LA]("1.1", FUNC("label"), ":label:16:unix:");
## Try[LA]("1.2", FUNC("abc12"), ":abc12:16:unix:");
## Try[LA]("1.3", FUNC("abc-1_2_"), ":abc-1_2_:16:unix:");
## Try[NLA]("2.1", FUNC("abc-1_2_"), ":abc-1_2_:16:unix:0");
## Try[TE]("10.0", FUNC(""), "label cannot be empty");
## msg := "invalid characters in label":
## Try[TE]("10.1", FUNC("+"), msg);
## Try[TE]("10.2", FUNC(" "), msg);
## Try[TE]("10.3", FUNC("\n"), msg);


CreateID := proc(label :: string
                 , { platform :: string := kernelopts(':-platform') }
                 , { pid :: integer := kernelopts(':-pid') }
                 , { release :: integer := "DEFAULT" }
                 , $
                )
local ver;
    if length(label) = 0 then
        error "label cannot be empty";
    elif not StringTools:-RegMatch("^[][A-Za-z0-9_-]+$", label) then
        error "invalid characters in label '%1'", label;
    end if;
    if release = "DEFAULT" then
        ver := kernelopts('version');
        ver := substring(ver, SearchText(" ",ver)+1 .. SearchText(".",ver)-1);
        ver := parse(ver);
    else
        ver := release;
    end if;
    return sprintf(":%s:%d:%s:%d:"
                   , label
                   , ver
                   , platform
                   , pid
                  );
end proc;

#}}}

#{{{ Version

Version := "2.4.2";

#}}}

#{{{ Sleep

##DEFINE CMD Sleep
##PROCEDURE(help) mdc[Sleep]
##HALFLINE pause execution of the engine
##AUTHOR   Joe Riel
##DATE     Aug 2011
##CALLINGSEQUENCE
##- \CMD('t')
##PARAMETERS
##- 't' : ::nonnegint::; number of seconds to sleep
##DESCRIPTION
##- The `\CMD` command pauses the execution of the Maple engine
##  a specified length of time.  While paused, it does not use CPU
##  resources.
##
##- The 't' parameter is the duration to pause, in seconds.
##
##- This routine is used internally when connecting to the Debugger
##  server.  It is provided here with the hope that it might
##  be useful elsewhere.
##
##EXAMPLES
##> mdc:-Sleep(1);
##
##SEEALSO
##- "Threads[Sleep]"
##- "mdc"

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
#{{{ Skip

##DEFINE CMD Skip
##PROCEDURE(help) mdc[Skip]
##HALFLINE assign the skip predicate
##AUTHOR   Joe Riel
##DATE     Jan 2012
##CALLINGSEQUENCE
##- \CMD('ex','opts')
##PARAMETERS
##- 'ex' : (optional) ::anything::
##param_opts(\CMD)
##RETURNS
##- `procedure`
##DESCRIPTION
##desc_skipping
##
##- The `\CMD` command
##  assigns the predicate used when skipping.
##  This is equivalent to using the `skip_until` option to `mdc`.
##
##- The newly assigned predicate is returned.
##  If \CMD is called with no arguments,
##  the current predicate is returned.
##
##- If the 'ex' parameter is a procedure (but not a name),
##  it is used as the predicate,
##  otherwise, the predicate is the procedure
##  ~proc() has([_passed],ex) end proc~.
##
##- When skipping, the result of each executed statement is
##  passed to the predicate.  If the predicate returns false,
##  skipping continues.  If it returns true, skipping stops
##  and a generic message is displayed in the debugger output window.
##  If it returns any other value, skipping stops and that
##  value is incorporated into the printed message.
##
##- The arguments passed to the predicate consist of
##  the result of each executed statement.
##
##- This procedure can be called from inside the debugger to
##  assign a predicate during a debugging session.
##
##NOTES
##-(nolead)
##  If the skip predicate is not satisfied, Maple continues executing
##  the code until the debugger exits.  The internal flag used to
##  enable skipping remains active, so skipping resumes when the
##  debugger is reentered.  To avoid this, clear the skipping flag by
##  calling ~mdc()~ from Maple.
##
##OPTIONS
##-(nolead) Some of the options are mutually exclusive.
##
##opt(before,truefalse)
##  Match 'ex', which must be a string, against the Maple code
##  corresponding to the next statement to execute; if it matches,
##  stop skipping.  This option works differently than the others in
##  that it does not generate a predicate.  It returns the previous
##  target of `before`.
##
##opt(bytesalloc,posint)
##  When passed, skip until ~kernelopts(':-bytesalloc')~ exceeds
##  the current setting by 'bytesalloc'.
##opt(exact,truefalse)
##  When true, the predicate is ~proc() evalb(_passed = ex) end proc~.
##  An exact match is efficient.
##  The default is false.
##opt(matchlocals,truefalse)
##  When true, any locals in the expression being tested are replaced
##  with globals before testing.  This option can be used by itself,
##  in which case the test using `has` is performed, or with `exact` option.
##  The default is false.
##opt(stacklevel,posint)
##  When passed, skip until _stacklevel < kernelopts(:-level)_.
##opt(usehastype,truefalse)
##  When true, the predicate is ~proc() hastype([_passed],ex) end proc~.
##  The default value is false.
##
##EXAMPLES(noexecute,notest)
##SUBSECTION Locate the source of an expression
##
##-(nolead)
##  Determine the exact point
##  where _x^2_ is computed when symbolically integrating _x_
##  with respect to _x_.
##-(nolead) Call \CMD with parameter _x^2_, that assigns the desired skip predicate.
##> mdc:-Skip(x^2):
##-(nolead) Instrument "int" for debugging.
##> mdc(int):
##-(nolead) Call `int`, which launches the debugger.
##  To start the skipping, type **S** in the debugger.
##> int(x,x);
##
##- Use the `usehastype` option to stop skipping when a square (power
##  of two) appears while doing the integration; this occurs before
##  the appearance of _x^2_.  Use "forget" to clear `int`\'s remember table.
##> mdc:-Skip(anything^2, usehastype):
##> forget(int):
##> int(x,x);
##
##- Use the `exact` option.
##> mdc:-Skip(1/2*x^2,'exact');
##> forget(int):
##> int(x,x);
##
##- Create a custom predicate with "patmatch".
##  Note that we first verify that ~_npassed=1~; without that,
##  a statement that returns `NULL` raises an error because
##  the wrong number of arguments are passed to `patmatch`.
##> mdc:-Skip(() -> _npassed=1 and patmatch(_passed,a::algebraic*x^2)):
##> forget(int):
##> int(x,x);
##
##ENDSUBSECTION
##SUBSECTION Stop before executing a particular statement
##-(nolead)
##  Use the `before` option to stop skipping before the
##  `INTERFACE_PLOT3D` streamcall is made.
##
##> mdc:-Skip("INTERFACE_PLOT3D",'before');
##> mdc(plot3d):
##> plot3d(sin(x)*sin(y),x=0..Pi,y=0..Pi);
##ENDSUBSECTION
##SUBSECTION Locate the source of intensive memory usage
##
##-(nolead)
##  Set a threshold on the memory allocated.
##  This is useful for quickly locating bottlenecks in a program.
##  When, after the debugger returns from skipping,
##  the stack (displayed with the 'K' key) contains
##  the statements that initiated the excessive usage.
##
##> restart;
##- Assign a procedure that inefficiently increase the sign of
##  a Vector a term at a time by constructing an entirely new Vector.
##>> ugly := proc(n)
##>> local i,x,V;
##>>    V := Vector();
##>>    for i to n do
##>>        V := Vector([seq(x,x=V),i]);
##>>    end do;
##>>    V;
##>> end proc:
##- The ~skip_until[alloc]~ option to `mdc`
##  is equivalent to calling `Skip` with
##  option `bytesalloc`.
##> mdc(ugly, skip_until[alloc]=10^8):
##> ugly(1000);
##
##- The proper way to increase the size of a Vector is
##  to use Maple's dynamic resizing ability.  Here
##  is the equivalent procedure
##>> nice := proc(n)
##>> local i,V;
##>>    V := Vector();
##>>    for i to n do
##>>        V(i) := i;
##>>    end do;
##>>    V;
##>> end proc:
##- The memory usage is minimal
##> CodeTools:-Usage(nice(1000));
##ENDSUBSECTION
##EXAMPLES(execute,notest)
##SUBSECTION Locate a stack overflow
##SET(noexecute)
##-(nolead)
##  Set a threshold on the size of the stack.  This is useful for
##  catching a *stack overflow* while it is occurring.
##
##> restart;
##> mdc:-Skip('stacklevel' = 1000):
##> f := proc(x) kernelopts('level'); 1 + procname(x+1) end proc:
##- This call generates a stack overflow.
##> f(1);
##SET(nolead,noshow,execute)
##- ``
##> printf("%s\n", "Error (in f) too many levels of recursion"):
##UNSET
##> mdc(f):
##- Call `f` again.  When the debugger starts, type **S** to begin
##  skipping.
##> f(1);
##ENDSUBSECTION
##
##SUBSECTION Match an expression with a local variable
##-(nolead)
##  Assign a procedure that computes an expression, ~exp(x)~,
##  with a local variable, ~x~, that we want to locate using skipping.
##>> f := proc()
##>> local x,y;
##>>     y := exp(x);
##>>     y^2;
##>> end proc:
##
##- Use the `matchlocals` option.
##> mdc:-Skip(exp(x), 'matchlocals'):
##> mdc(f):
##> f();
##ENDSUBSECTION
##
##SUBSECTION Printing a value on exit
##-(nolead)
##  If the predicate returns a non-boolean expression, the skipping
##  halts and the expression is displayed.  This can be used to
##  provide a useful message.  The following example illustrates the
##  use, if not the usefulness.
##
##>> timeskip := proc()
##>> local t;
##>>    t := time();
##>>    if t < 3 then
##>>        false;
##>>    else
##>>        sprintf("computation time was %a seconds", t);
##>>    end if;
##>> end proc:
##- Assign a procedure that loops endlessly.
##>> f := proc()
##>> local cnt;
##>>     cnt := 0;
##>>     do
##>>         cnt := cnt+1;
##>>     end do;
##>> end proc:
##- Instrument `f`.  Use the `skip_until` option to assign
##  the skip predicate (it calls `Skip`).  Note the
##  use of "eval" around the skip predicate (`timeskip`);
##  without that `timeskip` would be a name and the generated
##  predicate would become true only if that name appeared
##  as an output of a statement---that will not happen.
##> mdc(f, skip_until = eval(timeskip)):
##> f();
##
##ENDSUBSECTION
##UNSET
##SEEALSO
##- "mdc"
##- "mdc[ModuleApply]"

Skip := proc(ex := NULL
             , { before :: truefalse := false }
             , { bytesalloc :: posint := NULL }
             , { stacklevel :: posint := NULL }
             , { usehastype :: truefalse := false }
             , { exact :: truefalse := false }
             , { matchlocals :: truefalse := false }
            )
local alloc_orig, alloc_max;

    if before then
        if not ex :: string then
            error ( "target must be a string when using 'before' option, "
                    "received %1", ex );
        end if;
        return Debugger:-SkipBefore(ex);

    elif bytesalloc <> NULL then

        alloc_orig := kernelopts(':-bytesalloc');
        alloc_max := alloc_orig + bytesalloc;

        match_predicate := proc()
        local alloc;
            alloc := kernelopts(':-bytesalloc');
            if alloc_max < alloc then
                sprintf("allocated bytes increased by %a"
                        , alloc - alloc_orig
                       );
            else
                false;
            end if;
        end proc;


    elif stacklevel <> NULL then

        match_predicate := subs('_S' = stacklevel+51  # 48 = 20+4+4+20, but 51 is better
                                , proc()
                                  local lev;
                                      lev := kernelopts(':-level');
                                      if _S < lev then
                                          sprintf("stack level [%d] exceeded %d"
                                                  , lev - 51
                                                  , stacklevel
                                                 );
                                      else
                                          false;
                                      end if;
                                  end proc
                               );
    elif ex = NULL then

    elif exact then
        if matchlocals then
            match_predicate := proc()
            local n;
                _npassed > 0 and [ex] = subs([seq(n=cat('``',n), n=indets([_passed],`local`))],[_passed]);
            end proc;
        else
            match_predicate := proc() evalb(_passed = ex) end proc;
        end if;

    elif usehastype then
        if not ex :: type then
            error "argument must be a type when using hastype, received '%1'", ex;
        end if;
        match_predicate := proc() hastype([_passed],ex) end proc;

    elif matchlocals then
        match_predicate := proc()
        local n;
            _npassed > 0 and has(subs([seq(n=cat('``',n), n=indets([_passed],`local`))],[_passed]),ex);
        end proc;

    elif ex :: 'And(procedure,Not(name))' then
        match_predicate := eval(ex);

    else
        match_predicate := proc() has([_passed],ex) end proc;
    end if;

    return eval(match_predicate);

end proc;

#}}}
#{{{ Count

##DEFINE CMD Count
##PROCEDURE(help) mdc[Count]
##HALFLINE increment a counter
##AUTHOR   Joe Riel
##DATE     Sep 2011
##CALLINGSEQUENCE
##- \CMD('indx1','indx2',...,'opts')
##PARAMETERS
##- 'indxk' : (optional) arguments used to identify counter
##param_opts(\CMD)
##DESCRIPTION
##- The `\CMD` command increments a counter and returns the result.
##  It is intended to be used with the conditional form of the
##  `stopat` option to `mdc` to stop the debugger inside a
##  procedure after a specified number of calls.
##
##- The optional parameters 'indx1',...,'indx2' are used
##  as a single index to a table that stores the count value.
##  It is allowable to use no indices, which is the usual case
##  if you need only one counter.
##
##- The counter incremented is local to the `mdc` module.  Different
##  counters can be specified by passing arbitrary arguments
##  ('indices') to \CMD.
##
##OPTIONS
##opt(reset,truefalse)
##  If true, clear all the counters.
##  The default value is false, which does not clear counters.
##
##opt(value,truefalse)
##  If true, return the current value of the counter
##  associated with 'indices'.
##
##opt(indices,truefalse)
##  If true, return the assigned indices
##  as a sequence of lists.
##
##EXAMPLES(notest)
##> with(mdc):
##- Exercise a couple of counters.
##> Count(), Count(),Count(12), Count(), Count(12);
##- Get the sequence of indx's
##> indxs := [Count('indices')];
##- Get the associated values.
##> [seq(Count(op(k),'value'), k=indxs)];
##- Reset all counters.
##> Count('reset');
##> Count('value');
##- Assign a procedure that calls itself endlessly.
##> f := proc(x) procname(x+1) end proc:
##- Configure debugging to begin at statement 1 of the 23rd call to f.
##  Forward-quotes are used to prevent premature evaluation of the condition.
##>(noexecute) mdc(stopat=[f, 1, 'Count()=23']);
##- Call `f`, which launches the debugger if `f` when the counter
##  reaches 23.  When inside the debugger, type `q` to quit.
##>(noexecute) f(0);
##- Verify that the counter reached 23.
##>(noexecute) Count('value');
##<(show) 23
##> Count(reset);
##SEEALSO
##- "mdc"
##- "mdc[ModuleApply]"
##- "stopat"
##
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdc:-Count):
## macro(AS='verify,as_set');
##
## Try("1.0", [FUNC(),FUNC(),FUNC(1)], [1,2,1]):
## Try("1.1", [FUNC('value'),FUNC(1,'value'),FUNC(0,'value')],[2,1,0]):
## Try[AS]("1.2", [FUNC('indices')], [[],[1]]);
## Try("1.3", FUNC('reset'));

Count := proc( { reset :: truefalse := false }
               , { value :: truefalse := false }
               , { indices :: truefalse := false }
             )
    if reset then
        cnt := table();
        return NULL;
    elif value then
        if assigned(cnt[_rest]) then
            return cnt[_rest];
        else
            return 0;
        end if;
    elif indices then
        return :-indices(cnt);
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
#{{{ Monitor

Monitor := Debugger:-Monitor;

#}}}

end module:

protect('mdc'):

#savelib('mdc'):
