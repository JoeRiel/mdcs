##INCLUDE ../include/mpldoc_macros.mpi
##MODULE(help) mdc[Grid]
##HALFLINE submodule for debugging Grid-based procedures
##INDEXPAGE mdc[Exports],Grid,submodule for debugging Grid-based procedures
##AUTHOR   Joe Riel
##DATE     May 2011
##DESCRIPTION
##- The `mdc[Grid]` submodule exports procedures for instrumenting
##  code written for parallel computation using the "Grid"
##  package so that they can be debugged with the *Maple Debugger
##  Client*.
##
##- The exports are
##SHOWINDEX(table="mdc[Grid][Exports]")
##
##SEEALSO
##- "Grid"
##- "mdc"

Grid := module()

export CodeString
    ,  Procedure
    ;

##PROCEDURE(help) mdc[Grid][CodeString]
##HALFLINE instrument a block of code for use with Grid and mdc
##INDEXPAGE mdc[Grid][Exports],CodeString,instrument a block of code for use with Grid and mdc
##AUTHOR   Joe Riel
##DATE     Jun 2011
##CALLINGSEQUENCE
##- CodeString('defs', 'launch')
##PARAMETERS
##- 'defs'   : ::string::; code that defines procedures
##- 'launch' : ::string::; code that calls the procedures
##RETURNS
##- ::string::; string of code to be passed to "Grid[Launch]"
##DESCRIPTION
##- The `CodeString` command
##  instruments a string of Maple code that is
##  passed to "Grid[Launch]" so that it
##  can be debugged with `mdc`.
##  It does so by returning a new string of Maple code
##  that contains a call to `mdc` just before
##  executing the code that launches the debugging.
##  This returned string can be passed to `Grid[Launch]`.
##
##- The 'defs' parameter is a string of code
##  that assigns the parallel procedures run with `Grid`.
##
##- The 'launch' parameter is a string of code
##  that executes the parallel procedures.
##
##- The remaining arguments are optional arguments
##  to `mdc`.  They initialize the client
##  and instrument the selected procedures.
##
##EXAMPLES(notest)
##- Assign a simple procedure to run in 'Grid'.
##>> hello := proc(nam :: string)
##>>  uses Grid;
##>>     printf("Hi, %s.  I'm node %d of %d\n", nam, MyNode(),NumNodes());
##>>     Barrier();
##>>  end proc:
##- Assign a string that, when parsed and evaluated, assigns the
##  previously defined `hello` procedure.
##> defs := sprintf("%a:=%a:", hello, eval(hello));
##
##- Assign the string that, when parsed and evaluated, calls `hello`.
##> launch := sprintf("hello(%a):", "Debugger");
##
##- Create the block of code, a string, that is passed to
##  ~Grid[Launch]~.  The 'stopat' option is passed to 'mdc'
##  so that the debugger stops in the `hello` procedure.
##> Code := mdc:-Grid:-CodeString(defs, launch, stopat=hello);
##
##- Launch Grid with two nodes.
##  This initiates two independent debugging sessions
##  on the Maple Debugger Server.
##  In Emacs, issue the command ~mds-wm-display-all~
##  to display both debugging sessions.
##>(noexecute) Grid:-Launch(Code,numnodes=2);
##SEEALSO
##- "Grid"
##- "mdc"
##- "mdc[ModuleApply]"
##- "mdc[Grid]"
##- "mdc[Grid][Procedure]"

    CodeString := proc(defs :: string, launch :: string)
        cat(defs
            , sprintf("mdc('label'=\"%s_%d\",%q):"
                      , kernelopts('username,pid')
                      , _rest)
            , launch
           );

    end proc:

##PROCEDURE(help) mdc[Grid][Procedure]
##HALFLINE instrument a procedure for use with Grid and mdc
##INDEXPAGE mdc[Grid][Exports],Procedure,instrument a procedure for use with Grid and mdc
##AUTHOR   Joe Riel
##DATE     Jun 2011
##CALLINGSEQUENCE
##- Procedure('prc', 'opts')
##PARAMETERS
##- 'prc'  : procedure
##param_opts(mdc)
##RETURNS
##- ::procedure::;
##DESCRIPTION
##- The `Procedure` command
##  wraps a procedure, 'prc', in another procedure that
##  can be passed to "Grid[Launch]" and which launches `mdc`.
##
##- The 'prc' parameter is the procedure to execute.
##
##- The remaining arguments to `Procedure` are passed
##  to `mdc`.
##
##- The wrapper procedure passes its arguments to 'prc'.
##
##EXAMPLES(noexecute,notest)
##- Assign a simple procedure that greets each process.
##  Note that while using "printf" is appropriate for normal usage,
##  it currently does not work well with the debugger in that the
##  print stream does not flow to the debugger but rather to
##  the Maple display.
##>> hello := proc(nam :: string)
##>> uses Grid;
##>>    printf("Hi, %s.  I'm node %d of %d\n", nam, MyNode(),NumNodes());
##>>    Barrier();
##>> end proc:
##- Instrument `hello` for debugging with `mdc` in Grid.
##> Hello := mdc:-Grid:-Procedure(hello, stopat=hello);
##- Launch Grid and start the debugger.
##  In Emacs, issue the command ~mds-wm-display-all~
##  to display both debugging sessions.
##> Grid:-Launch(Hello, "Maple Debugger", numnodes=2);
##SEEALSO
##- "Grid"
##- "mdc"
##- "mdc[ModuleApply]"
##- "mdc[Grid]"
##- "mdc[Grid][CodeString]"

    Procedure := proc(prc :: procedure)
        subs("opts" = _rest
             , "prc" = eval(prc)
             , "lbl" = sprintf("%s_%d", kernelopts('username,pid'))
             , proc()
               local prc := "prc";
                   mdc('label' = "lbl", "opts");
                   prc(_rest);
               end proc
            );
    end proc;


##
end module:
