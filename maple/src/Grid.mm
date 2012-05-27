##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE SUBMOD Grid
##MODULE(help) \MOD[\SUBMOD]
##HALFLINE submodule for debugging Grid-based procedures
##AUTHOR   Joe Riel
##DATE     May 2011
##DESCRIPTION
##- The `\MOD[\SUBMOD]` submodule exports procedures for instrumenting
##  code written for parallel computation using the "Grid"
##  package so that they can be debugged with the *Maple Debugger
##  Client*.
##
##- The exports are
##
##-- "\MOD[\SUBMOD][CodeString]" : instrument a block of code
##-- "\MOD[\SUBMOD][Procedure]" : instrument a procedure
##
##SEEALSO
##- "Grid"
##- "mdc"

Grid := module()

export CodeString
    ,  Procedure
    ;

#{{{ CodeString

##DEFINE CMD CodeString
##PROCEDURE(help) \MOD[\SUBMOD][\CMD]
##HALFLINE instrument a block of code for use with Grid and mdc
##AUTHOR   Joe Riel
##DATE     Jun 2011
##CALLINGSEQUENCE
##- \CMD('defs', 'launch')
##PARAMETERS
##- 'defs'   : ::string::; code that defines procedures
##- 'launch' : ::string::; code that calls the procedures
##RETURNS
##- ::string::; string of code to be passed to "Grid[Launch]"
##DESCRIPTION
##- The `\CMD` command
##  instruments a string of code that is
##  passed to "Grid[Launch]" so that it
##  can be debugged with "mdc".
##  It does so by returning a new string of code
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
##  to "mdc[mdc]".  They initialize the client
##  and instrument the selected procedures.
##
##EXAMPLES(noexecute,notest)
##- Assign a simple procedure that will be run in 'Grid'.
##>> hello := proc(nam :: string)
##>>  uses Grid;
##>>     printf("Hi, %s.  I'm node %d of %d\n", nam, MyNode(),NumNodes());
##>>     Barrier();
##>>  end proc:
##- Assign a string that, when parsed and evaluated, assigns the
##  previously defined `hello` procedure.
##> defs := sprintf("%a:=%a:", hello, eval(hello))
##
##- Assign the string that, when parsed and evaluated, calls `hello`.
##> launch := sprintf("hello(%a):", "Debugger");
##
##- Create the block of code, a string, that is passed to
##  ~Grid[Launch]~.  The 'stopat' option is passed to 'mdc'
##  so that the debugger stops in the `hello` procedure.
##> Code := \MOD:-\SUBMOD:-\CMD(defs, launch, 'stopat'=hello);
##
##- Launch Grid with two nodes.
##  This initiates two independent debugging sessions
##  on the Maple Debugger Server.  They are grouped
##  together so that both can be readily observed and controlled.
##> Grid:-Launch(Code,'numnodes'=2);
##SEEALSO
##- "Grid"
##- "mdc"
##- "mdc[mdc]"
##- "mdc[Grid]"
##- "mdc[Grid][Procedure]"

    CodeString := proc(defs :: string, launch :: string)
        cat(defs
            , sprintf("mdc('label'=\"%s_%d\",%q,'usegrid'=true):"
                      , kernelopts('username,pid')
                      , _rest)
            , launch
           );

    end proc:

#}}}
#{{{ Procedure

##DEFINE CMD Procedure
##PROCEDURE(help) \MOD[\SUBMOD][\CMD]
##HALFLINE instrument a procedure for use with Grid and mdc
##AUTHOR   Joe Riel
##DATE     Jun 2011
##CALLINGSEQUENCE
##- \CMD('prc', 'opts')
##PARAMETERS
##- 'prc'  : procedure
##param_opts(mdc)
##RETURNS
##- ::procedure::;
##DESCRIPTION
##- The `\CMD` command
##  wraps a procedure, 'prc', in another procedure that
##  can be passed to "Grid[Launch]" and which launches "mdc".
##
##- The 'prc' parameter is the procedure to execute.
##
##- The remaining arguments to `\CMD` are passed
##  to "mdc[mdc]".
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
##> Hello := \MOD:-\SUBMOD:-\CMD(hello, 'stopat'=hello);
##- Launch Grid.
##> Grid:-Launch(Hello, "Maple Debugger", 'numnodes'=2);
##SEEALSO
##- "Grid"
##- "mdc[mdc]"
##- "mdc[Grid]"
##- "mdc[Grid][CodeBlock]"

    Procedure := proc(prc :: procedure)
        subs('_opts' = _rest
             , '_prc' = eval(prc)
             , '_lbl' = sprintf("%s_%d", kernelopts('username,pid'))
             , proc()
                   prc := _prc;
                   mdc('label' = _lbl
                       , _opts
                       , 'usegrid'=true
                      );
                   prc(_rest);
               end proc
            );
    end proc;

#}}}

end module:
