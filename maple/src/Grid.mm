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
##- 'defs'   : ::string::; code that assigns procedures
##- 'launch' : ::launch::; code that calls the procedures
##RETURNS
##- ::string::; string of code to be passed to "Grid[Launch]"
##DESCRIPTION
##- The `\CMD` command
##  instruments a string of code that is
##  passed to "Grid[Launch]" so that it
##  can be debugged with "mdc".
##
##- The 'defs' parameter is a string of code
##  that assigns the parallel procedures run with `Grid`.
##
##- The 'launch' parameter is a string of code
##  that calls one of the procedures assigned in 'defs'
##  with the starting arguments.
##
##- The remaining arguments are optional arguments
##  to "mdc[mdc]".  They initialize the client
##  and instrument the selected procedures.
##
##EXAMPLES(noexecute)
##- Assign the procedure that will be run in 'Grid'.
##>> hello := proc(nam :: string)
##>>  uses Grid;
##>>     printf("Hi, %s.  I'm node %d of %d\n", nam, MyNode(),NumNodes());
##>>     Barrier();
##>>  end proc:
##- Assign a string that, when parsed and evaluated, assigns 'hello'.
##> defs := sprintf("%a:=%a:", hello, eval(hello)):
##
##- Assign the string that. when parsed and evaluated, ~hello("Debugger")~.
##> launch := sprintf("hello(%a):", "Debugger");
##
##- Create the block of code, a string, that is passed to
##  ~Grid[Launch]~.  The 'stopat' option is passed to 'mdc'
##  to stop the debugger in the `hello` procedure.
##> Code := \MOD:-\SUBMOD:-\CMD(defs, launch, 'stopat'=hello);
##
##- Launch Grid.
##> Grid:-Launch(Code,'numnodes'=2);
##SEEALSO
##- "Grid"
##- "mdc[mdc]"
##- "mdc[Grid]"
##- "mdc[Grid][CodeBlock]"

    CodeString := proc(defs :: string, launch :: string)
        cat(defs
            , sprintf("mdc(%q,'usegrid'=true):", _rest)
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
##- \CMD('prc')
##PARAMETERS
##- 'prc'  : procedure
##RETURNS
##- ::procedure::;
##DESCRIPTION
##- The `\CMD` command
##  converts a procedure, 'prc', and arguments 'args',
##  to a string that can be passed to "Grid[Launch]".
##  The string includes debug code that launches "mdc"
##  for each Grid process.
##
##- The 'prc' argument is a procedure to execute.
##
##- The optional 'args' arguments are passed to 'prc' when it is
##  launched via "Grid[Launch]".
##
##OPTIONS
##opt(mdc_options,set)
##  Set of options for "mdc".  These are passed to `mdc`,
##  in addition to the option 'usegrid=true'.
##  The default is the empty set.
##
##EXAMPLES(noexecute)
##- Assign a simple procedure that greets each process.
##  Note that while using "printf" is appropriate for normal usage,
##  it currently does not work well with the debugger in that the
##  print stream does not flow to the debugger.
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
             , proc()
                   prc := _prc;
                   mdc(_opts, 'usegrid'=true);
                   prc(_rest);
               end proc
            );
    end proc;

#}}}

end module:
