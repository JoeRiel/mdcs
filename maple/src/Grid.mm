##INCLUDE ../include/mpldoc_macros.mi
##DEFINE SUBMOD Grid
##MODULE(help) \MOD[\SUBMOD]
##HALFLINE submodule for handling Grid a Maple Debugger Server
##AUTHOR   Joe Riel
##DATE     May 2011

Grid := module()

##DEFINE CMD ProcToCode
##PROCEDURE(help) \MOD[\SUBMOD][\CMD]
##HALFLINE instrument a procedure for use with Grid
##AUTHOR   Joe Riel
##DATE     Jun 2011
##CALLINGSEQUENCE
##- \CMD('prc', 'args', 'opts')
##PARAMETERS
##- 'prc'  : name of procedure
##- 'args' : (optional) arguments to 'prc'
##param_opts(\CMD)
##RETURNS
##- ::string::; string of code to be passed to "Grid[Launch]"
##DESCRIPTION
##- The `\CMD` command
##  creates a string that instruments a procedure
##  for debugging with `\MOD` so that it can be
##  used with the "Grid[Launch]" command.
##
##OPTIONS
##opt_stopat
##opt_stoperror
##opt_traperror
##
##EXAMPLES
##- Assign a simple procedure that greets each process.
##  Note that while using "printf" is appropriate for normal usage,
##  it currently does not work well with the debugger in that the
##  print stream does not flow to the debugger.
##>> hello := proc(nam :: string)
##>> uses Grid;
##>>    printf("Hi, %s.  I'm node %d of %d\n", nam, MyNode(),NumNodes());
##>>    Barrier();
##>> end proc:
##- Instrument it for debugging with \MOD in Grid.
##> code := \MOD:-\SUBMOD:-\CMD(hello, "Maple Debugger", 'stopat'=hello):
##- Launch Grid.
##> Grid:-Launch(code, 'numnodes'=2);

export ProcToCode;

    ProcToCode := proc(prc :: And(name,procedure)
                       (*
                           consider making these a single option, 'mdcargs'
                         , { mdcargs := {} }
                       *)
                       , { stopat :: {string,name} := "" }
                       , { stoperror :: truefalse := false }
                       , { traperror :: truefalse := false }
                      )

        sprintf("%a:=%a;"   # assign the proc
                "mdc(%s);"  # instrument it
                "%a(%s);"   # call it, with arguments
                , prc, eval(prc)
                , sprintf("%q"
                          , _options['stopat']
                          , _options['stoperror']
                          , _options['traperror']
                         )
                , prc
                , sprintf("%q",_rest)
               );

    end proc:

end module:
