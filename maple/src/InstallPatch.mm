##INCLUDE ../include/mpldoc_macros.mpi
##PROCEDURE mdc[InstallPatch]
##HALFLINE install a patched procedure
##AUTHOR   Joe Riel
##DATE     Oct 2011
##CALLINGSEQUENCE
##- InstallPatch('addr','prc')
##PARAMETERS
##- 'addr' : ::integer::; Maple address of procedure to replace
##- 'prc'  : ::procedure::; new procedure
##RETURNS
##- `NULL`
##DESCRIPTION
##- The `InstallPatch` command replaces a procedure.
##  This command is used by the Maple debugger server (mds) and
##  is not intended to be directly called by users.
##
##- The 'addr' parameter is the address of the
##  procedure that is to be replaced.
##
##- The 'prc' parameter is the new procedure.
##
##NOTES
##- If the replaced procedure uses module-local variables, it will be
##  generally be necessary to assign ~kernelopts(opaqumodules=false)~
##  to use it.
##
##TEST
## $include <test_macros.mi>
## AssignFUNC(InstallPatch);
## macro(NE=testnoerror):
##
## Try[NE]("1.0", proc() "hello, world"; end proc, 'assign'="P");
## Try[NE]("1.1", FUNC(addressof(P), proc() "goodbye"; end proc));
## Try("1.2", eval(P), proc() "goodbye" end proc);
##

InstallPatch := proc(addr, prc)
local nm,prot;
    nm := pointto(addr);
    prot := type(nm,protected);
    if prot then
        unprotect(nm);
    end if;
    proc(nm, prc) nm := prc end proc(pointto(addr), prc);
    if prot then
        protect(nm);
    end if;
    return NULL;
end proc;
