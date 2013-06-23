##INCLUDE ../include/mpldoc_macros.mpi
##DEFINE PROC indexed2slashed
##PROCEDURE mdc[indexed2slashed]
##HALFLINE convert indexed name to a slashed name
##AUTHOR   Joe Riel
##DATE     May 2010
##CALLINGSEQUENCE
##- \PROC('nm')
##PARAMETERS
##- 'nm' : ::name::; name to convert
##RETURNS
##- ::symbol::
##DESCRIPTION
##- Convert an indexed name to a slashed name.
##  For example, ~base[index]~ is converted to
##  ~`base/index`~.
##
##- If a name is not indexed it is returned as is.
##TEST
## $include <AssignFunc.mi>
## AssignFUNC(mdc:-indexed2slashed);
## $define NE testnoerror
### mdc(FUNC):
## Try("1.0", FUNC(a), a);
## Try("1.1", FUNC(a[1]), `a/1`);
## Try("1.2", FUNC(a[1][2]), `a/1/2`);
##
## Try("3.1", FUNC(`a+b`[1][2]), `a+b/1/2`);
##
## $define TE testerror
## err := "cannot convert %1 to a slashed name":
## Try[TE]("10.1", FUNC(a[]), err);
## Try[TE]("10.2", FUNC(a[1,2]), err);

indexed2slashed := proc(nm :: name, $)
    if nm :: indexed then
        if nops(nm) <> 1 then
            error "cannot convert %1 to a slashed name", nm;
        end if;
        return nprintf("%A/%A", procname(op(0,nm)), op(1,nm));
    else
        return nm;
    end if;
end proc;
