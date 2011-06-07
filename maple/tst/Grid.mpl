dosome := proc()
uses Grid;
local i, me;
option trace;
    mdc();
    ssystem("sleep 1");
    DEBUG();
    me := MyNode();
    for i from 1 to 10^(me+3) do
        if i mod 10^5 = 0 then
            print(me,i);
        end if;
    end do;
    print(me,"done");
end proc:

go := proc()
    Grid:-Launch(dosome,numnodes=3);
end proc:
