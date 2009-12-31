fibonacci := proc(i :: integer )
option cache;
    if i < 1 then 0;
    elif i = 1 then 1;
    else
        procname(i-2) + procname(i-1);
    end if;
end proc:

stopat(fibonacci):
fibonacci(5);
