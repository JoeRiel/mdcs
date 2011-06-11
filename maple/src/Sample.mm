Sample := module()

export Choose
    ,  Error
    ,  Grid
    ;

local ModuleApply;

    ModuleApply := Choose;

    Choose := proc()
    local line;
        do
            printf("Enter \n"
                   " (1) to loop\n"
                   " (2) to test Grid\n"
                   " (3) to raise an error\n"
                   " (4) to issue a warning \n"
                   " (5) to exit\n"
                  );

            # readline
            line := sprintf("%d", RandomTools:-Generate(integer(range=1..5)));

            if   line = "2" then Grid();
            elif line = "3" then Error();
            elif line = "4" then WARNING("this is a warning");
            elif line = "5" then "Finished with Sample";
            end if;
        end do;

    end proc;

    Error := proc()
    local i;
        for i to 4 do
            if i = 2 then
                try
                    error "a trapped error: i=%d", i;
                catch:
                end try;
            elif i = 3 then
                error "an untrapped error: i=%d", i;
            end if;
        end do;
    end proc;

    Grid := proc()
        print("not yet implemented");
    end proc;

end module:
