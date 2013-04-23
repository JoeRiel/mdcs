# Sample module used to demonstrate the lineinfo feature.

MyModule := module()
export A;
local  B;

    A := proc(x)
    local i,y;
        # initialize y to zero
        y := 0;
        # loop a few times
        for i to 23 do
            # call local procedure
            y := B(x,y);
        end do;
        return y; # return the precious result
    end proc;

    B := proc(x,y)
        # square and add
        x^2 + y;
    end proc;

end module:
