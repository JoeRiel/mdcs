send_packets := proc(len :: posint
                     , num :: posint
                     , { char :: string := NULL }
                     , { secs :: numeric := 0   }
                     , $
                    )

uses Sockets,Grid;
local sid,packet;
    sid := Open("localhost", 20\000);
    if char <> NULL then
        packet := StringTools:-Repeat(char,len);
    else
        packet := StringTools:-Random(len);
    end if;
    to num do
        Write(sid, packet);
        if secs > 0 then
            ssystem(sprintf("sleep %f", secs));
        end if;
    end do;
    Close(sid);
    NULL;
end proc:

