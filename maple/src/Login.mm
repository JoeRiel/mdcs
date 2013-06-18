Login := module()


export GeneratePassKey
    ,  GenerateResponse
    ,  GenerateSaltAndPassKey
    ,  SHA1
    ;
local Salt;

    macro(LENGTH = 40);

    GetSalt := proc(userid :: string)
    end proc;

    StoreSaltAndPassKey := proc( userid :: string
                                 , password :: string
                                 , $
                               )
    local saltkey;
        saltkey := GenerateSaltAndPassKey( password );
        Salt[userid] := saltkey[1];
        return saltkey[2];
    end proc;


    GenerateSaltAndPassKey := proc( password :: string, $ )
    local salt;
        # Should check complexity and reject. Is there a way to clear
        # the string from Maple's simplification table?  Hmm. That
        # will require some hackery.
        StringTools:-Randomize();
        salt := StringTools:-Random(LENGTH, alnum);
        [ salt, GeneratePassKey(salt,password) ];
    end proc;

    GeneratePassKey := proc(salt, password)
        SHA1(cat(salt,password));
    end proc;

    GenerateResponse := proc( userid :: string
                              , challenge :: string
                              , password :: string
                              , $ )
    local encrypt, passkey, rand, salt;

        if not length(challenge) = 2*LENGTH then
            error "challenge is wrong format";
        end if;

        salt := GetSalt(userid);
        rand := challenge[1..LENGTH];
        encrypt := challenge[LENGTH+1..-1];

        passkey := GeneratePassKey(salt, password);
        if encrypt <> SHA1(cat(rand,passkey)) then
            error "challenge is invalid";
        end if;
        return SHA1(cat(passkey, rand));
    end proc;

    SHA1 := proc( msg :: string )
        # for now I'll just call a shell command
        ssystem(sprintf("echo -n '%s' | sha1sum --text", msg))[2][1..LENGTH];
    end proc;




end module:
