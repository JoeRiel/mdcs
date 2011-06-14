unprotect('SHA');
module SHA()

export ModuleApply
    ,  SHA1
    ,  SHA2
    ;

#{{{ locals

local RotateByScaling
    , FillArray
    , InsertLengthInArray
    , IntegerToString
    , OuterLoop
    , PartialFillArray
    , ProcessChunk
    , StringToInteger

    (* constants *)

    , `bytes/chunk` := 64
    , `bits/chunk`  := 8 * `bytes/chunk`
    , bits_in_length_code := 64

    ;

#}}}
    #{{{ ModuleApply

    ModuleApply := proc( str :: string
                         , { sha :: identical(sha1,sha2) := 'sha1' }
                         , $
                       )

        if sha = sha1 then
            return SHA1(str);
        else
            return SHA2(str);
        end if;
    end proc;

    #}}}

# Define a few macros to accelerate conversions
$define rotr(n,m) RotateByScaling(n, 2^(32-m), 2^m)
$define rotl(n,m) RotateByScaling(n, 2^m, 2^(32-m))
$define toword(n) irem(n,2^32)

    #{{{ SHA1

    SHA1 := module()
    export CombineHash
        ,  Initialize
        ,  ModuleApply
        ,  ProcessChunk
        ;

    local K := Array(0..3, map(convert
                               , ["5a827999","6ed9eba1","8f1bbcdc","ca62c1d6"]
                               , 'decimal, hexadecimal'
                              ))

        , H0 := Array(0..4, map(convert
                                , ["67452301", "efcdab89", "98badcfe", "10325476", "c3d2e1f0"]
                                , 'decimal, hexadecimal'
                               ))
        ;


        ModuleApply := proc( s :: string )
            OuterLoop( s, thismodule );
        end proc;

        Initialize := proc()
            ( Array(0..79), Array( H0 ))
        end proc;

        ProcessChunk := proc( W :: Array, H :: Array )
        local i,a,b,c,d,e,f,k;

        uses Bits;

            # Extend the sixteen 32-bit words into eighty 32-bit words:
            for i from 16 to 79 do
                W[i] := rotl(Xor(Xor(W[i-3],W[i-8]), Xor(W[i-14],W[i-16])), 1);
            end do;

            # Initialize for this chunk
            (a,b,c,d,e) := seq(H[k],k=0..4);

$define ABCDE (irem(rotl(a,5) + f + e + k + W[i],2^32), a, rotl(b,30), c, d)
#$define PRINT printf("%2d: %x  %x  %x  %x  %x  %x  %x\n", i, a, b, c, d, e, f, W[i])

            k := K[0];
            for i from 0 to 19 do
                f := Xor(d, And(b, Xor(c,d)));
                (a,b,c,d,e) := ABCDE;
            end do;

            k := K[1];
            for i from 20 to 39 do
                f := Xor(Xor(b,c),d);
                (a,b,c,d,e) := ABCDE;
            end do;

            k := K[2];
            for i from 40 to 59 do
                f := Or(Or(And(b,c),And(b,d)),And(c,d));
                (a,b,c,d,e) := ABCDE;
            end do;

            k := K[3];
            for i from 60 to 79 do
                f := Xor(Xor(b,c),d);
                (a,b,c,d,e) := ABCDE;
            end do;

            H[0] := toword(H[0] + a);
            H[1] := toword(H[1] + b);
            H[2] := toword(H[2] + c);
            H[3] := toword(H[3] + d);
            H[4] := toword(H[4] + e);

            return NULL;

        end proc;

        CombineHash := proc( H :: Array )
            cat(seq(IntegerToString(H[i]), i = 0..4));
        end proc;

    end module;


    IntegerToString := proc(i)
    local s;
        s := sprintf("%x",i);
        cat("0"$(8-length(s)), s);
    end proc;



    #}}}
    #{{{ SHA2

    SHA2 := module()
    export CombineHash
        ,  Initialize
        ,  ModuleApply
        ,  ProcessChunk
        ;

    local K := Array(0..64
                     , map(convert
                           , ["428a2f98", "71374491", "b5c0fbcf", "e9b5dba5", "3956c25b", "59f111f1", "923f82a4", "ab1c5ed5",
                              "d807aa98", "12835b01", "243185be", "550c7dc3", "72be5d74", "80deb1fe", "9bdc06a7", "c19bf174",
                              "e49b69c1", "efbe4786", "0fc19dc6", "240ca1cc", "2de92c6f", "4a7484aa", "5cb0a9dc", "76f988da",
                              "983e5152", "a831c66d", "b00327c8", "bf597fc7", "c6e00bf3", "d5a79147", "06ca6351", "14292967",
                              "27b70a85", "2e1b2138", "4d2c6dfc", "53380d13", "650a7354", "766a0abb", "81c2c92e", "92722c85",
                              "a2bfe8a1", "a81a664b", "c24b8b70", "c76c51a3", "d192e819", "d6990624", "f40e3585", "106aa070",
                              "19a4c116", "1e376c08", "2748774c", "34b0bcb5", "391c0cb3", "4ed8aa4a", "5b9cca4f", "682e6ff3",
                              "748f82ee", "78a5636f", "84c87814", "8cc70208", "90befffa", "a4506ceb", "bef9a3f7", "c67178f2"]
                           , 'decimal, hexadecimal'
                          ))

        , H0 := Array(0..7, map(convert
                                , ["6a09e667", "bb67ae85", "3c6ef372", "a54ff53a", "510e527f", "9b05688c", "1f83d9ab", "5be0cd19"]
                                , 'decimal, hexadecimal'
                             ))
        ;


        ModuleApply := proc( s :: string )
            OuterLoop( s, thismodule );
        end proc;

        Initialize := proc()
            ( Array(0..63), Array(H0) );
        end proc;

        ProcessChunk := proc( W :: Array, H :: Array )
        local i,j,a,b,c,d,e,f,g,h,s0,s1,t1,t2,ch,maj;

        uses Bits;

            # Extend the sixteen 32-bit words into sixty-four 32-bit words:
            for i from 16 to 63 do
                s0 := Xor(Xor(rotr(W[i-15],7), rotr18(W[i-15])), iquo(W[i-15],2^3));
                s1 := Xor(Xor(rotr(W[i-2],17), rotr19(W[i-2])), iquo(W[i-2],2^10));
                W[i] := toword(W[i-16] + s0 + W[i-7] + s1);
            end do;



            # Initialize for this chunk
            (a,b,c,d,e,f,g,h) := seq(H[j],j=0..7);

            for i from 0 to 63 do
                s0 := Xor(Xor(rotr(a,2), rotr(a,13)), rotr(a,22));
                maj := Xor(Xor(And(a,b), And(a,c)), And(b,c));
                t2 := toword(s0 + maj);
                s1 := Xor(Xor(rotr(e,6), rotr(e,11)), rotr(e,25));
                ch := Xor(And(e,f), And(Not(e,'bits'=32), g));
                t1 := toword(h + s1 + ch + K[i] + W[i]);

                h := g;
                g := f;
                f := e;
                e := d + t1;
                d := c;
                c := b;
                b := a;
                a := t1 + t2;
            end do;

            # Add this chunk's hash to result so far:
            H[0] := toword(H[0] + a);
            H[1] := toword(H[1] + b);
            H[2] := toword(H[2] + c);
            H[3] := toword(H[3] + d);
            H[4] := toword(H[4] + e);
            H[5] := toword(H[5] + f);
            H[6] := toword(H[6] + g);
            H[7] := toword(H[7] + h);

            return NULL;

        end proc;

        CombineHash := proc( H :: Array )
        local j;
            cat(seq(IntegerToString(H[j]), j = 0..7));
        end proc;

    end module;



    #}}}

    #{{{ OuterLoop

    OuterLoop := proc(str :: string, sha)
    uses Bits;
    local bits_in_str, bytes_in_str, Nchunks, i, rem_bits, s, W, H;

        (W,H) := sha:-Initialize();

        # Preprocess
        bytes_in_str := length(str); # each character is one-byte
        bits_in_str := 8*bytes_in_str;
        Nchunks := iquo(bytes_in_str, `bytes/chunk`);

        # Process most chunks.
        # Extract the 32-byte substring, convert to an integer,
        # then pass to ProcessChunk.
        for i to Nchunks do
            s := str[(i-1)*`bytes/chunk`+1 .. i*`bytes/chunk`];
            FillArray(s,W);
            sha:-ProcessChunk(W, H);
        end do;

        # Do last chunk(s)
        s := str[`bytes/chunk`*Nchunks+1..];
        rem_bits := 8*length(s);
        PartialFillArray(s,W);

        # Determine whether we can fit the remainder in one chunk.
        if rem_bits + bits_in_length_code + 1 > `bits/chunk` then
            ProcessChunk( W, H );
            InsertLengthInArray( bits_in_str, W, 'clear');
        else
            InsertLengthInArray( bits_in_str, W);
        end if;

        sha:-ProcessChunk( W, H );

        return sha:-CombineHash( H );

    end proc;

    #}}}

    #{{{ IntegerToString

    IntegerToString := proc(i)
    local s;
        s := sprintf("%x",i);
        cat("0"$(8-length(s)), s);
    end proc;

    #}}}
    #{{{ StringToInteger

    StringToInteger := proc(s)
        Bits:-Join(ListTools:-Reverse(convert(s,'bytes')), 8);
    end proc;

    #}}}
    #{{{ FillArray

    # Fill rows 0 to 15 of Array W with string str,
    # converted to integers.  Each row holds four characters.
    # Str is exactly 512 characters.

    FillArray := proc(str :: string, W :: Array)
    local i;
        for i from 0 to 15 do
            W[i] := StringToInteger(str[i*4+1 .. (i+1)*4]);
        end do;
        return W;
    end proc;

    #}}}
    #{{{ PartialFillArray

    # Fill some of the rows of Array with string str.
    # Each row holds four characters.  Str is less than 512
    # characters; it may be zero length.  Insert a one bit
    # (byte 2^7) after the last character.  Fill remaining
    # positions, through row 15, with zeros.

    PartialFillArray := proc(str :: string, W :: Array)
    local i, fullrows, r;
        fullrows := iquo(length(str), 4, 'r');  # r is left over characters
        for i from 0 to fullrows-1 do
            W[i] := StringToInteger(str[i*4+1 .. (i+1)*4]);
        end do;
        # Convert remaining string to integer, then shift so
        # it is flush-left, which means shift it 4-r bytes.
        # Finally, add the 1-bit just to the right of it.
        W[i] := StringToInteger(str[i*4+1..]) * 2^(8*(4-r)) + 2^(8*(4-r)-1);

        # Clear the remaining rows, through row 15
        for i from i+1 to 15 do
            W[i] := 0;
        end do;
        return W;
    end proc;
    #}}}
    #{{{ InsertLengthInArray

    # Insert the length of the original string, in bits, into
    # the Array.  It is a 64-bit field and goes in rows 14 and 15.
    # If 'clear' is true, clear the previous rows.
    InsertLengthInArray := proc( len :: nonnegint
                                 , W :: Array
                                 , { clear :: truefalse := false }
                                 , $
                               )
    local i,r;
        if clear then
            for i from 0 to 15-2 do
                W[i] := 0;
            end do;
        end if;
        # The length field is 64-bits.
        # It goes in slots 14 and 15.
        W[14] := iquo(len, 2^32, 'r');
        W[15] := r;
        return W;
    end proc;
    #}}}
    #{{{ RotateByScaling

    RotateByScaling := proc(n,a,b)
        a*irem(n,b,'q') + q;
    end proc;

    #}}}

end module:

LibraryTools:-Save('SHA', sprintf("%s/maple/lib/sha.mla", kernelopts('homedir')));

