unprotect('SHA1');
module SHA1()

export ModuleApply ;

#{{{ locals

local h0,h1,h2,h3,h4
    , W
    , rot1
    , rot5
    , rot30
    , FillW
    , InsertLengthInW
    , PartialFillW
    , ProcessChunk
    , StringToInteger

    (* constants *)

    , `bytes/chunk` := 64
    , `bits/chunk`  := 8 * `bytes/chunk`
    , bits_in_length_code := 64

    , h00 := convert("67452301", 'decimal, hexadecimal')
    , h10 := convert("EFCDAB89", 'decimal, hexadecimal')
    , h20 := convert("98BADCFE", 'decimal, hexadecimal')
    , h30 := convert("10325476", 'decimal, hexadecimal')
    , h40 := convert("C3D2E1F0", 'decimal, hexadecimal')

    , `2^32` := 2^32
    , `2^31` := 2^31  # used in rot1
    , `2^30` := 2^30  # used in rot30
    , `2^27` := 2^27  # used in rot5
    , `2^5`  := 2^5   # used in rot5

    , `0x5A827999` := convert("5A827999", 'decimal, hexadecimal')
    , `0x6ED9EBA1` := convert("6ED9EBA1", 'decimal, hexadecimal')
    , `0x8F1BBCDC` := convert("8F1BBCDC", 'decimal, hexadecimal')
    , `0xCA62C1D6` := convert("CA62C1D6", 'decimal, hexadecimal')

    ;

#}}}

    #{{{ ModuleApply

    ModuleApply := proc(str :: string)
    uses Bits;
    local bits_in_str, bytes_in_str, Nchunks, chunk, i, rem_bits, s, hash;

        W := Array(0..79); # , 'datatype'=integer[4]);

        # Initialize hash values
        h0 := h00;
        h1 := h10;
        h2 := h20;
        h3 := h30;
        h4 := h40;

        # Preprocess
        bytes_in_str := length(str); # each character is one-byte
        bits_in_str := 8*bytes_in_str;
        Nchunks := iquo(bytes_in_str, `bytes/chunk`);

        # Process most chunks.
        # Extract the 32-byte substring, convert to an integer,
        # then pass to ProcessChunk.
        for i to Nchunks do
            s := str[(i-1)*`bytes/chunk`+1 .. i*`bytes/chunk`];
            FillW(s,W);
            ProcessChunk(W);
        end do;

        # Do last chunk(s)
        s := str[`bytes/chunk`*Nchunks+1..];
        rem_bits := 8*length(s);
        PartialFillW(s,W);

        # Determine whether we can fit the remainder in one chunk.
        if rem_bits + bits_in_length_code + 1 > `bits/chunk` then
            ProcessChunk( W );
            InsertLengthInW( bits_in_str, W, 'clear');
        else
            InsertLengthInW( bits_in_str, W);
        end if;

        ProcessChunk( W );

        # Convert to hex notation and combine, prepending
        # 0's to fill fields.
        hash := map2(sprintf, "%x", [h0,h1,h2,h3,h4]);
        return cat(seq('("0"$(8-length(s)),s)', s in hash));

    end proc;

    #}}}

    #{{{ ProcessChunk

    ProcessChunk := proc( W :: Array )
    local i,a,b,c,d,e,f,k;

    uses Bits;

        # move up when working

        # Extend the sixteen 32-bit words into eighty 32-bit words:
        for i from 16 to 79 do
#            W[i] := rot1(Xor(Xor(Xor(W[i-3],W[i-8]),W[i-14]),W[i-16]));
            W[i] := rot1(Xor(Xor(W[i-3],W[i-8]), Xor(W[i-14],W[i-16])));
        end do;

        # Initialize for this chunk
        (a,b,c,d,e) := (h0,h1,h2,h3,h4);

$define ABCDE (irem(rot5(a,5) + f + e + k + W[i],`2^32`), a, rot30(b), c, d)
#$define PRINT printf("%2d: %x  %x  %x  %x  %x  %x  %x\n", i, a, b, c, d, e, f, W[i])

        k := `0x5A827999`;
        for i from 0 to 19 do
            f := Xor(d, And(b, Xor(c,d)));
            (a,b,c,d,e) := ABCDE;
        end do;

        k := `0x6ED9EBA1`;
        for i from 20 to 39 do
            f := Xor(Xor(b,c),d);
            (a,b,c,d,e) := ABCDE;
        end do;

        k := `0x8F1BBCDC`;
        for i from 40 to 59 do
            f := Or(Or(And(b,c),And(b,d)),And(c,d));
            (a,b,c,d,e) := ABCDE;
        end do;

        k := `0xCA62C1D6`;
        for i from 60 to 79 do
            f := Xor(Xor(b,c),d);
            (a,b,c,d,e) := ABCDE;
        end do;

        h0 := irem(h0 + a, `2^32`);
        h1 := irem(h1 + b, `2^32`);
        h2 := irem(h2 + c, `2^32`);
        h3 := irem(h3 + d, `2^32`);
        h4 := irem(h4 + e, `2^32`);

        return NULL;

    end proc;

    #}}}

    #{{{ StringToInteger

    StringToInteger := proc(s)
        Bits:-Join(ListTools:-Reverse(convert(s,'bytes')), 8);
    end proc;

    #}}}
    #{{{ FillW

    FillW := proc(str :: string, W :: Array)
    local i;
        for i from 0 to 15 do
            W[i] := StringToInteger(str[i*4+1 .. (i+1)*4]);
        end do;
        return W;
    end proc;

    #}}}
    #{{{ PartialFillW

    PartialFillW := proc(str :: string, W :: Array)
    local i, num, fullrows, r, s;
        fullrows := iquo(length(str), 4, 'r');  # r is left over characters
        for i from 0 to fullrows-1 do
            W[i] := StringToInteger(str[i*4+1 .. (i+1)*4]);
        end do;
        # Convert remaining string in integer, then shift so
        # it is flush-left, which means shift it 4-r bytes.
        s := str[i*4+1..];
        num := StringToInteger(s);
        num := num * 2^(8*(4-r));
        # Now insert a 1 to the right of the character
        num := num + 2^(8*(4-r)-1);
        W[i] := num;
        for i from i+1 to 15 do
            W[i] := 0;
        end do;
        return W;
    end proc;
    #}}}
    #{{{ InsertLengthInW
    InsertLengthInW := proc( len :: nonnegint
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
        W[14] := iquo(len, `2^32`, 'r');
        W[15] := r;
        return W;
    end proc;
    #}}}
    #{{{ Rotations

##TEST
## $include <AssignFunc.mi>
## AssignLocal(rot1, SHA1:-rot1);
## AssignLocal(rot5, SHA1:-rot5);
## AssignLocal(rot30, SHA1:-rot30);
##
## Try("1.0", map(rot1, [0,1,2,4,2^31]), [0,2,4,8,1]);
## Try("1.1", rot1(23) + rot1(24), rot1(23+24));
## Try("5.0", map(rot5, [0,1,2,4,2^31]), [0,32,64,128,16]);
## Try("30.0", map(rot30, [0,1,2^31]), [0,2^30,2^29]);

    rot1 := proc(n)
    local q;
        2*irem(n,`2^31`,'q') + q;
    end proc;

    rot5 := proc(n)
    local q;
        `2^5`*irem(n,`2^27`,'q') + q;
    end proc;

    rot30 := proc(n)
    local q;
        `2^30`*irem(n,4,'q') + q;
    end proc;

    #}}}

end module:

#LibraryTools:-Save('SHA1', sprintf("%s/maple/lib/sha1.mla", kernelopts('homedir')));

