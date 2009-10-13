mdb_prettyprint := proc(expr)
    if expr :: 'Or(set,list)' then
        expr[];
    elif expr :: 'Or(record,table)' then
        local fld;
        seq(fld = expr[fld], fld in [exports(expr)]);
    else
        expr
    end if;
end proc:

mla := "mdb_prettyprint.mla";
fremove(mla);
LibraryTools:-Save(mdb_prettyprint, mla);
