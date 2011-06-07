Browser := module()

export ModuleApply, Browser;

ModuleApply := proc(thing :: {string,name,`module`,procedure})

local nm, exps, modules;

    # Parse strings and recurse
    if thing :: string then
        nm := parse(thing);
        try
            bind(nm);
        catch:
        end try;
        return procname(nm);
    elif thing :: `module` then
        exps := sort([exports(thing)]);
        map2(printf,"%a\n", exps);
        modules := select(type, exps, `module`);
        map( exp -> ModuleApply(nm[exp]),modules );
        return NULL;
    end if;

end proc;


end module:
