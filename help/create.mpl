proc()
local hdb;
    hdb := "mdc.help";
    if not FileTools:-Exists(hdb) then
        HelpTools:-Database:-Create(hdb);
    end if;
    HelpTools:-LoadWorksheets("maple/mhelp", hdb);
end proc():
