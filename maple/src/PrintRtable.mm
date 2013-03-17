##MODULE mdc[PrintRtable]
##HALFLINE fix problem with ~`print/rtable`~
##AUTHOR   Joe Riel
##DATE     Mar 2013
##DESCRIPTION
##- Provide commands to replace and restore ~`print/rtable`~.
##  When a "DataTable" component is embedded in a Standard worksheet,
##  ~`print/rtable`~ is frequently called.  When the mdc debugger
##  is active, and trapping all errors, that causes errors because,
##  for reasons unknown, the call to ~streamcall(INTERFACE_GET(.))~
##  returns NULL.  
##  
##- The `Replace` export modifies `print/rtable`, replacing the
##  calls to "IsWorksheetInterface" and "interface" with
##  their values. 
##  
##- The `Restore` export restores the original procedure.

PrintRtable := module()
    
export Replace
    ,  Restore
    ;
    
local orig;
    
    Replace := proc()
    global `print/rtable`;
        
        if not assigned(orig) then
            orig := op(`print/rtable`);
        end if;
        
        `print/rtable` := subs([NULL
                                , 'IsWorksheetInterface'() = IsWorksheetInterface()
                                , 'interface'(''rtablesize'') = interface('rtablesize')
                                , 'interface'(''prettyprint'') = interface('prettyprint')
                               ]
                               , op(orig)
                              );
        NULL;
    end proc;
    
    Restore := proc()
    global `print/rtable`;
        if assigned(orig) then
            `print/rtable` := op(orig);
        end if;
    end proc;
    
end module:
