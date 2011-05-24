# -*- mode: mpldoc -*-
#
##DEFINE PKG mdcs
##PACKAGE \PKG
##HALFLINE Maple Debugger Client/Server
##AUTHOR   Joe Riel
##DATE     May 2011
##CALLINGSEQUENCE
##- \PKG[command](args)
##DESCRIPTION
##- The `\PKG` is a ...

unprotect('mdcs');

# kernelopts(includepath);

module mdcs()

export Client
    ,  Format
#    ,  Server
    ;

$include <src/Client.mm>
$include <src/Format.mm>
#$include <Server.mm>

end module:

#savelib('mdcs'):
