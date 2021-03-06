#LINK mdc.mpl
##INCLUDE ../include/mpldoc_macros.mpi
##TOPIC Tags
##DESCRIPTION
##- Assign macros that define the tags used to identify the messages
##  sent to the server.

$define TAG_CLEAR_MSG  "C" # clear a message displayed at bottom of screen
$define TAG_ERROR      "E" # text is an error message
$define TAG_EVAL       "O" # computed result
$define TAG_INFO       "I" # message is information, usually of a debugger query
$define TAG_MONITOR    "M" # output of a monitored expression
$define TAG_PRINTF     "P" #
$define TAG_PROMPT     ">" # display a prompt
$define TAG_RESULT     "R" # result of request
$define TAG_UNLIMITED  "U" # result of request, no length restriction
$define TAG_SAME       "%" # state is the same as the previous
$define TAG_SS_DEAD    "D" # showstat output for ss-dead buffer
$define TAG_SS_LIVE    "L" # showstat output for ss-live buffer
$define TAG_STACK      "K" # stack output
$define TAG_STATE      "S" # current state (including line-info)
$define TAG_WARN       "W" # warning
$define TAG_WATCHED    "w" #
