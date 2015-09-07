<--*- markdown -*-->
MDCS: Maple Debugger Client/Server
==================================

**Version 2.5.0**

Table of Contents
-----------------

* [Description](#description)
* [Screenshot](#screenshot)
* [Requirements](#requirements)
* [Installation](#installation)
* [Configuration](#configuration)
* [Usage](#usage)

Description
-----------

This package provides the client and server for a Maple Debugger
Client/Server architecture, an enhancement to the existing Maple
debugger.  Maple is a computer algebra system sold by
[Maplesoft](http://www.maplesoft.com/).

The server side is an Emacs package with several major modes
for displaying and controlling the code that is being debugged.
The client side is a Maple package that, when invoked,
replaces the Maple library debugger.

The client/server architecture provides several benefits:

* A common, full-featured, debugger interface that can be used
  whether running Maple from the GUI, the command-line, or from a
  script.

* Remote debugging--the client (Maple) can be run on one machine, the
  server on another. Communication is via standard TCP.

* Concurrent debugging--multiple Maple processes can be debugged
  simultaneously. This permits interactively comparing the actions of
  different versions of code, or comparing code run on machines with
  different operating systems. It also permits independently or
  synchronously stepping through separate processes in a Grid
  application.

The user interface to the debugger, which is provided by MDS, has
the following features:

* The procedure being debugged is displayed in a buffer with
  font-locking to highlight syntactic elements.  If lineinfo
  data is available for the procedure, the source file itself
  is used, otherwise an interpreted version of the procedure
  is displayed.

* Executing the code advances an _overlay-arrow_ that points to the
  next statement to be executed.

* The output of executed statements and evaluated expressions are
  displayed in a separate window.  These are printed in 1-D format
  so that they can be saved and reused as Maple input.

* As lines of code are executed, they are echoed in the output
  window.  The result provides a visual trace of all the code that
  has been executed.  Clicking in the output window opens another
  window that displays the procedure and statement that
  generated the selected output.

* Expressions can be directly evaluated and displayed in a nice
  format.  For example, a record can be displayed as a vertical list
  of equations with field name on the left and the entry on the
  right.

* The common debugging commands, *next*, *into*, *step*, etc., are
  bound to keystrokes (n, i, s, etc.).

* Breakpoints can be set or cleared directly in the buffer.

* A trace mode permits rapidly tracing through instrumented
  procedures.  That, coupled with the ability to jump from the
  output to the corresponding statement in the code, facilitates
  debugging.

* Debugged procedures can be _live-patched_, that is, modified
  in-place.  This permits testing changes without access to the full
  source code.

Screenshot
----------
Following is a screen shot of the debugger interface.

* The left pane displays the source file.
  The marker in the left margin indicates the current statement.
* The top right pane contains output from previously executed statements
  and a query of the current stack.
* The bottom right pane shows interpreted code for one of the calls on the stack.

![screen shot](images/mds-example.png?raw=true "Screen Shot")

Requirements
------------
* [GNU Emacs](https://www.gnu.org/software/emacs/) 23.1+ (earlier versions may work)
* [maplev-mode](https://github.com/JoeRiel/maplev) 2.33+ (Emacs mode for developing Maple code)
* [Maple](https://www.maplesoft.com) 14+ (earlier versions should work)

Installation
------------
Copy the files to locations appropriate on your system.
Here is where I put the various files:

    $HOME/maple/toolbox/mdc/lib    :  mdc.mla mdc.hdb mdc.help
    $HOME/.emacs.d/maple           :  lisp/mds*.el
    $HOME/share/info               :  doc/mds.info (do not include the "doc/")

Under the doc subdirectory is an html format of the mds manual.  It
may be useful if you do not have Emacs or info installed and so cannot
read the info file (mds).

On a Linux system you might be able to install everything with

    make install

Configuration
-------------
Modify your .emacs file.  I have the following

	(add-to-list 'load-path (concat user-emacs-directory "maple"))
    (autoload 'mds "mds" "Restart the Maple Debugger Server" t)

Usage
-----
Fire up Emacs and launch the server:

    M-x mds

Now launch Maple, either tty or the GUI.  If libname
is set properly, you should be able to access help for
the Maple Debugger Client (mdc), by typing ?mdc.

For those in hurry, to debug the **int** command, do

    > mdc(int):
    > int(x,x);

If you then go to Emacs, you should see an `mds-showstat`
and `mds-output` buffer displayed.

In the `mds-showstat` buffer, type C-h m to get a summary of
the available keystrokes.

