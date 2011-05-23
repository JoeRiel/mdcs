;;; mdb-server.el --- mdb-server-mode
;;; -*- mode: emacs-lisp -*-

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the Maple debugger server.

;;; Code:

;;{{{ License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc.,  51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;}}}
;;{{{ Commentary

;; This package provides a major mode, mdb, for debugging Maple code.
;; It uses the Maple debugger to step through interpreted Maple code,
;; which is displayed in a buffer. It is not a true source code
;; debugger, that is, one that allows stepping through the source
;; *file*; however, it is the next best thing.

;;}}}
;;{{{ TODO

;; A useful feature would be the ability to jump from the *showstat*
;; buffer to the corresponding location in the source file.  That
;; requires (1) a method to map a Maple procedure to its source; (2) a
;; means to locate the matching source line from the interpreted
;; value.  The latter is significantly more difficult than the former.
;; The source can look significantly different from the output.
;; Differences include:
;;
;; (*) indentation
;; (*) comments
;; (*) parenthese
;; (*) macros
;; (*) use-variables
;; (*) conversions ( _passed --> args, etc)
;; (*) ...
;;
;; A way to finesse the problem is to copy the source to a temporary
;; buffer, insert numbered dummy statements, interpret that procedure,
;; go to the line in this procedure, and then uses its adjacent dummy
;; statement to index into the original source.  Inserting dummy
;; statements in a robust way is not trivial, but doable.
;;
;; A simpler approach is pragmatic; generate a partial match and
;; search for it.  If the search fails, then move point to the top of
;; the procedure.  Or count lines and move the same percentage into
;; the procedure.  Or count, say, assignments.  A reasonable approach
;; might to determine the type of the statement (assignment, function,
;; control, ... ) then count forward from the start of the procedure.
;; That won't always work, but may be close enough most of the time.
;; For example, if the statement is the third assignment, then move to
;; the third assignment (search on ":=") in the source.  We would have
;; to skip commented-out code.  Handling stuff in $ifdef macros is a
;; problem.

;; Add stopwhenif to showstat actions (done).  There is a bug in the
;; debugger (SCR64943) that currently prevents this from working.  It
;; isn't all that useful, because it only applies to global variables.
;; Suggest that that be improved.

;; Add function to intelligently select variable/expression at point.
;; For example, if `mdb-stopwhen' is activated at a for-loop, the
;; default should not be the symbol 'for', but rather the
;; loop-variable.

;; Expand stopwhen to operate on local variables.  That has been done
;; however, it is less useful than it could be because debugopts
;; does not currently handly local variables of module exports.

;;}}}

;;{{{ Lisp Requirements

;;(require 'mdb-showstat)
;;(require 'maplev)
(eval-when-compile
  (require 'hl-line))

;;}}}

;;{{{ Constants

(defconst mdb-server-port 10000
  "Port used by mdb server")

(defvar mdb-server-proc nil "process for the server")

(defconst mdb-server-buffer "*mdb-server*"
  "Buffer associated with mdb server")

;;}}}
;;{{{ Variables

(defvar mdb-server-client-id 0)

(defvar mdb-server-clients '() 
  "Alist where KEY is a client/server process.")

;;}}}

;;{{{ Start and stop server

(defun mdb-server-start ()
  "Start an mdb server; return the process."
  (interactive)
  (get-buffer-create mdb-server-buffer)
  (unless (process-status "mdb-server")
    (setq mdb-server-clients '()
	  mdb-server-client-id 0
	  mdb-server-proc (make-network-process 
			   :name "mdb-server" 
			   :buffer mdb-server-buffer
			   :family 'ipv4 
			   :service mdb-server-port 
			   :sentinel 'mdb-server-sentinel 
			   :filter 'mdb-server-filter 
			   :server 't))))

(defun mdb-server-stop nil
  "Stop the Emacs mdb server."
  (interactive)
  ;; Delete each entry, killing process and buffers
  (while mdb-server-clients
    (let ((entry (car mdb-server-clients)))
      (delete-process (mdb-server--get-proc entry))
      (mdb-showstat-kill-buffer (mdb-server--get-showstat-buffer entry)))
    (setq mdb-server-clients (cdr mdb-server-clients)))
  ;; Kill the server process and buffer
  (if (process-status mdb-server-proc)
      (delete-process mdb-server-proc))
  (kill-buffer mdb-server-buffer))

;;}}}
;;{{{ Filter and sentinel

(defun mdb-server-filter (proc msg)
  "Filter to handle the Maple clients.
PROC identifies the client, MSG is the message."
  (mdb-server-log proc msg))

(defun mdb-server-sentinel (proc msg)
  "Handle change to client status.  PROC is client process,
MSG is the message indicating the changed status."
  ;; Consider adding a validation message.
  ;; Possibly other stuff.
  (cond
   ((eq 't (compare-strings msg 0 10 "open from " 0 10))
    ;; A Maple client has attached.
    ;; Create mdb and showstat buffers, and add to alist
    (mdb-server-add-client proc))
   ((string= msg "connection broken by remote peer\n")
    ;; A Maple client has closed.
    (mdb-server-delete-client proc))
   ((string= msg "deleted\n"))
   (t (error "unexpected sentinel message: %s" msg))))

;;}}}

;;{{{ Handle Clients

(defun mdb-server-add-client (proc)
  "Add a Maple client.  Buffers are created and added to the `mdb-servers-clients' alist."
  (if (assoc proc mdb-server-clients)
      (error "client already exists in list")
    ;; Create and add mdb buffers, add to 
    (setq mdb-server-client-id (1+ mdb-server-client-id)
	  mdb-server-clients (cons (cons proc 
					 (mdb-server-alist-entry proc mdb-server-client-id))
				   mdb-server-clients))
    (mdb-server-log proc "added client")))

(defun mdb-server-delete-client (proc)
  (let ((entry (assoc proc mdb-server-clients)))
    (if (null entry)
	(error "client does not exist")
      (mdb-server-log proc "removing client")
      ;; kill the showstat 
      (mdb-showstat-kill-buffer (mdb-server--get-showstat-buffer entry))
      (setq mdb-server-clients (delq entry mdb-server-clients)))))

;;}}}

;;{{{ Access fields in client entry of alist

(defun mdb-server-alist-entry (proc id)
  "Create an entry for the `mdb-servers-clients' alist.
Generate new buffers for the showstat and Maple output."
  (cons (mdb-showstat-generate-buffer proc) id))

;; Access elements of a client (entry in `mdb-server-clients')
;; Don't forget the entry, which comes from (assoc ...),
;; is a cons cell of the key and 
(defsubst mdb-server--get-proc            (entry) (car entry))
(defsubst mdb-server--get-showstat-buffer (entry) (cadr entry))
(defsubst mdb-server--get-id              (entry) (cddr entry))

;;}}}

;;{{{ talk to client

(defun mdb-server-send-client (proc msg)
  "Send MSG to Maple client with process PROC."
  (process-send-string proc msg))

(defun mdb-server-send-client-id (id msg)
  "Send MSG to the Maple client with ID."
  (interactive)
  (let ((clients mdb-server-clients)
	client proc)
    (while (and clients (null proc))
      (setq client (car clients))
      (if (eq id (mdb-server--get-id client))
    	  (setq proc (car client))
    	(setq clients (cdr clients))))
    (if proc
    	(mdb-server-send-client proc msg)
      (error "no client with id: %d" id))))

;;}}}

;;{{{ log stuff

(defun mdb-server-log (proc msg)
  (with-current-buffer mdb-server-buffer
    (goto-char (point-max))
    (let* ((client (assoc proc mdb-server-clients))
	   (id (mdb-server--get-id client)))
      (insert (format "client %d: %s\n" id msg)))
    (set-window-point (get-buffer-window) (point))))

;;}}}


(provide 'mdb-server)

;;{{{ Manual Tests
;;
;; (load "/home/joe/emacs/mdb/lisp/mdb-showstat.el")
;; (load "/home/joe/emacs/mdb/lisp/mdb-server.el")
;; (describe-variable 'mdb-server-clients)
;; (mdb-server-start)
;; (mdb-server-stop)
;;
;; (mdb-server-send-client-id 2 "showstat")
;; (mdb-server-send-client-id 2 "cont")
;; (mdb-server-send-client-id 2 "showstack")
;; (mdb-server-send-client-id 2 "where")
;; (mdb-server-send-client-id 2 "stopat 10")
;; (mdb-server-send-client-id 2 "cont")
;; (mdb-server-send-client-id 2 "step")
;; (mdb-server-send-client-id 2 "showstat")
;; (mdb-server-send-client-id 2 "cont")

;;}}}

;;; mdb-showstat.el ends here

