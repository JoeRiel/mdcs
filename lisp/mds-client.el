;;; mds-client.el --- Assign the structure used for each mds client

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the Emacs Maple debugger server.
;; It is a part of the Maple Debugger Client-Server package.

;; Purpose:
;;
;; This file defines the data structure associated with each Maple
;; client and the methods used to access that structure.  The
;; basic structure consists of a list of seven elements.
;;
;; The first element is the process used to communicate with the client.
;; The process, which is an an argument to the sentinel and filter functions
;; of the TCP server,  is used as a key in the assoc-list `mds-clients'.
;; Each entry in the assoc-list is client structure defined here.

;; Client Data Structure
;;
;; The client structure is a vector.
;;
;; 0 - process
;; 1 - status
;; 2 - queue
;; 3 - id

;; 4 - live-buffer
;; 5 - dead-buffer
;; 6 - out-buffer
;; 7 - li-buffer
;; 8 - code-window

;; 9 - address of Maple procedure
;; 10 - procname
;; 11 - state (NEW)
;; 12 - statement
;; 13 - last debug command
;; 14 - result

;; 15 - allow-input-flag
;; 16 - has-source-flag
;; 17 - use-lineinfo-flag
;; 18 - quiet-flag
;; 19 - trace-flag

;;; Code:

(require 'mds-custom)

(declare-function mds-kill-buffer "mds")
(declare-function mds-li-create-buffer "mds-li")
(declare-function mds-out-create-buffer "mds-out")
(declare-function mds-queue-buffer "mds-queue")
(declare-function mds-queue-create "mds-queue")
(declare-function mds-ss-create-buffer "mds-ss")
(declare-function mds-writeto-log-proc "mds")

(defvar mds-client nil "Buffer-local client structure.")

(defvar mds-clients '()
  "Assoc-list containing info of accepted clients.
The list is indexed by the associated process.
See `mds-client-create' for the form of each entry.")

(defvar mds-clients-number 0
  "Current number of clients.
Maximum is given by `mds-max-number-clients'.")

;;{{{ basic

(defsubst mds-client-proc     (client) "Return CLIENT's process."          (aref client 0))
(defsubst mds-client-status   (client) "Return CLIENT's status."           (aref client 1))
(defsubst mds-client-queue    (client) "Return CLIENT's queue."            (aref client 2))
(defsubst mds-client-id       (client) "Return CLIENT's id."               (aref client 3))

;;}}}
;;{{{ buffers and window

(defsubst mds-client-live-buf (client) "Return CLIENT's live buffer."      (aref client 4))
(defsubst mds-client-dead-buf (client) "Return CLIENT's dead buffer."      (aref client 5))
(defsubst mds-client-out-buf  (client) "Return CLIENT's output buffer."    (aref client 6))
(defsubst mds-client-li-buf   (client) "Return CLIENT's line-info buffer." (aref client 7))

(defsubst mds-client-get-code-window (client)
  "Get CLIENT's code window.
This is the window used to display the input code; it may contain
either the ss-live or line-info (li) buffer."
  (aref client 8))
(defsubst mds-client-set-code-window (client window)
  "Set CLIENT's code window to WINDOW.
This is the window used to display the input code; it may contain
either the ss-live or line-info (li) buffer."
  (aset client 8 window))


;;}}}
;;{{{ maple

(defsubst mds-client-get-addr (client)
  "Get CLIENT's current procedure address."
  (aref client 9))
(defsubst mds-client-set-addr (client addr)
  "Set CLIENT's current procedure address to ADDR."
  (aset client 9 addr))

(defsubst mds-client-get-procname (client)
  "Get CLIENT's current procedure name."
  (aref client 10))
(defsubst mds-client-set-procname (client procname)
  "Set CLIENT's current procedure name to PROCNAME."
  (aset client 10 procname))

(defsubst mds-client-get-state (client)
  "Get CLIENT's current state."
  (aref client 11))
(defsubst mds-client-set-state (client state)
  "Set CLIENT's current state to STATE."
  (aset client 11 state))

(defsubst mds-client-get-statement (client)
  "Get CLIENT's statement.
The statement is the current line of Maple code."
  (aref client 12))
(defsubst mds-client-set-statement (client statement)
  "Set CLIENT's statement to STATEMENT.
The statement is the current line of Maple code."
  (aset client 12 statement))

(defsubst mds-client-get-last-cmd (client)
  "Get CLIENT's last command."
  (aref client 13))
(defsubst mds-client-set-last-cmd (client cmd)
  "Set CLIENT's last CMD."
  (aset client 13 cmd))

(defsubst mds-client-get-result (client)
  "Return the result of a request to a Maple CLIENT."
  (aref client 14))
(defsubst mds-client-set-result (client result)
  "Set CLIENT's RESULT."
  (aset client 14 result))

;;}}}
;;{{{ flags

(defsubst mds-client-get-allow-input (client)
  "Get CLIENT's allow-input flag."
  (aref client 15))
(defsubst mds-client-set-allow-input (client flag)
  "Set CLIENT's allow-input FLAG."
  (aset client 15 flag))

(defsubst mds-client-has-source-p (client)
  "Get CLIENT's has-source flag.
True means line-info source is available for the code."
  (aref client 16))
(defsubst mds-client-set-has-source (client flag)
  "Set CLIENT's has-source flag to FLAG.
True means line-info source is available for the code."
  (aset client 16 flag))

(defsubst mds-client-use-lineinfo-p (client)
  "Get CLIENT's use-lineinfo flag.
True means to display line-info source, when available."
  (aref client 17))
(defsubst mds-client-set-use-lineinfo (client flag)
  "Set CLIENT's use-lineinfo flag to FLAG.
True means to display line-info source, when available."
  (aset client 17 flag))

(defsubst mds-client-quiet-p (client)
  "Get CLIENT's quiet flag.
True means to hide the echoing of user commands and statements."
  (aref client 18))
(defsubst mds-client-set-quiet (client flag)
  "Set CLIENT's quiet flag to FLAG.
True means to hide the echoing of user commands and statements."
  (aset client 18 flag))

(defsubst mds-client-get-trace (client)
  "Get CLIENT's trace state."
  (aref client 19))
(defsubst mds-client-set-trace (client state)
  "Set CLIENT's trace STATE."
  (aset client 19 state))

;;}}}

(defun mds-client-create (proc id)
  "Create a client that is associated with process PROC and has identity ID.
The returned client structure is a vector [PROC status queue ID
live-buf dead-buf out-buf addr], where status is initialized to
'new'."
  (set-process-query-on-exit-flag proc mds-query-on-exit-flag)
  (let ((client (make-vector 20 nil)))
    (aset client 0 proc)
    (aset client 1 'login)
    (aset client 2 (mds-queue-create proc))
    (aset client 3 id)
    (aset client 4 (mds-ss-create-buffer client 'live))
    (aset client 5 (mds-ss-create-buffer client))
    (aset client 6 (mds-out-create-buffer client))
    (aset client 7 (mds-li-create-buffer client))
    (aset client 15 t)   ; allow-input-flag
    (aset client 17 mds-use-lineinfo-flag)
    client))
  
(defun mds-client-destroy (client)
  "Destroy a CLIENT by deleting the associated process and buffers."
  (delete-process  (mds-client-proc client))
  (mds-kill-buffer (mds-queue-buffer (mds-client-queue client)))
  (mds-kill-buffer (mds-client-live-buf client))
  (mds-kill-buffer (mds-client-dead-buf client))
  (mds-kill-buffer (mds-client-out-buf client))
  (mds-kill-buffer (mds-client-li-buf client)))

(defun mds-client-set-status (client status)
  "Set the status of CLIENT to STATUS, which is a symbol."
  (aset client 1 status))

(defun mds-client-set-id (client id)
  "Set the identifier of CLIENT to ID.
Currently ID consists of a four element list of strings,
\(LABEL VERSION OS MAPLE-PID)."
  (aset client 3 id))

(defun mds-client-delete (client)
  "Delete CLIENT from `mds-clients'.
Stop the associated process, kill the buffers, and decrement
`mds-clients-number'."
  (if client
      (let* ((proc (mds-client-proc client))
	     (entry (assq proc mds-clients)))
	(if (null entry)
	    (error "Client is unknown")
	  (mds-writeto-log-proc proc "removing client")
	  (mds-client-destroy client)
	  ;; update `mds-clients' and `mds-clients-number'
	  (setq mds-clients (delq entry mds-clients)
		mds-clients-number (1- mds-clients-number))))))

(defun mds-client-add (client)
  "Add CLIENT to the front of the assoc list of clients, `mds-clients'."
  (setq mds-clients (cons (cons (mds-client-proc client) client) mds-clients)
	mds-clients-number (1+ mds-clients-number)))

(defun mds-client-send (client msg)
  "Send the Maple CLIENT a MSG."
  (process-send-string (mds-client-proc client) msg))

(defun mds-clients-kill ()
  "Kill all clients in `mds-clients'."
  (while mds-clients
      (mds-client-delete (cdar mds-clients))))

(defun mds-client-interrupt (client)
  "Interrupt the Maple kernel of CLIENT.
Alas, this currently is not implemented."
  ;; Interrupt the kernel.  Alas, I don't know a general way to do
  ;; this, one that works across OSes, Maple interfaces, etc.  More
  ;; exactly, I only know of one method that works for one case; when
  ;; the process is local (and permission is available), sending
  ;; SIGINT to the tty controlling the mserver works.
  ;;
  ;; We should be able to interrupt Maple running on a different
  ;; machine.  We can communicate with the process (maybe).  Is there
  ;; a way to force it to interrupt?  That currently is *not* handled
  ;; by the Debugger code.
  (ding)
  (message
   (concat "Interrupting is currently not implemented.  "
	   "Use quit to close the debugger connection, "
	   "then manually interrupt Maple.")))

(defun mds-client-get-maple-version (client)
  "Return the version of the Maple kernel used by CLIENT.
The value is a string."
  (nth 1 (mds-client-id client)))

(defun mds-client-code-buffer (client)
  "Return the code-buffer of CLIENT."
  (if (and (mds-client-use-lineinfo-p client)
	   (mds-client-has-source-p client))
      (mds-client-li-buf client)
    (mds-client-live-buf client)))

(defun mds-client-toggle-line-info-p (client)
  "Toggle the line-info flag of CLIENT and return the new value."
  (mds-client-set-use-lineinfo client (not (mds-client-use-lineinfo-p client))))

(defun mds-client-toggle-quiet-p (client)
  "Toggle the quiet flag of CLIENT and return the new value."
  (mds-client-set-quiet client (not (mds-client-quiet-p client))))


(provide 'mds-client)

;;; mds-client.el ends here
