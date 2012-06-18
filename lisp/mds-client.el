;;; mds-client.el

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
;; Each client consists of a list of seven elements.

(eval-when-compile
  (require 'mds-custom)
  (defvar mds-ss-result nil))

(declare-function mds-ss-create-buffer "mds-ss")
(declare-function mds-li-create-buffer "mds-li")
(declare-function mds-out-create-buffer "mds-out")
(declare-function mds-queue-create "mds")
(declare-function mds-writeto-log "mds")
(declare-function mds-kill-buffer "mds")

(defvar mds-client nil "Buffer-local client structure.")

(defvar mds-clients '()
  "Assoc-list containing info of accepted clients, indexed by the associated process.
See `mds-client-create' for the form of each entry.")

(defvar mds-clients-number 0
  "Current number of clients.
Maximum is given by `mds-max-number-clients'.")

(defsubst mds-client-proc     (client) "Return CLIENT's process."          (aref client 0))
(defsubst mds-client-status   (client) "Return CLIENT's status."           (aref client 1))
(defsubst mds-client-queue    (client) "Return CLIENT's queue."            (aref client 2))
(defsubst mds-client-id       (client) "Return CLIENT's id."               (aref client 3))
(defsubst mds-client-live-buf (client) "Return CLIENT's live buffer."      (aref client 4))
(defsubst mds-client-dead-buf (client) "Return CLIENT's dead buffer."      (aref client 5))
(defsubst mds-client-out-buf  (client) "Return CLIENT's output buffer."    (aref client 6))
(defsubst mds-client-li-buf   (client) "Return CLIENT's line-info buffer." (aref client 7))

(defsubst mds-client-get-addr (client)      "Get CLIENT's procedure address."       (aref client 8))
(defsubst mds-client-set-addr (client addr) "Set CLIENT's procedure address, ADDR." (aset client 8 addr))

(defsubst mds-client-get-allow-input (client)      "Get CLIENT's allow-input flag." (aref client 9))
(defsubst mds-client-set-allow-input (client flag) "Set CLIENT's allow-input FLAG." (aset client 9 flag))

(defsubst mds-client-get-last-cmd (client)     "Get CLIENT's last command." (aref client 10))
(defsubst mds-client-set-last-cmd (client cmd) "Set CLIENT's last CMD."     (aset client 10 cmd))

(defsubst mds-client-get-trace (client)       "Get CLIENT's trace state." (aref client 11))
(defsubst mds-client-set-trace (client state) "Set CLIENT's trace STATE." (aset client 11 state))

(defsubst mds-client-has-source-p (client)        "Get CLIENT's has-source flag" (aref client 12))
(defsubst mds-client-set-has-source (client flag) "Set CLIENT's has-source FLAG" (aset client 12 flag))

(defsubst mds-client-use-lineinfo-p   (client)      "Get CLIENT's use-lineinfo flag" (aref client 13))
(defsubst mds-client-set-use-lineinfo (client flag) "Set CLIENT's use-lineinfo FLAG" (aset client 13 flag))

(defsubst mds-client-get-code-window (client)        "Get CLIENT's code window" (aref client 14))
(defsubst mds-client-set-code-window (client window) "Set CLIENT's code WINDOW" (aset client 14 window))

(defun mds-client-create (proc id)
  "Create a client that is associated with process PROC and has identity ID.
The returned client structure is a vector [PROC status queue ID
live-buf dead-buf out-buf addr], where status is initialized to
'new'."
  (let ((client (make-vector 15 nil)))
    (aset client 0 proc)
    (aset client 1 'login)
    (aset client 2 (mds-queue-create proc))
    (aset client 3 id)
    (aset client 4 (mds-ss-create-buffer client 'live))
    (aset client 5 (mds-ss-create-buffer client))
    (aset client 6 (mds-out-create-buffer client))
    (aset client 7 (mds-li-create-buffer client))
    (aset client 8 "")  ; addr
    (aset client 9 t)   ; allow-input
    (aset client 13 mds-use-lineinfo-flag)
    client))
  
(defun mds-client-destroy (client)
  "Destroy a CLIENT by deleting the associated process and buffers."
  (delete-process  (mds-client-proc client))
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
  "Delete CLIENT from `mds-clients'.  Stop the associated process,
kill the buffers, and decrement `mds-clients-number'."
  (if client
      (let* ((proc (mds-client-proc client))
	     (entry (assq proc mds-clients)))
	(if (null entry)
	    (error "Client is unknown.")
	  (mds-writeto-log proc "removing client")
	  (mds-client-destroy client)
	  ;; update `mds-clients' and `mds-clients-number'
	  (setq mds-clients (delq entry mds-clients)
		mds-clients-number (1- mds-clients-number))))))

(defun mds-client-add (client)
  "Add CLIENT to the front of the assoc list of clients, `mds-clients'."
  (setq mds-clients (cons (cons (mds-client-proc client) client) mds-clients)
	mds-clients-number (1+ mds-clients-number)))

(defun mds-client-send (client msg)
  "Send MSG to CLIENT."
  (let ((proc (mds-client-proc client)))
    (process-send-string proc msg)))

(defun mds-clients-kill ()
  "Kill all clients in `mds-clients'."
  (while mds-clients
      (mds-client-delete (cdar mds-clients))))

(defun mds-client-interrupt (client)
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
  (message "Interrupting is currently not implemented.  Use quit to close the debugger connection, then manually interrupt Maple."))

(defun mds-client-get-maple-release (client)
  "Return the major release of the Maple kernel used by CLIENT.
The value is an integer."
  (string-to-number (nth 1 (mds-client-id client))))

(defun mds-client-set-result (client result)
  "Assign `mds-ss-result' in CLIENT the value RESULT."
  (with-current-buffer (mds-client-live-buf client)
    (setq mds-ss-result result)))

(defun mds-client-get-result (client)
  "Return the value of `mds-ss-result in CLIENT."
  (buffer-local-value 'mds-ss-result (mds-client-live-buf client)))

(defun mds-client-code-buffer (client)
  "Return the code-buffer of CLIENT."
  (if (and (mds-client-use-lineinfo-p client)
	   (mds-client-has-source-p client))
      (mds-client-li-buf client)
    (mds-client-live-buf client)))

(defun mds-client-toggle-line-info-p (client)
  "Toggle the line-info flag of CLIENT and return the new value."
  (mds-client-set-use-lineinfo client (not (mds-client-use-lineinfo-p client))))


(provide 'mds-client)
