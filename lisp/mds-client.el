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

(declare-function mds-ss-create-buffer "mds-ss")
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


(defsubst mds-client-proc     (client) (car client))
(defsubst mds-client-status   (client) (nth 1 client))
(defsubst mds-client-queue    (client) (nth 2 client))
(defsubst mds-client-id       (client) (nth 3 client))
(defsubst mds-client-live-buf (client) (nth 4 client))
(defsubst mds-client-dead-buf (client) (nth 5 client))
(defsubst mds-client-out-buf  (client) (nth 6 client))

(defun mds-client-create (proc id)
  "Create a client that is associated with process PROC and has identity ID.
The returned client structure is a list (PROC status queue ID
live-buf dead-buf out-buf), where status is initialized to 'new'."
  (let ((client (list proc)))
    (setcdr client (list
		    'login
		    (mds-queue-create proc)
		    id
		    (mds-ss-create-buffer client 'live)
		    (mds-ss-create-buffer client)
		    (mds-out-create-buffer client)))
    client))
  
(defun mds-client-destroy (client)
  "Destroy a CLIENT by deleting the associated process and buffers."
  (delete-process  (mds-client-proc client))
  (mds-kill-buffer (mds-client-live-buf client))
  (mds-kill-buffer (mds-client-dead-buf client))
  (mds-kill-buffer (mds-client-out-buf client)))

(defun mds-client-set-status (client status)
  "Set the status of CLIENT to STATUS, which is a symbol."
  (setcar (cdr client) status))

(defun mds-client-set-id (client id)
  "Set the identifier of CLIENT to ID.
Currently ID consists of a three element list of strings,
\(USER-ID OS MAPLE-PID)."
  (setcar (nthcdr 3 client) id))


(defun mds-client-delete (client)
  "Delete CLIENT from `mds-clients'.  Stop the associated process,
kill the buffers, and decrement `mds-clients-number'."
  (if client
      (let ((proc (mds-client-proc client)))
	(mds-writeto-log proc "removing client")
	(mds-client-destroy client)
	;; update `mds-clients' and `mds-clients-number'
	(setq mds-clients (delq client mds-clients)
	      mds-clients-number (1- mds-clients-number)))))

(defun mds-client-add (client)
  "Add CLIENT to the front of the assoc list of clients, `mds-clients'."
  (setq mds-clients (cons client mds-clients)
	mds-clients-number (1+ mds-clients-number)))

(defun mds-client-send (client msg)
  "Send MSG to CLIENT."
  (let ((proc (mds-client-proc client)))
    (process-send-string proc msg)))



(defun mds-clients-kill ()
  "Kill all clients in `mds-clients'."
  (while mds-clients
      (mds-client-delete (car mds-clients))))

(provide 'mds-client)
