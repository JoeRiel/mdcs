;;; mds-where.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger 
;;
;;; Commentary:


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

;; This package provides a major mode, mds-showstat, for debugging
;; Maple code.  It uses the Maple debugger to step through interpreted
;; Maple code, which is displayed in a buffer. It is not a true source
;; code debugger, that is, one that allows stepping through the source
;; *file*; however, it is the next best thing.

;;}}}

;;{{{ Lisp Requirements

(require 'maplev)

;;}}}
;;{{{ Customizations

(defcustom mds-where-p4 "p4" "Perforce command-line executable")
(defcustom mds-where-swhere "swhere" "Executable for running swhere")

;;}}
;;{{{ Constants

(defconst mds-where-eom "^Process swhere finished")
(defconst mds-where-source-file-re "^ +source in: \\(.*\\)$")

;;}}}
;;{{{ Variables

(defvar mds-where-swhere-process nil)
(defvar mds-where-swhere-buffer nil)

;; Need method to change this

(defvar mds-where-p4-repo "//wmi/projects/mapleV/main")
(defvar mds-where-p4-process nil)
(defvar mds-where-display-buffer nil)

;;}}}

;;{{{ swhere stuff

(defun mds-where (name)
  "Call 'swhere' to find the source file for NAME.
If successful, a buffer named *swhere* opens.  Each source
filename is hyperlinked.  Clicking the hyperlink downloads
the source and displays it in another buffer."
  (interactive)
  (setq mds-where-swhere-buffer (mds-where-start-swhere-process name)
	mds-where-swhere-process (get-buffer-process mds-where-swhere-buffer)))

(defun mds-where-start-swhere-process (name)
  "Start an asynchronous process that calls swhere. 
The name of the associated buffer is *swhere*; the buffer is returned.
The process calls the executable given by `mds-where-swhere'."
  (let* ((buffer (get-buffer-create "*swhere*"))
	 (proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      (if proc (delete-process proc))
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (insert (format "%s:\n" name)))
    (set-process-sentinel
     (setq proc (start-process "swhere"
			       buffer
			       mds-where-swhere
			       name))
     'mds-where-swhere-sentinel)
    buffer))


(defun mds-where-swhere-sentinel (proc event)
  "When the swhere process finishes, count the number of matching source files.
If 0, return a message; if 1, download and display it; otherwise hyperlink
each match so that clicking on the link downloads and displays it."
  (if (not (string= event "finished\n"))
      (error "Process: %s had the event `%s'" proc event)
    (with-current-buffer mds-where-swhere-buffer
      (goto-char (point-min))
      (forward-line) ;; this can fail
      (let ((cnt 0) beg end)
	(while (re-search-forward mds-where-source-file-re nil t)
	  (setq cnt (1+ cnt))
	  (setq beg (match-beginning 1)
		end (match-end 1))
	  (make-text-button beg end :type 'mds-where-hyperlink-source))
	(setq buffer-read-only t)
	(cond
	 ((= cnt 0)
	  (beep)
	  (message "No match found"))
	 ((= cnt 1)
	  (mds-where-view-file (buffer-substring-no-properties beg end)))
	 (t (pop-to-buffer mds-where-swhere-buffer)))))))

(define-button-type 'mds-where-hyperlink-source
  'help-echo "View file"
  'action 'mds-where-view-file-button
  'follow-link t
  'face 'link)

;;}}}
;;{{{ perforce stuff

;; Functions for printing a function to a buffer
;; from the perforce repository.

(defun mds-where-start-p4-process (file)
  "Launch asynchronous process to download FILE from
perforce repository.  FILE is a bit of a misnomer,
it is the complete perforce path specification."
  (let* ((buffer (get-buffer-create "*p4*"))
	 (proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      (if proc (delete-process proc))
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max)))
    (set-process-sentinel
     (setq proc (start-process "p4"
			       buffer
			       mds-where-p4
			       "print" "-q"
			       file))
     'mds-where-p4-sentinel)
    buffer))


(defun mds-where-p4-sentinel (proc event)
  "When the p4 process has terminated successfully,
set the buffer to `mpldoc-mode', then display it."
  (if (not (string= event "finished\n"))
      (error "Process: %s had the event `%s'" proc event)
    (with-current-buffer mds-where-display-buffer
       (mpldoc-mode)
       (setq buffer-read-only t))
    (pop-to-buffer mds-where-display-buffer)))

(defun mds-where-view-file-button (button)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward mds-where-source-file-re (line-end-position))
	(mds-where-view-file
	 (format "%s/%s" mds-where-p4-repo (match-string-no-properties 1))))))

(defun mds-where-view-file (file)
  "Download and display FILE from the perforce repository.
Assign the global variables `mds-where-display-buffer' and
`mds-where-p4-process'.  The reason this seems strange is that an
asynchronous process is used to prevent tying up Emacs while
waiting for the file to display (can be slow on this end).  When
the file is ready for viewing, the buffer is displayed."
  (setq mds-where-display-buffer (mds-where-start-p4-process file)
	mds-where-p4-process (get-buffer-process mds-where-display-buffer)))


;;}}}

(provide 'mds-where)
