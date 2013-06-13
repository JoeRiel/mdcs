;;; mds-swhere.el --- interface to swhere utility

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
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


;;}}}

;;; Code

;;{{{ Lisp Requirements

(eval-when-compile
  (require 'maplev))

;;}}}
;;{{{ Customizations

(defcustom mds-swhere-p4 "p4"
  "Perforce command-line executable."
  :type 'string
  :group 'mds)

(defcustom mds-swhere-executable "swhere"
  "Executable for running swhere."
  :type 'string
  :group  'ms)

;;}}}
;;{{{ Constants

(defconst mds-swhere-eom "^Process swhere finished")
(defconst mds-swhere-source-file-re "^ +source in: \\(.*\\)$")

;;}}}
;;{{{ Variables

(defvar mds-swhere-process nil)
(defvar mds-swhere-buffer nil)

;; Need method to change this

(defvar mds-swhere-p4-repo "//wmi/projects/mapleV/main")
(defvar mds-swhere-p4-process nil)
(defvar mds-swhere-display-buffer nil)

;;}}}

;;{{{ swhere stuff

(defun mds-swhere (name)
  "Call 'swhere' to find the source file for NAME.
If successful, a buffer named *swhere* opens.  Each source
filename is hyperlinked.  Clicking the hyperlink downloads
the source and displays it in another buffer."
  (interactive)
  (setq mds-swhere-buffer (mds-swhere-start-swhere-process name)
	mds-swhere-process (get-buffer-process mds-swhere-buffer)))

(defun mds-swhere-start-swhere-process (name)
  "Start an asynchronous process that calls swhere.
The name of the associated buffer is *swhere*; the buffer is returned.
The process calls the executable given by `mds-swhere'."
  (let* ((buffer (get-buffer-create "*swhere*"))
	 (proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      (if proc (delete-process proc))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "%s:\n" name)))
    (set-process-sentinel
     (setq proc (start-process "swhere"
			       buffer
			       mds-swhere-executable
			       name))
     'mds-swhere-sentinel)
    buffer))

(defun mds-swhere-sentinel (proc event)
  "When the swhere process finishes, count the number of matching source files.
If 0, return a message; if 1, download and display it; otherwise hyperlink
each match so that clicking on the link downloads and displays it."
  (if (not (string= event "finished\n"))
      (error "Process: %s had the event `%s'" proc event)
    (with-current-buffer mds-swhere-buffer
      (goto-char (point-min))
      (forward-line) ;; this can fail
      (let ((cnt 0) beg end)
	(while (re-search-forward mds-swhere-source-file-re nil t)
	  (setq cnt (1+ cnt))
	  (setq beg (match-beginning 1)
		end (match-end 1))
	  (make-text-button beg end :type 'mds-swhere-hyperlink-source))
	(setq buffer-read-only t)
	(cond
	 ((= cnt 0)
	  (ding)
	  (message "No match found"))
	 ((= cnt 1)
	  (mds-swhere-view-file (buffer-substring-no-properties beg end)))
	 (t (pop-to-buffer mds-swhere-buffer)))))))

(define-button-type 'mds-swhere-hyperlink-source
  'help-echo "View file"
  'action 'mds-swhere-view-file-button
  'follow-link t
  'face 'link)

;;}}}
;;{{{ perforce stuff

;; Functions for printing a function to a buffer
;; from the perforce repository.

(defun mds-swhere-start-p4-process (file)
  "Launch asynchronous process to download FILE from perforce repository.
FILE is a bit of a misnomer, it is the complete perforce path specification."
  (let* ((buffer (get-buffer-create "*p4*"))
	 (proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      (if proc (delete-process proc))
      (setq buffer-read-only nil)
      (erase-buffer))
    (set-process-sentinel
     (setq proc (start-process "p4"
			       buffer
			       mds-swhere-p4
			       "print" "-q"
			       file))
     'mds-swhere-p4-sentinel)
    buffer))


(defun mds-swhere-p4-sentinel (proc event)
  "When the p4 process has terminated successfully,
set the buffer to `mpldoc-mode', then display it."
  (if (not (string= event "finished\n"))
      (error "Process: %s had the event `%s'" proc event)
    (with-current-buffer mds-swhere-display-buffer
       (mpldoc-mode)
       (setq buffer-read-only t))
    (pop-to-buffer mds-swhere-display-buffer)))

(defun mds-swhere-view-file-button (button)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward mds-swhere-source-file-re (line-end-position))
	(mds-swhere-view-file
	 (format "%s/%s" mds-swhere-p4-repo (match-string-no-properties 1))))))

(defun mds-swhere-view-file (file)
  "Download and display FILE from the perforce repository.
Assign the global variables `mds-swhere-display-buffer' and
`mds-swhere-p4-process'.  The reason this seems strange is that an
asynchronous process is used to prevent tying up Emacs while
waiting for the file to display (can be slow on this end).  When
the file is ready for viewing, the buffer is displayed."
  (setq mds-swhere-display-buffer (mds-swhere-start-p4-process file)
	mds-swhere-p4-process (get-buffer-process mds-swhere-display-buffer)))


;;}}}

(provide 'mds-swhere)

(provide 'mds-swhere)

;;; mds-swhere.el ends here
