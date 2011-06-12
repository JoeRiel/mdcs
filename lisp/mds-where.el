

(defconst mds-where-eom "^Process swhere finished")
(defconst mds-where-source-file-re "^ +source in: \\(.*\\)$")


(defvar mds-where-swhere "swhere")
(defvar mds-where-swhere-process nil)
(defvar mds-where-swhere-buffer nil)

(defvar mds-where-p4 "p4")
(defvar mds-where-p4-repo "//wmi/projects/mapleV/main")
(defvar mds-where-p4-process nil)
(defvar mds-where-display-buffer nil)


(defun mds-where (name)
  (interactive)
  (setq mds-where-swhere-buffer (mds-where-start-swhere-process name)
	mds-where-swhere-process (get-buffer-process mds-where-swhere-buffer)))

(defun mds-where-start-swhere-process (name)
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
  (if (string= event "finished\n")
      (mds-where-font-lock-swhere-buffer)
    (error "Process: %s had the event `%s'" proc event)))


(defun mds-where-font-lock-swhere-buffer ()
  (with-current-buffer mds-where-swhere-buffer
    (mds-where-mode)
    (goto-char (point-min))
    (forward-line) ;; this can fail 
    (while (re-search-forward mds-where-source-file-re nil t)
      (make-text-button (match-beginning 1) (match-end 1) :type 'mds-where-hyperlink-source))
    (beep)
    (setq buffer-read-only t)))
    
(define-button-type 'mds-where-hyperlink-source
  'help-echo "View file"
  'action 'mds-where-view-file
  'follow-link t
  'face 'link)


;; kind of pointless
(defun mds-where-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mds-where-mode
	mode-name "swhere")
  (run-hooks 'mds-where-mode-hook))

(defun mds-where-start-p4-process (file)
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
  (if (not (string= event "finished\n"))
      (error "Process: %s had the event `%s'" proc event)
    (pop-to-buffer mds-where-display-buffer)
    (mpldoc-mode)
    (setq buffer-read-only t)))
  

;; (mds-where "normal")

(defun mds-where-view-file (button)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward mds-where-source-file-re (line-end-position))
	(let* ((basefile (match-string-no-properties 1))
	       (file (format "%s/%s" mds-where-p4-repo basefile)))
	  (setq mds-where-display-buffer (mds-where-start-p4-process file)
		mds-where-p4-process (get-buffer-process mds-where-display-buffer))))))

  
