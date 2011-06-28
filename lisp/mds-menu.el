(require 'easymenu)

(defcustom mds-menu-beep t
  "If non-nil, then beep for various changes in a client's status."
  :type 'boolean
  :group 'mds)

(defcustom mds-menu-echo-input t
  "If non-nil, then echo executed commands from debugged procedure to output."
  :type 'boolean
  :group 'mds)

(defcustom mds-menu-truncate-showstat t
  "If non-nil, then truncate showstat lines."
  :type 'boolean
  :group 'mds)

(defcustom mds-menu-truncate-output t
  "If non-nil, then truncate output lines."
  :type 'boolean
  :group 'mds)

(defcustom mds-menu-compact-output nil
  "If non-nil, then use the compact form for the output."
  :type 'boolean
  :group 'mds)

(defcustom mds-login-allow-anon nil
  "If non-nil, then allow anonymous login"
  :type 'boolean
  :group 'mds)

(defcustom mds-login-localhost-only nil
  "If non-nil, then allow anonymous login"
  :type 'boolean
  :group 'mds)

(defmacro mds-menu-toggle (title flag)
  "Create menu entry, with TITLE, that toggles FLAG variable.
When activated, it toggles the variable and displays the message
TITLE is enabled|disabled."
  `[,title
    (progn 
      (setq ,flag (not ,flag))
      (message "%s is %s" ',title (if ,flag "enabled" "disabled")))
    :style toggle
    :selected ,flag])


(defvar maple-menu-keymap (make-sparse-keymap))
(easy-menu-define maple-menu maple-menu-keymap
  "Maple Menu"
  `("Maple"
    ("Debugger"
     ("Server Configuration"
      ,(mds-menu-toggle "Allow anonymous login" mds-login-allow-anon)
      ,(mds-menu-toggle "Restrict to localhost" mds-login-localhost-only)
      "---"
      [ "TCP port" mds-menu-set-port :suffix (number-to-string mds-port)]
      "---"
      [ "Idle timeout" mds-menu-idle-timeout t]
      [ "Limit clients" mds-menu-limit-clients :suffix (number-to-string mds-max-number-clients)]
      [ "Maximum string length" mds-menu-max-length t]
      )
     ("Default Window Settings"
      ,(mds-menu-toggle "Beep"  mds-menu-beep)
      ,(mds-menu-toggle "Echo input"  mds-menu-echo-input)
      ,(mds-menu-toggle "Truncate showstat" mds-menu-truncate-showstat)
      ,(mds-menu-toggle "Truncate output" mds-menu-truncate-output)
      ,(mds-menu-toggle "Compact output" mds-menu-compact-output)
      )
     "---"
     ["Debugger help" mds-help t]
     "---"
     ["Restart" mds t]
     ["Stop"    mds-stop t]
     )
    ["Find source"  mds-finder t]
    ["Maple help" mds-help t]
    ))

;; This will go elsewhere; maybe in .emacs.
;;(easy-menu-add-item global-map '("menu-bar") maple-menu "help-menu") 
  
;;(macroexpand '(mds-menu-toggle "Beep" mds-menu-beep))

(defun mds-read-numeric (prompt initial &optional min max )
  (let (n)
    (while
	(progn
	  (let ((str (read-string prompt (number-to-string initial))))
	    (condition-case nil
		(setq n (read str))
	      (error nil)))
	  (unless
	   (and (integerp n)
		(if min (<= min n) t)
		(if max (<= n max) t))
	    (message "Please enter an integer between %s and %s"
		     (if min (number-to-string min) "-infinity")
		     (if max (number-to-string max) "+infinity"))
	    (sit-for 2))))
    n))
	    
(defun mds-menu-set-port ()
  (interactive)
  (setq mds-port (mds-read-numeric "port: " mds-port 1000 20000)))

(defun mds-menu-limit-clients ()
  (interactive)
  (setq mds-port (mds-read-numeric "max clients: " mds-max-number-clients 1)))


(provide 'mds-menu)