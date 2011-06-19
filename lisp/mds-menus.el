(require 'easymenu)

(easy-menu-define mds-mode-menu mds-showstat-mode-map
  "MDS menus"
  '("MDS"
    ("Server Configuration"
     [ "Allow anonymous login" mds-menu-allow-anon t]
     [ "Restrict to localhost" mds-menu-restrict-localhost t]
     "---"
     [ "TCP port" mds-menu-set-port :suffix mds-port]
     "---"
     [ "Idle timeout" mds-menu-idle-timeout t]
     [ "Limit clients" mds-menu-limit-clients t]
     [ "Maximum string length" mds-menu-max-length t]
     )
    ("Window Settings"
     [ "Beep" (mds-menu-toggle mds-menu-beep) :style toggle :selected mds-menu-beep ]
     [ "Echo input" (mds-menu-toggle mds-menu-echo-input) :style toggle :select mds-menu-echo-input]
     [ "Truncate lines" toggle-truncate-lines :style toggle :select truncate-lines]
     [ "Compact view" mds-windows-toggle compact :style toggle :select   ]
;;     [ "Enable buttons"]
     )
;;    ("Customize"
;;     ["Set colors"])
    "---"
    ["Find source"  mds-finder t]
    "---"
    ["Help" mds-help t]
    "---"
    ["Restart MDS" mds-restart t]
    ["Quit MDS" mds-quit t]))
  
(defmacro mds-menu-toggle (flag)
  `(setq ,flag (not ,flag)))

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
  (setq mds-port (mds-read-numeric "port: " mds-port 1000 20000)))


  
