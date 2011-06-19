(require 'easymenu)

(easy-menu-define mds-mode-menu '(mds-showstat-mode-map mds-output-mode-map)
  "MDS menus"
  '("MDS"
    ("Server Configuration"
     [ "Allow anonymous login" mds-menu-allow-anon t]
     [ "Restrict to localhost" mds-menu-restrict-localhost t]
     "---"
     [ "TCP port" mds-menu-set-port t ]
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
     [ "Enable buttons"]
     )
    ("Customize"
     ["Set colors"])
    "---"
    ["Find source"  mds-finder t]
    "---"
    ["Help" mds-help t]
    "---"
    ["Restart MDS" mds-restart t]
    ["Quit MDS" mds-quit t]))
  
(defmacro mds-menu-toggle (flag)
  `(setq ,flag (not ,flag)))

(setq flg t)
(mds-menu-toggle flg)



(defun mds-menu-limit-clients 