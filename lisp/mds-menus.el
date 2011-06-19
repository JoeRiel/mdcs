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
     [ "Beep" mds-menu-beep t]
     [ "Echo input" mds-menu-echo-input t]
     [ "Truncate lines" mds-menu-truncate-lines t]
     [ "Compact view"  ]
     [ "Enable buttons"]
     )
    ("Customize"
     ["Set colors"])
    "---"
    ["Find source"  mds-finder t]
    "---"
    ["Restart MDS" mds-restart t]
    ["Quit MDS" mds-quit t]))
  