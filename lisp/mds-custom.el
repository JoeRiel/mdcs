;;; mds-custom.el --- Maple Debugger Customizations



;;; Commentary:
;; 

;;; Code:

(defgroup mds nil
  "Maple Debugger Server."
  :group 'tools)

(defcustom mds-port 10000  "Port used by mds server."
  :type 'integer
  :group 'mds)


(defcustom mds-show-args-flag t
  "Non-nil means display args on entry to procedure."
  :type 'boolean
  :group 'mds)

(defcustom mds-track-input-flag t
  "Non-nil means track (echo) the input line to the output buffer after each command."
  :type 'boolean
  :group 'mds)

(defcustom mds-stop-trace-at-trapped-error-flag nil
  "Non-nil means stop tracing when a trapped error occurs.
This only has an effect if error trapping is also enabled.
Use `mds-toggle-stop-trace-at-trapped-error' to toggle this variable."
  :type 'boolean
  :group 'mds)

(defcustom mds-log-messages-flag nil
  "Non-nil means write all messages to `mds-log-buffer'."
  :type 'boolean
  :group 'mds)

(defgroup mds nil
  "Maple Debugger Server."
  :group 'tools)

(defcustom mds-truncate-lines-flag t
  "Non-nil means lines in showstat buffer are initially truncated."
  :type 'boolean
  :group 'mds)

(defcustom mds-wait-until-ready-flag t
  "Non-nil means do not send input to Maple until prompt has been received.
Setting this to nil allows a quicker response, but prevents a notification
that the debugger may have exited."
  :type 'boolean
  :group 'mds)

(defcustom mds-use-lineinfo-flag t
  "Non-nil means use lineinfo, if available."
  :type 'boolean
  :group 'mds)

(defcustom mds-query-on-exit-flag nil
  "Non-nil means query for confirmation to kill mds process when exiting Emacs."
  :type 'boolean
  :group 'mds)

;;{{{ Cursors

(defcustom mds-cursor-waiting 'hollow
  "Cursor used in showstat buffer when waiting for Maple to respond."
  :type 'symbol
  :group 'mds)

(defcustom mds-cursor-ready 'box
  "Cursor used in showstat buffer when ready for a user input."
  :type 'symbol
  :group 'mds)

;;}}}
;;{{{ Source Location

(defcustom mds-maple-root-alist nil
  "Alist mapping a Maple version to the library source directory.
Each element has the form \(version maple-root\).

VERSION comes from the string returned by kernelopts(version) and 
includes the decimal and fractional part; for example, \"2016.2\".

MAPLE-ROOT is the directory of the installed Maple sandbox;
for example, \"/home/joe/maplesoft/sandbox/2016.2\".

This is used to expand a leading right angle-bracket (>) in line-info data
to the corresponding directory; it is only useful if the source files
for the Maple library code are available."
  :group 'mds
  :type '(alist :key-type   (string :tag "Version")
		:value-type (directory :tag "Maple root")))

;;}}}
;;{{{ Window manager

(defgroup mds-wm nil
  "Maple Debugger Server Window Manager."
  :group 'mds)

(defcustom mds-wm-side-by-side t
  "If non-nil, display the showstat and output windows side-by-side in a single client view."
  :type 'boolean
  :group 'mds-wm)

(defcustom mds-wm-ss-fractional-size nil
  "Fractional size of the showstat window in relation to the frame size.
Width is used when `mds-wm-side-by-side' is non-nil, otherwise height is used.
If nil, the window is set to half the frame size."
  :type 'float
  :group 'mds-wm)

;;}}}

(provide 'mds-custom)

;;; mds-custom.el ends here
