;; This is a minimal .emacs file suitable for using the Maple
;; Debugger.  These elisp commands should be copied to your .emacs
;; file (typically located at $HOME/.emacs).  A fresh Emacs install
;; does not create such a file, so if you do not have one, just copy
;; this file.  To find out where Emacs thinks the .emacs files lies
;; (and is named) check the value of user-init-file.  Do this with
;; (M-x describe-variable user-init-file)

;; Add $HOME/.emacs.d/maple to load-path.  This is where
;; mds, maplev, and friends are located.
(add-to-list 'load-path (concat user-emacs-directory "maple"))

;; Make mds available
(autoload 'mds "mds" "Restart the Maple Debugger server" t)

;; Uncomment the following lines to assign global keys (f12 and C-f12)
;; that cycle and combine the available debugger clients.

;; (global-set-key [f12] 'mds-wm-cycle-clients)
;; (global-set-key [C-f12] 'mds-wm-display-all)

;; Start the debugger server
(mds)

;; Add $HOME/share/info, if it exists, to the default path searched by Emacs help
(let ((info-dir (concat (getenv "HOME") "/share/info/")))
  (if (file-exists-p info-dir)
      (add-to-list 'Info-default-directory-list info-dir)))

;; Make maplev-mode available
(autoload 'maplev-mode "maplev" "Maple editing mode" t)

;; Associate the extensions mpl, mm, and mi with maplev-mode.
;; You will want to modify this if you use mpldoc-mode.
(setq auto-mode-alist (cons (cons (concat "\\." (regexp-opt '("mpl" "mm" "mi") t) "$")
				  'maplev-mode)
			    auto-mode-alist))

;; end of .emacs
