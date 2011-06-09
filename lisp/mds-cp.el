;;; mds-conrol-panel.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; Code for the Maple Debugger Control Panel.

;;;

(require 'widget)
(eval-when-compile
  (require 'wid-edit))

;;{{{ declarations

;; avoid compiler warnings

(declare-function mds--get-client-out-buf "mds")
(declare-function mds-showstat-view-dead-proc "mds-showstat")
(declare-function mds-windows-display-dead "mds-windows")

;;}}}
;;{{{ faces

(defface mds-cp-face
  '((((class color) (background dark)) (:foreground "lawn green")))
  "Face for stack arguments."
  :group 'mds-faces)

;;}}}
;;{{{ constants


;;}}}
;;{{{ variables

(defvar mds-cp-ss-buf nil "Live showstat buffer associated with control panel.")

;;}}}

;;{{{ mds-cp-mode

(define-derived-mode mds-cp-mode fundamental-mode "mds-control-panel-mode"
  "Long winded description"
  :group 'mds

  (delete-region (point-min) (point-max))

  (widget-insert "MDS Control Panel\n\n")
  (widget-create 'push-button "Next")
  (widget-create 'push-button "Into")
  (widget-create 'push-button "Step")
  (widget-create 'push-button "Cont")

  (widget-create 'push-button "Outfrom")
  (widget-create 'push-button "Return")
  (widget-create 'push-button "Done")

  (widget-setup)
  )


;;}}}

(provide 'mds-cp)

;; mds-control-panel.el ends here
