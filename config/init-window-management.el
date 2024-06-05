;;; init-window-management.el --- -*- lexical-binding: t; -*-

;;; Winner mode
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init
  (lgreen/leader-define-key
    "w r" 'winner-redo
    "w u" 'winner-undo))

(use-package popper
  :bind (("C-'"   . popper-toggle)
	 ("C-M-'"   . popper-cycle))
  :init
  (lgreen/leader-define-key
    "`" '(popper-toggle :wk "Toggle popup")
    "t p" '(popper-toggle-type :wk "Toggle popper"))
  (setq popper-reference-buffers
	'("\\*eat\\*"
	  "\\*.*-eat\\*"
	  eshell-mode    ;eshell as popups
	  shell-mode     ;shell as popups
	  term-mode      ;term as popups
	  vterm-mode     ;vterm as popups
	  help-mode      ;help windows
	  compilation-mode)) ;compilation buffers
  (setq popper-group-function #'popper-group-by-project)
  (popper-mode +1)
  (popper-echo-mode +1))

;;; Shackle
;; keep em secure
;; TODO Do we need Shackle or is the display `alist' sufficient?
;; (use-package shackle
;;   :config
;;   (shackle-mode 1))

;;; _
(provide 'init-window-management)
