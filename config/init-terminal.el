;;; init-terminal.el --- -*- lexical-binding: t; -*-

;;; Vterm
(use-package vterm)

;;; VTerm-toggle
(use-package vterm-toggle
  :after general
  :custom
  (vterm-toggle-fullscreen-p nil)
  :init
  (lgreen/leader-define-key
    "o t" '(vterm-toggle :wk "Open vterm"))
  :config
  (add-to-list 'display-buffer-alist
	       '((lambda (buffer-or-name _)
		   (let ((buffer (get-buffer buffer-or-name)))
		     (with-current-buffer buffer
		       (or (equal major-mode 'vterm-mode)
			   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
		 (display-buffer-reuse-window display-buffer-at-bottom)
		 (reusable-frames . visible)
		 (window-height . 0.4)))
  )

;;; EAT
(use-package eat
  :after evil
  :config
  ;; Set the initial state for specific modes, if necessary
  (evil-set-initial-state 'eat-mode 'insert))

;;; _
(provide 'init-terminal)
