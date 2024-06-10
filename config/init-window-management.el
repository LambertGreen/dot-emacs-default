;;; init-window-management.el --- -*- lexical-binding: t; -*-

;;; Winner mode
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init
  (lgreen/leader-define-key
    "w r" 'winner-redo
    "w u" 'winner-undo))

;;; Popper
;; What's popping?
(use-package popper
  :bind (("C-'" . popper-toggle)
	 ("C-M-'" . popper-cycle))
  :custom
  (popper-reference-buffers
   '("\\*eat\\*"
     "\\*.*-eat\\*"
     eshell-mode    ; eshell as popups
     shell-mode     ; shell as popups
     term-mode      ; term as popups
     vterm-mode     ; vterm as popups
     help-mode      ; help windows
     compilation-mode)) ; compilation buffers
  (popper-group-function #'popper-group-by-project)
  (popper-display-control 'user)
  :init
  (lgreen/leader-define-key
    "`" '(popper-toggle :wk "Toggle popup")
    "t p" '(popper-toggle-type :wk "Toggle popup type"))
  (popper-mode +1)
  (popper-echo-mode +1))

  :config
  (add-to-list 'display-buffer-alist
	       '("\\*.*eat\\*"
		 (display-buffer-reuse-window
		  display-buffer-at-bottom)
		 (window-height . 0.3)))

  (defun close-window-on-eat-buffer-kill ()
    "Close the window when an eat buffer is killed."
    (when (string-match-p "\\*.*eat\\*" (buffer-name))
      (delete-window)))

  (add-hook 'kill-buffer-hook 'close-window-on-eat-buffer-kill)
  )

;; ;;; Shackle
;; ;; keep em secure
;; ;; TODO Consider using Shackle for managing popup window locations
;; (use-package shackle
;;   :config
;;   (shackle-mode 1))

;;; _
(provide 'init-window-management)
