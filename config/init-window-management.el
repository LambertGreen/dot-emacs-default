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
;; NOTE: Use the universal argument before toggling to keep popups showing
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

;;; Customize eat terminal popup windows
(use-package emacs
  :ensure nil
  :config
  ;; Make terminal (eat) buffer show in right window
  (add-to-list 'display-buffer-alist
	       '("\\*.*eat\\*"
		 (display-buffer-in-side-window)
		 (side . bottom)
		 (display-buffer-reuse-window
		  display-buffer-at-bottom)
		 (window-height . 0.4)))

  (defun close-window-on-eat-buffer-kill ()
    "Close the window when an eat buffer is killed."
    (when (string-match-p "\\*.*eat\\*" (buffer-name))
      (delete-window)))

  (add-hook 'kill-buffer-hook 'close-window-on-eat-buffer-kill))

;;; Customize compilation windows
(use-package emacs
  :ensure nil
  :config
  ;; Make compilation buffer show in right window
  (add-to-list 'display-buffer-alist
               '("\\*compilation\\*"
		 (display-buffer-in-side-window)
		 (side . right)
		 (slot . 1)
		 (window-width . 0.5)))

  (require 'ansi-color)
  (defun lgreen/colorize-compilation-buffer ()
    "Apply ANSI color codes to the compilation buffer."
    (when (derived-mode-p 'compilation-mode)
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'lgreen/colorize-compilation-buffer))

;; ;;; Shackle
;; ;; keep em secure
;; ;; TODO Consider using Shackle for managing popup window locations
;; (use-package shackle
;;   :config
;;   (shackle-mode 1))

;;; _
(provide 'init-window-management)
