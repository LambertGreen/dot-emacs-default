;; init-window-management.el --- -*- lexical-binding: t; -*-

;;; Winner mode
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "w r" 'winner-redo
    "w u" 'winner-undo))

;;; Ace Window
(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;;; Keymaps
  :init
  (lgreen/leader-define-key
    "w SPC " 'ace-window))

;;; Popper
;; What's popping?
;; Usage notes:
;; - Use the universal argument before toggling to keep popups showing
;; - Use the universal argument twice for toggling all
(use-package popper
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
;;;; Keymaps
  :bind (("C-'" . popper-toggle)
         ("C-M-'" . popper-cycle))
  :init
  (lgreen/leader-define-key
    "`" '(popper-toggle :wk "toggle popup")
    "t p" '(popper-toggle-type :wk "toggle popup type"))
;;;; -
  (popper-mode +1)
  (popper-echo-mode +1))

;;; Customize compilation windows
(use-package compilation-mode
  :ensure nil
  :hook (compilation-filter . lgreen/colorize-compilation-buffer)
  :config
  ;; Make compilation buffer show in right window
  (add-to-list 'display-buffer-alist
               '("\\*compilation\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 1)
                 (window-width . 0.5)))
;;;; Functions
  (defun lgreen/colorize-compilation-buffer ()
    "Apply ANSI color codes to the compilation buffer."
    (require 'ansi-color)
    (when (derived-mode-p 'compilation-mode)
      (ansi-color-apply-on-region (point-min) (point-max)))))

;;; Shackle
;; keep em secure
;; TODO Consider using Shackle for managing popup window locations
(use-package shackle
  :disabled t
  :config
  (shackle-mode 1))

;;; _
(provide 'init-window-management)

;; Local Variables:
;; jinx-local-words: "eshell vterm"
;; End:
