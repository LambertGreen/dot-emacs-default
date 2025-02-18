;; init-my-elisp-functions.el --- -*- lexical-binding: t; -*-

;;; My Elisp Functions
;; Funky functionals
(use-package emacs
  :ensure nil
  :commands (lgreen/transparency
             lgreen/remove-background-in-terminal
             lgreen/yank-full-filename
             lgreen/yank-filename
             lgreen/yank-parent-directory)
  :init
;;;; Keymaps
;;;;; File
  (with-eval-after-load 'general
    (lgreen/leader-define-key
      "f y" '(:ignore t :wk "yank")
      "f y y" '(lgreen/yank-full-filename :wk "full-filename")
      "f y f" '(lgreen/yank-filename :wk "filename")
      "f y d" '(lgreen/yank-parent-directory :wk "parent-directory-name")))
;;;; Functions
;;;;; Visuals
  ;; Set transparency of emacs
  (defun lgreen/transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque:")
    (set-frame-parameter (selected-frame) 'alpha value))
  ;; Function to make the background transparent when running in a terminal
  (defun lgreen/remove-background-in-terminal (&optional frame)
    "Unsets the background color in terminal mode."
    (interactive)
    (or frame (setq frame (selected-frame)))
    (unless (display-graphic-p frame)
      (set-face-background 'default "unspecified-bg" frame)))
;;;;; Filename handling
  (defun lgreen/yank-full-filename ()
    "Copy the full path of the current buffer's file to the clipboard."
    (interactive)
    (if buffer-file-name
        (progn
          (kill-new (file-truename buffer-file-name))
          (message "Full filename copied: %s" (file-truename buffer-file-name)))
      (message "No file associated with this buffer.")))
  (defun lgreen/yank-filename ()
    "Copy the name of the current buffer's file to the clipboard."
    (interactive)
    (if buffer-file-name
        (progn
          (kill-new (file-name-nondirectory buffer-file-name))
          (message "Filename copied: %s" (file-name-nondirectory buffer-file-name)))
      (message "No file associated with this buffer.")))
  (defun lgreen/yank-parent-directory ()
    "Copy the parent directory of the current buffer's file to the clipboard."
    (interactive)
    (if buffer-file-name
        (progn
          (kill-new (file-name-directory (file-truename buffer-file-name)))
          (message "Parent directory copied: %s" (file-name-directory (file-truename buffer-file-name))))
      (message "No file associated with this buffer."))))

;;; _
(provide 'init-my-elisp-functions)