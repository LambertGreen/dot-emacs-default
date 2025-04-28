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
;;;;; Shell theme
  (defun lgreen/set-shell-theme-env ()
    "Set LGREEN_SHELL_THEME_MODE for shells based on Emacs theme background mode."
    (let ((mode (if (eq (frame-parameter nil 'background-mode) 'light)
                    "light"
                  "dark")))
      (setenv "LGREEN_SHELL_THEME_MODE" mode)))
;;;;; Font Diagnostics
  (defun lgreen/nerd-font-test-sheet ()
    "Insert a test sheet of Nerd Font icons into a new scratch buffer."
    (interactive)
    (let ((buf (generate-new-buffer "*Nerd Font Test Sheet*")))
      (switch-to-buffer buf)
      (insert (propertize "🖋 Nerd Font Icon Test Sheet\n\n" 'face '(:height 2.0 :weight bold)))
      (dolist (char '(
                      ;; Popular Nerd Font icons (private use area)
                      #xe5ff ; Folder
                      #xf015 ; Home
                      #xf07b ; Folder open
                      #xf0c5 ; Copy
                      #xf0e7 ; Lightning bolt
                      #xf11c ; Hdd
                      #xf120 ; Terminal
                      #xf121 ; Code
                      #xf13b ; File text
                      #xf17b ; GitHub
                      #xf1c9 ; File pdf
                      #xf1d8 ; Linux
                      #xf1e0 ; Server
                      #xf1f9 ; Apple
                      #xf23a ; Windows
                      #xf2f1 ; Battery
                      #xf3ed ; Docker
                      ;; Add more if you like!
                      ))
        (insert (format "%s  (U+%04X)\n" (char-to-string char) char)))
      (goto-char (point-min))
      (read-only-mode 1)))
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
