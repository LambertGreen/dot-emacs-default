;;; init-my-elisp-functions.el --- -*- lexical-binding: t; -*-

;;; My Functions
;; Funky functionals
(use-package emacs
  :ensure nil
  :config

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

  ;; Insert the current filename at point
  (defun lgreen/insert-current-filename ()
    "Insert the current file name at the point."
    (interactive)
    (let ((filename (if (buffer-file-name)
			(file-name-nondirectory (buffer-file-name))
		      "Buffer has no file name.")))
      (insert filename))))
;;; _
(provide 'init-my-elisp-functions)
