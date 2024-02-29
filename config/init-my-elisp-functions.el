;;; init-my-elisp-functions.el --- -*- lexical-binding: t; -*-

(use-package emacs
  :config

  ;; Set transparency of emacs
  (defun lgreen/transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque:")
    (set-frame-parameter (selected-frame) 'alpha value)))

;;; _
(provide 'init-my-elisp-functions)
