;;; init-folding.el --- -*- lexical-binding: t; -*-


;;; Origami
;; Enable Vim like code folding
(use-package origami
  :ensure t
  :hook
  (prog-mode . origami-mode))

;;; _
(provide 'init-folding)
