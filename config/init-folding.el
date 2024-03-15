;;; init-folding.el --- -*- lexical-binding: t; -*-


;;; Origami
;; Enable Vim like code folding
(use-package origami
  :hook
  (prog-mode . origami-mode))

;;; _
(provide 'init-folding)
