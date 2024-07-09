;; init-cpp.el --- -*- lexical-binding: t; -*-


(use-package emacs
  :ensure nil
  :config
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode)))

;; _
(provide 'init-cpp)
