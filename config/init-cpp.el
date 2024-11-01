;; init-cpp.el --- -*- lexical-binding: t; -*-

;;; C/C++
;; Seeing above the assembly
(use-package cc-mode
  :ensure nil
  :config
  ;; Add mappings for Tree-sitter C and C++ modes
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

;;; _
(provide 'init-cpp)
