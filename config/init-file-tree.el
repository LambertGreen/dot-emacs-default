;; init-file-tree.el --- -*- lexical-binding: t; -*-

;;; Treemacs
;; Squinting at the forest
(use-package treemacs
  :after general
  :commands treemacs
  :custom
  (treemacs-project-follow-mode t)
  (treemacs-width-is-initially-locked nil)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "o p" '(treemacs :wk "Open project tree"))

  ;; Ensure Treemacs is not overriding the universal leader key
  (general-define-key
   :states '(normal motion)
   :keymaps 'treemacs-mode-map
   lgreen/general-leader-key nil)

  :config
  (treemacs-resize-icons 18))

;;; Treemacs-Evil
;; The root of all evil
(use-package treemacs-evil
  :after (treemacs evil)
  :init (eval-after-load 'treemacs '(require 'treemacs-evil)))

;;; Treemacs-Magit
;; Trees and branches
(use-package treemacs-magit
  :after (treemacs magit)
  :init (eval-after-load 'treemacs '(require 'treemacs-magit)))

;;; Treemacs-Tab-Bar
;; Trees at the workspace level
(use-package treemacs-tab-bar
  :after (treemacs)
  :init (eval-after-load 'treemacs '(require 'treemacs-tab-bar))
  :config (treemacs-set-scope-type 'Tabs))

;;; _
(provide 'init-file-tree)