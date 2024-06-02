;;; init-file-tree.el --- -*- lexical-binding: t; -*-

;;; Treemacs
;; Squintting at the forest
(use-package treemacs
  :config
  (lgreen/leader-define-key
    "o p" '(treemacs :wk "Open project tree")))

;; TODO Remove if you decide to stay with project.el over projectile
;; (use-package treemacs-projectile
;;   :after (treemacs projectile))

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; (use-package project-treemacs
;;   :demand t
;;   :after treemacs
;;   :config
;;   (project-treemacs-mode))

;;; Treemacs-Evil
;; The root of all evil
(use-package treemacs-evil
  :after (treemacs evil))

;;; Treemacs-Magit
;; Trees and branches
(use-package treemacs-magit
  :after (treemacs magit))

;;; Treemacs-Tab-Bar
;; Trees at the workspace level
(use-package treemacs-tab-bar
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

;;; _
(provide 'init-file-tree)
