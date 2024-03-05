;;; init-workspaces.el --- -*- lexical-binding: t; -*-

(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package persp-projectile
  :ensure t)

;; TODO Try this package out
;; (use-package centaur-tabs
;;   :ensure t
;;   :custom
;;   (centaur-tabs--buffer-show-groups t)
;;   (centaur-tabs-cycle-scope 'groups)
;;   :bind
;;   (:map evil-normal-state-map
;;         ("g t" . centaur-tabs-forward)
;;         ("g T" . centaur-tabs-backward))
;;   :config
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-group-by-projectile-project)
;;   (centaur-tabs-headline-match)
;;   )

;;; _
(provide 'init-workspaces)
