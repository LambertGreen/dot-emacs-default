;;; init-workspaces.el --- -*- lexical-binding: t; -*-


;;; Perspective
;; Switching from one view to the other
(use-package perspective
  :custom
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode)
  (lgreen/leader-define-key
    "p s" '(persp-switch :wk "Switch perspective")))

;;; Persp-Projectile
;; Perspective and project integration
(use-package persp-projectile)

;;; Centaur-Tabs
;; See your projects in the tabs
(use-package centaur-tabs
  :after (projectile)
  :commands centaur-tabs-mode
  :custom
  (centaur-tabs-height 28)
  (centaur-tabs-modified-marker "•") ;; Unicode Bullet (0x2022)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons nil)
  (centaur-tabs-style "bar")
  (centaur-tabs-enable-ido-completion nil)
  (centaur-tabs-cycle-scope 'group)
  :general
  (:states '(normal)
	   "gt" 'centaur-tabs-forward
	   "gT" 'centaur-tabs-backward)
  :config
  (setq centaur-tabs--buffer-show-groups t)
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match))

;;; _
(provide 'init-workspaces)
