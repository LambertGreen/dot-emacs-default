;;; init-workspaces.el --- -*- lexical-binding: t; -*-


;;; Perspective
;; Switching from one view to the other
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

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
  :bind
  (:map evil-normal-state-map
	("g t" . centaur-tabs-forward)
	("g T" . centaur-tabs-backward))
  :config
  (setq centaur-tabs--buffer-show-groups t)
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  )

;;; _
(provide 'init-workspaces)
