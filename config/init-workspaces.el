;;; init-workspaces.el --- -*- lexical-binding: t; -*-


;; TODO Remove if finally deciding to move from projectile to project.el
;; ;;; Perspective
;; ;; Switching from one view to the other
;; (use-package perspective
;;   :custom
;;   (persp-suppress-no-prefix-key-warning t)
;;   :init
;;   (persp-mode)
;;   (lgreen/leader-define-key
;;     "p s" '(persp-switch :wk "Switch perspective")))

;; ;;; Persp-Projectile
;; ;; Perspective and project integration
;; (use-package persp-projectile
;;   :after (projectile perspective))

;; ;;; Centaur-Tabs
;; ;; See your projects in the tabs
;; (use-package centaur-tabs
;;   :after (projectile)
;;   :commands centaur-tabs-mode
;;   :custom
;;   ;; (centaur-tabs-height 24)
;;   (centaur-tabs-style "bar")
;;   (centaur-tabs-set-bar 'over)
;;   (centaur-tabs-modified-marker "•") ;; Unicode Bullet (0x2022)
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-change-fonts "arial" 160)
;;   (centaur-tabs-show-navigation-buttons nil)
;;   (centaur-tabs-enable-ido-completion nil)
;;   (centaur-tabs-cycle-scope 'group)
;;   :general
;;   (:states '(normal)
;;	   "gt" 'centaur-tabs-forward
;;	   "gT" 'centaur-tabs-backward)
;;   :config
;;   (setq centaur-tabs--buffer-show-groups t)
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-group-by-projectile-project)
;;   (centaur-tabs-headline-match))

;;; Tab-bar
;; Put it on my tab
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  init:
  (lgreen/leader-define-key
    "t t" '(toggle-frame-tab-bar :wk "Toggle tab bar")
    "TAB" '(:ignore :wk "Workspace")
    "TAB n" '(tab-bar-switch-to-next-tab  :wk "Tab next")
    "TAB p" '(tab-bar-switch-to-prev-tab  :wk "Tab previous"))
  :config
  (defun lgreen/set-face-tab-bar (&rest _)
    "Set face for tab-bar-tab."
    (message "Setting tab-bar-tab face attributes")
    (set-face-attribute 'tab-bar-tab nil
			:inherit 'doom-modeline-project-dir
			:foreground 'unspecified
			:background 'unspecified))
  (advice-add 'load-theme :after 'lgreen/set-face-tab-bar))

;;; Tabspaces
;; Make tabs work those spaces
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :init
  (lgreen/leader-define-key
    "TAB TAB" '(tabspaces-switch-or-create-workspace :wk "Switch workspace")
    "TAB b" '(tabspaces-switch-buffer-and-tab :wk "Switch to buffer")
    "TAB d" '(tabspaces-close-workspace :wk "Close workspace")
    "TAB o" '(tabspaces-open-or-create-project-and-workspace :wk "Open workspace"))
  :config
  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
	    :narrow   ?w
	    :history  'buffer-name-history
	    :category 'buffer
	    :state    #'consult--buffer-state
	    :default  t
	    :items    (lambda () (consult--buffer-query
				  :predicate #'tabspaces--local-buffer-p
				  :sort 'visibility
				  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  )

;;; _
(provide 'init-workspaces)
