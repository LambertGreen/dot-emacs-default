;;; init-ui.el --- -*- lexical-binding: t; -*-


;;; Doom-Modeline
;; Add Doom's modeline
(use-package doom-modeline
  :custom
  (doom-modeline-height 20)
  ;; (doom-modeline-indent-info t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-minor-modes nil)
  ;; :hook (after-init . doom-modeline-mode)
  :init
  (lgreen/leader-define-key
    "t m" '(:ignore t :wk "modeline")
    "t m m" '(lgreen/toggle-mode-line :wk "Toggle modeline")
    "t m e" '(lgreen/toggle-doom-modeline-buffer-encoding :which-key "toggle modeline encoding"))
  :config
  (doom-modeline-mode 1)
  (defun lgreen/toggle-doom-modeline-buffer-encoding ()
    "Toggle the doom-modeline-buffer-encoding variable"
    (interactive)
    (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding)))
  (defun lgreen/toggle-mode-line ()
    "Toggle the visibility of the mode line."
    (interactive)
    (if (eq mode-line-format nil)
	(setq mode-line-format (default-value 'mode-line-format))
      (setq mode-line-format nil))
    (force-mode-line-update)
    (redraw-display)))

;;; Spaceline
;; The mode line from Spacemacs
(use-package spaceline
  :disabled t
  :custom
  ( powerline-default-separator 'contour)
  ( powerline-gui-use-vcs-glyph t)
  ( powerline-height 22)
  (spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ( spaceline-workspace-numbers-unicode t)
  ( spaceline-window-numbers-unicode t)
  ( spaceline-separator-dir-left '(left . right))
  ( spaceline-separator-dir-right '(right . left))
  ( spaceline-flycheck-bullet "❀ %s")
  ;; (powerline-default-separator 'utf-8)
  ;; (spaceline-minor-modes-separator " ")
  ;; (spaceline-window-numbers-unicode t)
  ;; (spaceline-workspace-numbers-unicode t)
  :config
  (require `spaceline-config)
  (spaceline-spacemacs-theme))

;;; Spaceline-All-The-Icons
(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (spaceline-all-the-icons-theme))

;;; Solaire-Mode
;; Darken popup buffers
(use-package solaire-mode
  :config (solaire-global-mode +1))

;;; HL-Todo
;; Highlight TODO's
(use-package hl-todo
  ;; Ensure is elpaca specifier:
  ;; Given hl-todo default does not specify version, but it is required to
  ;; install dependencies we indicate the version.
  :ensure (hl-todo :version (lambda (_) "3.6.0"))
  :hook ((org-mode . hl-todo-mode)
	 (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(("TODO"       warning bold)
	  ("FIXME"      error bold)
	  ("BUG"        error bold)
	  ("HACK"       font-lock-constant-face bold)
	  ("REVIEW"     font-lock-keyword-face bold)
	  ("NOTE"       success bold)
	  ("DEPRECATED" font-lock-doc-face bold))))

;;; Indent-Guide
;; Know what vertical you are on
(use-package indent-guide
  :custom
  ;; (indent-guide-recursive t)
  (indent-guide-char "│")   ; Useful characters: ·│┊┆╎┋▏
  :init
  (lgreen/leader-define-key
    "t i" '(lgreen/toggle-indent-guide :wk "Toggle indent guides"))
  :hook (prog-mode . indent-guide-mode)
  :config
  (defun lgreen/toggle-indent-guide ()
    "Toggle indent guides in programming modes."
    (interactive)
    (if (derived-mode-p 'prog-mode)
	(indent-guide-mode (if indent-guide-mode -1 1))
      (message "Not in a programming mode!")))
  (defun lgreen/set-face-indent-guide (&rest _)
    "Set the indent-guide color to match the comment color of the current theme"
    (set-face-attribute 'indent-guide-face nil
			:inherit 'font-lock-comment-face
			:foreground 'unspecified
			:background 'unspecified))
  (advice-add 'load-theme
	      :after 'lgreen/set-face-indent-guide))

;;; Outshine
;; Org like faces and outlining for non-org modes
(use-package outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;; Nerd-Icons
;; Fancy icons
(use-package nerd-icons)

;;; Nerd-Icons-Completion
;; Fancy icons in completion window
(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

;;; Pulsar
;; Know what line point is on after movement
(use-package pulsar
  :custom
  ((pulsar-pulse t)
   (pulsar-delay 0.05)
   (pulsar-iterations 10)
   (pulsar-face 'pulsar-magenta)
   (pulsar-highlight-face 'pulsar-yellow)
   (pulsar-pulse-functions
    '(isearch-repeat-forward
      isearch-repeat-backward
      evil-search-next
      evil-search-previous
      evil-ex-search-next
      evil-ex-search-previous
      evil-ex-search-forward
      evil-ex-search-backward
      evil-avy-goto-line
      evil-avy-goto-char
      evil-avy-goto-char-timer
      evil-avy-goto-word-0
      evil-avy-goto-word-1
      evil-scroll-line-to-top
      evil-scroll-line-to-center
      evil-scroll-line-to-bottom
      evilem-motion-next-line
      evilem-motion-previous-line
      recenter-top-bottom
      move-to-window-line-top-bottom
      reposition-window
      bookmark-jump
      other-window
      delete-window
      delete-other-windows
      forward-page
      backward-page
      scroll-up-command
      scroll-down-command
      windmove-right
      windmove-left
      windmove-up
      windmove-down
      windmove-swap-states-right
      windmove-swap-states-left
      windmove-swap-states-up
      windmove-swap-states-down
      tab-new
      tab-close
      tab-next
      org-next-visible-heading
      org-previous-visible-heading
      org-forward-heading-same-level
      org-backward-heading-same-level
      outline-backward-same-level
      outline-forward-same-level
      outline-next-visible-heading
      outline-previous-visible-heading
      outline-up-heading
      evil-scroll-up
      evil-scroll-down
      evil-scroll-page-up
      evil-scroll-page-down
      eshell-previous-prompt
      eshell-next-prompt)))
  :config
  (pulsar-global-mode 1))

;;; Olivetti
;; Give yourself some breathing room
(use-package olivetti
  :custom
  (olivetti-body-width 130)
  :init
  (lgreen/leader-define-key
    "t o" '(olivetti-mode :wk "Toggle ovlivetti"))
  :hook ((org-mode
	  text-mode
	  dired-mode
	  magit-mode
          emacs-lisp-mode) . olivetti-mode))

;;; Perfect-Margin
;; Give yourself some breathing room
(use-package perfect-margin
  :disabled t
  :custom
  (perfect-margin-visible-width 130)
  :hook ((org-mode
	  text-mode
	  dired-mode
          emacs-lisp-mode) . perfect-margin-mode)
  :config
  ;; auto-center minibuffer windows
  ;; (setq perfect-margin-ignore-filters nil)
  ;; auto-center special windows
  ;; (setq perfect-margin-ignore-regexps nil)
  ;; add additinal bding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))


;;; _
(provide 'init-ui)
