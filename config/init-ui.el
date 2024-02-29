;;; init-ui.el --- -*- lexical-binding: t; -*-

;; Get a beautiful and functional theme
(use-package catppuccin-theme
  :ensure t
  :config (load-theme 'catppuccin :no-confirm))

;; Add Doom's modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Highlight TODO's
(use-package hl-todo
  :ensure t
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

;; Fancy icons
(use-package nerd-icons
  :ensure t)

;; Fancy icons for Dired
(use-package nerd-icons-dired
  :ensure t
  :after (:all nerd-icons dired)
  :hook (dired-mode . nerd-icons-dired-mode))

;; Fancy icons in completion window
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Know what line point is on after movement
(use-package pulsar
  :ensure t
  :custom
  ((pulsar-pulse t)
   (pulsar-delay 0.05)
   (pulsar-iterations 10)
   (pulsar-face 'pulsar-magenta)
   (pulsar-highlight-face 'pulsar-yellow)

   (pulsar-pulse-functions
    '(isearch-repeat-forward
      isearch-repeat-backward
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

;;; _
(provide 'init-ui)
