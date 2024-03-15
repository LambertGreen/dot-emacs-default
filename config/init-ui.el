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
  :config
  (doom-modeline-mode 1)
  ;; Add toggle keybind
  (lgreen/leader-keys
    "t m" '(:ignore t :wk "modeline")
    "t m e" '(lgreen/toggle-doom-modeline-buffer-encoding :which-key "toggle modeline encoding"))
  (defun lgreen/toggle-doom-modeline-buffer-encoding ()
    "Toggle the doom-modeline-buffer-encoding variable"
    (interactive)
    (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding))))

;;; HL-Todo
;; Highlight TODO's
(use-package hl-todo
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
  (indent-guide-char "┊")   ; Useful characters: ·│┊┆╎
  ;; (indent-guide-recursive t)
  :hook
  (prog-mode . indent-guide-mode)
  :config
  ;; Set the indent-guide color to match the comment color of the current theme
  (set-face-foreground 'indent-guide-face
		       (face-foreground 'font-lock-comment-face))
  ;; Add toggle
  (lgreen/leader-keys
    "t i" '(lgreen/toggle-indent-guide :wk "Toggle indent guides"))

  (defun lgreen/toggle-indent-guide ()
    "Toggle indent guides in programming modes."
    (interactive)
    (if (derived-mode-p 'prog-mode)
	(indent-guide-mode (if indent-guide-mode -1 1))
      (message "Not in a programming mode!"))))

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
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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
