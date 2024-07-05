;;; init-ui.el --- -*- lexical-binding: t; -*-


;;; Doom-Modeline
;; Add Doom's modeline
(use-package doom-modeline
  :disabled t
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

;;; Moody
;; Get into the mood boy
(use-package moody
  :config
  (setq evil-mode-line-format '(before . moody-mode-line-buffer-identification))
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 22)
  (when (memq window-system '(mac ns))
    (setq moody-slant-function #'moody-slant-apple-rgb))
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-mode-line-front-space)
  (moody-replace-eldoc-minibuffer-message-function)

  (setq evil-normal-state-tag   (propertize " 󰬕 " 'face '((:height 178 :weight extrabold)))
        evil-emacs-state-tag    (propertize " 󰬌 " 'face '((:height 178 :weight extrabold :foreground "blue")))
        evil-insert-state-tag   (propertize " 󰬐 " 'face '((:height 178 :weight extrabold :foreground "green")))
        evil-motion-state-tag   (propertize " 󰬔 " 'face '((:height 178 :weight extrabold :foreground "yellow")))
        evil-visual-state-tag   (propertize " 󰬝 " 'face '((:height 178 :weight extrabold :foreground "orange")))
        evil-replace-state-tag  (propertize " 󰬙 " 'face '((:height 178 :weight extrabold :foreground "red")))
        evil-operator-state-tag (propertize " 󰬖 " 'face '((:height 178 :weight extrabold :foreground "violet"))))

  (defun lgreen/customize-vc-mode (orig-func &rest args)
    "Customize the vc-mode string to include a Git icon with custom face."
    (let ((result (apply orig-func args)))
      (when (and vc-mode (stringp vc-mode))
        (let ((git-icon (propertize "  " 'face '((:height 158 :weight extrabold)))))
	  (setq vc-mode
		(concat git-icon (substring vc-mode 4)))))
      result))

  ;; Advise vc-mode-line to apply custom vc-mode formatting
  (advice-add 'vc-mode-line :around #'lgreen/customize-vc-mode))

;;; Minions
;; Keeping the minor modes in line, instead sprawled in the mode line
(use-package minions
  :config (minions-mode 1))

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
  (indent-guide-char "▏")   ; Useful characters: ·│┊┆╎┋▏
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
  (lgreen/set-face-indent-guide)
  (advice-add 'load-theme
	      :after 'lgreen/set-face-indent-guide))

;;; Outshine
;; Org like faces and outlining for non-org modes
(use-package outshine
  :hook (prog-mode . outshine-mode)
  :general
  (:keymaps 'outline-mode-map
            :states 'normal
            "<tab>" 'outline-cycle
            "<backtab>" 'outshine-cycle-buffer
	    "gh" 'outline-up-heading))

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

;;; All-The-Icons
;; A picture is like a thousand words
(use-package all-the-icons)

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

;;; Rainbow-Mode
;; Turning words and numbers into visible light
;; Example: Red (#ff0000), Green (#00ff00), Blue (#0000ff)
(use-package rainbow-mode)

;;; _
(provide 'init-ui)
