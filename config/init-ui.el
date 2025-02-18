;; init-ui.el --- -*- lexical-binding: t; -*-

;;; Doom-Modeline
;; Add Doom's modeline
(use-package doom-modeline
  :demand t
  :custom
  (column-number-mode 1)
  (doom-modeline-height 24)
  (doom-modeline-indent-info t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-minor-modes t)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "t m" '(:ignore t :wk "modeline")
    "t m m" '(lgreen/toggle-mode-line :wk "Toggle modeline")
    "t m e" '(lgreen/toggle-doom-modeline-buffer-encoding :which-key "toggle modeline encoding")
    "t m i" '(lgreen/toggle-doom-modeline-indent-info :which-key "toggle modeline indent-info"))
  :config
  (doom-modeline-mode 1)
;;;; Functions
  (defun lgreen/toggle-doom-modeline-buffer-encoding ()
    "Toggle the doom-modeline-buffer-encoding variable"
    (interactive)
    (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding)))
  (defun lgreen/toggle-doom-modeline-indent-info ()
    "Toggle the `doom-modeline-indent-info` variable."
    (interactive)
    (setq doom-modeline-indent-info (not doom-modeline-indent-info)))
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
;; NOTE: We disable Moody as we are back to using Doom-Modeline
(use-package moody
  :disabled t
  :after evil
  :custom
  (column-number-mode 1)
  :init
  (lgreen/leader-define-key
    "t m" '(:ignore t :wk "modeline")
    "t m m" '(lgreen/toggle-mode-line :wk "Toggle modeline"))
  :config
  (defun lgreen/toggle-mode-line ()
    "Toggle the visibility of the mode line."
    (interactive)
    (if (eq mode-line-format nil)
        (setq mode-line-format (default-value 'mode-line-format))
      (setq mode-line-format nil))
    (force-mode-line-update)
    (redraw-display))
  (setq evil-mode-line-format '(before . moody-mode-line-buffer-identification))
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 22)
  (when (memq window-system '(mac ns))
    (setq moody-slant-function #'moody-slant-apple-rgb))
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-mode-line-front-space)
  (moody-replace-eldoc-minibuffer-message-function)
  ;; Customize evil state colors to match the colors from diff-mode
  (setq evil-normal-state-tag   (propertize " 󰬕 " )
        evil-emacs-state-tag    (propertize " 󰬌 " 'face '((:inherit diff-header :background unspecified)))
        evil-insert-state-tag   (propertize " 󰬐 " 'face '((:inherit diff-added :background unspecified)))
        evil-motion-state-tag   (propertize " 󰬔 " 'face '((:inherit diff-removed :background unspecified)))
        evil-visual-state-tag   (propertize " 󰬝 " 'face '((:inherit diff-changed :background unspecified)))
        evil-replace-state-tag  (propertize " 󰬙 " 'face '((:inherit diff-removed :background unspecified)))
        evil-operator-state-tag (propertize " 󰬖 " 'face '((:inherit diff-removed :background unspecified))))
  ;; Customize the `vc-mode'
  (defun lgreen/customize-vc-mode (orig-func &rest args)
    "Customize the vc-mode string to include a Git icon with custom face."
    (let ((result (apply orig-func args)))
      (when (and vc-mode (stringp vc-mode))
        (let ((git-icon (propertize "  " 'face '((:weight extrabold)))))
          (setq vc-mode
                (replace-regexp-in-string "^ Git[:-]" git-icon vc-mode))))
      result))
  ;; Advise vc-mode-line to apply custom vc-mode formatting
  (advice-add 'vc-mode-line :around #'lgreen/customize-vc-mode))

;;; Minions
;; Keeping the minor modes in line, instead sprawled in the mode line
(use-package minions
  :defer 1
  :config (minions-mode 1))

;;; Anzu
;; Display current match and total matches in search modes
(use-package anzu
  :defer 1
  :config (global-anzu-mode 1))

;;; Yascroll
;; An indicator to how long the papyrus is
;; NOTE: Does not render correctly in Org-Mode source blocks,
;; hence disabling
(use-package yascroll
  :defer 2
  :disabled t
  :config
  (defun lgreen/yascroll-update-colors ()
    "Set `yascroll' scroll-bar color to match the `highlight` face."
    (interactive)
    (when (face-background 'highlight)
      (custom-set-faces
       `(yascroll:thumb-fringe ((t (:background ,(face-background 'highlight)
                                                :foreground ,(face-background 'highlight))))))))
  (add-hook 'after-load-theme-hook #'lgreen/yascroll-update-colors)
  (global-yascroll-bar-mode 1))

;;; Solaire-Mode
;; Darken popup buffers
(use-package solaire-mode
  :init
  (solaire-global-mode 1)
  ;; Prevent solaire-mode from overriding the modeline faces. The difference in
  ;; color between normal buffers and special is confusing and annoying.
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (setf (alist-get face solaire-mode-remap-alist) nil)))

;;; HL-Todo
;; Highlight TODO's
(use-package hl-todo
  ;; Ensure is elpaca specifier:
  ;; Given hl-todo default does not specify version, but it is required to
  ;; install dependencies we indicate the version.
  :ensure (hl-todo :version (lambda (_) "3.6.0"))
  :hook ((prog-mode org-mode) . hl-todo-mode)
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

;;; Display-Fill-Column-Indicator
(use-package display-fill-column-indicator
  :ensure nil
  :after theme-settings
  :hook (prog-mode . display-fill-column-indicator-mode)
  :custom
  (fill-column 120)
  (display-fill-column-indicator-character
   (plist-get '( triple-pipe  ?┆
                 double-pipe  ?╎
                 double-bar   ?║
                 solid-block  ?█
                 empty-bullet ?◦)
              'double-pipe))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "t f" '(lgreen/toggle-fill-column-indicator :wk "Toggle fill-column indicator"))
  :config
;;;; Functions
  (defun lgreen/set-face-for-fill-column-indicator()
    "Sets the fill column indicator face to match the current theme's comment color."
    (interactive)
    (set-face-attribute 'fill-column-indicator nil
                        :foreground (face-attribute 'font-lock-comment-face :foreground)
                        :inherit 'font-lock-comment-face
                        :weight 'thin
                        :height 0.5)
    )
  (defun lgreen/toggle-fill-column-indicator ()
    "Toggle the fill column indicator."
    (interactive)
    (if display-fill-column-indicator-mode
        (display-fill-column-indicator-mode -1)
      (display-fill-column-indicator-mode 1)))

  (lgreen/set-face-for-fill-column-indicator)
  (advice-add 'load-theme
              :after 'lgreen/set-face-for-fill-column-indicator))

;;; Indent-Bars
;; Know what vertical you are on
(use-package indent-bars
  :ensure (:fetcher github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.2)
  (indent-bars-highlight-current-depth '(:blend 0.9))
  (indent-bars-prefer-character t)
  (indent-bars-starting-column 0)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-treesit-support t)
  :hook (prog-mode . indent-bars-mode)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "t i" '(indent-bars-mode :wk "Toggle indent bars")))

;;; Outline
;; The beginning of something.. useful hopefully
(use-package outline
  :ensure nil
  :after general
  :hook ((emacs-lisp-mode conf-mode) . outline-minor-mode)
  :init
;;;; Keymaps
  (general-def
    :keymaps 'outline-mode-map
    :states 'normal
    "<tab>" 'outline-cycle
    "gh" 'outline-up-heading)
  (lgreen/leader-define-key
    :keymaps 'outline-mode-map
    "z" '(:ignore t :wk "Outline")
    "z a" '(outline-show-all :wk "Show all")
    "z m" '(outline-hide-body :wk "Fold all")
    "z c" '(outline-hide-subtree :wk "Hide sub-tree")
    "z o" '(outline-show-subtree :wk "Show sub-tree")
    "z t" '(outline-toggle-children :wk "Toggle sub-tree")
    "z u" '(outline-up-heading :wk "Up heading")
    "z n" '(outline-next-visible-heading :wk "Next heading")
    "z p" '(outline-previous-visible-heading :wk "Previous heading")))

;;; Outshine
;; Org like faces and outlining for non-org modes
(use-package outshine
  :after outline
  :hook ((emacs-lisp-mode conf-mode) . outshine-mode))

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
  :after (evil org)
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
      markdown-next-visible-heading
      markdown-previous-visible-heading
      evil-scroll-up
      evil-scroll-down
      evil-scroll-page-up
      evil-scroll-page-down
      eshell-previous-prompt
      eshell-next-prompt)))
  :init
  (pulsar-global-mode 1))

;;; Olivetti
;; Give yourself some breathing room
(use-package olivetti
  :custom
  (olivetti-body-width 130)
  :hook ((org-mode
          text-mode
          dired-mode
          magit-mode
          emacs-lisp-mode) . olivetti-mode)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "t o" '(olivetti-mode :wk "Toggle olivetti")))

;;; Perfect-Margin
;; Give yourself some breathing room
;; NOTE: Disabled: Using Olivetti-Mode instead
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
  ;; add additional binding on margin area
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

;;; Breadcrumb
;; Like how Hansel & Gretel got back home
(use-package breadcrumb
  :ensure (:fetcher github :repo "joaotavora/breadcrumb")
  :init (breadcrumb-mode t))

;;; Nyan Mode
;; Rainbow and cat
(use-package nyan-mode
  :init
  (when (display-graphic-p)
    (nyan-mode)))

;;; _
(provide 'init-ui)