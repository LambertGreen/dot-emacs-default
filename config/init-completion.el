;; init-completion.el --- -*- lexical-binding: t; -*-

;;; General Completion Settings
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; We use cape-dict with a frequency-sorted word list instead.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
;;;; Keymaps
  :bind (("C-;" . completion-at-point)))

;;; Dabbrev
;; Short and sweet
;; NOTE: No explicit binding - dabbrev is available via cape-dabbrev in C-; completion
(use-package dabbrev
  :ensure nil)

;;; Completion Preview
;; You do the starting but the ghost does the ending
(use-package completion-preview
  :ensure nil
  :hook ((text-mode prog-mode) . completion-preview-mode)
;;;; Keymaps
  :bind (:map completion-preview-active-mode-map
              ("C-j" . completion-preview-next-candidate)
              ("C-k" . completion-preview-prev-candidate)))

;;; Corfu
;; Just completion-at-point UI
(use-package corfu
  :after dabbrev
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)               ;; Disable auto completion - invoke manually with `completion-at-point' binding
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Enable Corfu globally, but exclude fundamental-mode to keep it clean
  (global-corfu-modes '((not fundamental-mode) t))
;;;; Keymaps
  :bind (:map corfu-map
              ("TAB" . corfu-expand)
              ([tab] . corfu-expand)
              ("M-m" . corfu-move-to-minibuffer))
;;;; Functions
  :preface
  (defun corfu-move-to-minibuffer ()
    "Transfer current Corfu completion session to the minibuffer."
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
;;;; _
  :init
  ;; Enable Corfu globally (recommended since Dabbrev can be used globally with M-/)
  (global-corfu-mode)
  :config
  ;; Allow corfu-move-to-minibuffer to continue completion
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

;;;; Corfu-Quick
  (use-package corfu-quick
    :ensure nil
;;;;; Keymaps
    :bind (:map corfu-map
                ("M-;" . corfu-quick-insert)))

;;;; Corfu-History
  (use-package corfu-history
    :ensure nil
    :hook (corfu-mode . corfu-history-mode))

;;;; Corfu-Info
  ;; TODO Remove `corfu-info' package as it is sufficient to use `corfu-popupinfo'
  (use-package corfu-info
    :ensure nil
    :disabled t
    :hook (corfu-mode . corfu-info-mode))

;;;; Corfu-PopupInfo
  (use-package corfu-popupinfo
    :ensure nil
    :hook (corfu-mode . corfu-popupinfo-mode)
    :custom
    (corfu-popupinfo-delay 0.5)))

;;; Corfu-Terminal
;; Do it without child-frames
(use-package corfu-terminal
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode t)))

;;; Corfu-Prescient
;; Ranking completions-at-point
(use-package corfu-prescient
  :after (corfu prescient)
  :hook (corfu-mode . corfu-prescient-mode)
  :custom (corfu-prescient-enable-filtering nil "Orderless handles filtering"))

;;; Nerd-Icons-Corfu
(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; Cape
;; Completing things like a superhero
;; NOTE Press `C-' ?' for help
(use-package cape
  :after general
  :hook ((emacs-lisp-mode . lgreen/emacs-lisp-completion-setup)
         (prog-mode . lgreen/prog-mode-completion-setup)
         (text-mode . lgreen/text-mode-completion-setup)
         (markdown-mode . lgreen/text-mode-completion-setup))
;;;; Keymaps
  :bind ("C-'" . cape-prefix-map)
;;;; Custom
  :custom
  ;; cape-dict works best with a frequency-sorted word list (most common words first).
  ;; The default /usr/share/dict/words is alphabetically sorted, which causes grep
  ;; to return uncommon words. Using google-10000-english gives common words first.
  ;; Source: https://github.com/first20hours/google-10000-english
  (cape-dict-file "~/.emacs.d/dictionaries/google-10000-english.txt")
;;;; Functions
  :init
  (defun lgreen/base-completion-setup ()
    "Set up base cape completion backends for prog and text modes."
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))
  (defun lgreen/prog-mode-completion-setup ()
    "Set up cape completion for programming modes."
    (lgreen/base-completion-setup)
    (add-to-list 'completion-at-point-functions #'cape-keyword))
  (defun lgreen/emacs-lisp-completion-setup ()
    "Set up cape completion for Emacs Lisp mode."
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block))
  (defun lgreen/text-mode-completion-setup ()
    "Set up cape completion for text modes."
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-emoji)
                      #'cape-file
                      #'cape-history))))

;;; Yasnippet-Capf
(use-package yasnippet-capf
  :after (yasnippet cape)
  :hook ((prog-mode text-mode) . lgreen/add-yasnippet-capf)
  :preface
  (defun lgreen/add-yasnippet-capf ()
    "Add yasnippet-capf to completion-at-point-functions for the current buffer."
    (interactive)
    (add-hook 'completion-at-point-functions #'yasnippet-capf nil t))
;;;; Keymaps
  :general
  (general-def :keymaps 'cape-prefix-map
    "y" '(yasnippet-capf :which-key "Yasnippet CAPF")))

;;; Kind-Icon
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the background color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

;;; _
(provide 'init-completion)
