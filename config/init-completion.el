;; init-completion.el --- -*- lexical-binding: t; -*-


;;; Dabbrev
;; Short and sweet
(use-package dabbrev
  :ensure nil
;;;; Keymaps
  :bind (
         ("C-/" . dabbrev-expand)
         ("C-;" . dabbrev-completion)))

;;; Company
;; Not a crowd
;; NOTE: Company Disabled: We are using Corfu now
(use-package company
  :disabled t
  :init
  (setq company-idle-delay 0.1  ; Show suggestions after a small delay
        company-minimum-prefix-length 2 ; Start completing after 2 characters
        company-show-numbers t) ; Show numbers for easy selection
  :config
  (global-company-mode t)) ; Enable Company mode globally

;;; Corfu
;; Just completion-at-point UI
(use-package corfu
  :after dabbrev
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-preselect 'first)       ;; Pre-select the prompt
  (corfu-auto-delay 0.2)         ;; Delay auto popup slightly
  (corfu-auto-prefix 3)          ;; Require at least 3 characters before auto popup
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind
;;;; Keymaps
  ("C-;" . completion-at-point)
  (:map corfu-map
        ("C-;" . corfu-next)
        ("C-n" . 'corfu-next)
        ("C-p" . 'corfu-previous)
        ("<tab>" . 'lgreen/corfu-complete-common-prefix))
;;;; _
  :init
  (defun lgreen/corfu-complete-common-prefix ()
    "Complete the longest common prefix of all candidates."
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (prefix (buffer-substring-no-properties start end))
           (common (try-completion prefix corfu--candidates)))
      (if (and (stringp common)
               (not (string= prefix common)))
          (progn
            (delete-region start end)
            (insert common))
        (corfu-next))))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

;;;; Corfu-Quick
  (use-package corfu-quick
    :ensure nil
    :after corfu
;;;;; Keymaps
    :bind (:map corfu-map
                ("M-;" . corfu-quick-insert)))

;;;; Corfu-History
  (use-package corfu-history
    :ensure nil
    :after corfu
    :hook (corfu-mode . corfu-history-mode))

;;;; Corfu-Info
  ;; TODO Remove `corfu-info' package as it is sufficient to use `corfu-popupinfo'
  (use-package corfu-info
    :ensure nil
    :disabled t
    :after corfu
    :hook (corfu-mode . corfu-info-mode))

 ;;;; Corfu-PopupInfo
  (use-package corfu-popupinfo
    :ensure nil
    :hook (corfu-mode . corfu-popupinfo-mode)
    :config
    (setq corfu-popupinfo-delay 0.5)))

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
;; NOTE Press `C-c ; ?' for help
(use-package cape
  :after general
  :hook ((emacs-lisp-mode . lgreen/emacs-lisp-completion-setup)
         (prog-mode . lgreen/prog-mode-completion-setup)
         (text-mode . lgreen/text-mode-completion-setup)
         (markdown-mode . lgreen/text-mode-completion-setup))
  :general
;;;; Keymaps
  (general-def
    "C-c ;" '(:keymap cape-prefix-map :which-key "Completions using [Cape]"))
;;;; _
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
;;;; Functions
  (defun lgreen/prog-mode-completion-setup ()
    "Set up cape completion for programming modes."
    (add-to-list 'completion-at-point-functions #'cape-keyword))
  (defun lgreen/emacs-lisp-completion-setup ()
    "Set up cape completion for Emacs Lisp mode."
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block))
  (defun lgreen/text-mode-completion-setup ()
    "Set up cape completion for text modes."
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-emoji)))

;;; Yasnippet-Capf
(use-package yasnippet-capf
  :after (yasnippet cape)
  :hook ((prog-mode text-mode) . lgreen/add-yasnippet-capf)
  :preface
  (defun lgreen/add-yasnippet-capf ()
    "Add yasnippet-capf to completion-at-point-functions for the current buffer."
    (interactive)
    (add-hook 'completion-at-point-functions #'yasnippet-capf nil t))
  :general
;;;; Keymaps
  (general-def
    :keymaps 'cape-prefix-map
    "y" '(yasnippet-capf :which-key "Yasnippet CAPF")))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;; (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;;; Kind-Icon
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  (svg-lib-icons-dir "~/env/emacs/cache/")
  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  ;; (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

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

;; Local Variables:
;; jinx-local-words: "Dabbrev Orderless capf corfu dabbrev dir elisp emacs etags formatter formatters ispell setq sgml svg tex"
;; End:
