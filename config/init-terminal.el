;;; init-terminal.el --- -*- lexical-binding: t; -*-

;; ;;; Vterm
;; ;; TODO: Consider having `eat' supersede `vterm', especially given the compilation issue below
;; ;; FIXME: Failing to compile on latest update
;; (use-package vterm
;;   :disabled t
;;   :custom (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))

;; ;;; VTerm-toggle
;; (use-package vterm-toggle
;;   :after (general vterm)
;;   :custom
;;   (vterm-toggle-fullscreen-p nil)
;;   (vterm-toggle-scope 'project)
;;   :init
;;   (lgreen/leader-define-key
;;     "o t" '(vterm-toggle :wk "Open vterm"))
;;   )

;;; EAT
;; Emulate A Terminal
;; NOTE: On first use you may need to run `eat-compile-terminfo'
(use-package eat
  :after project
  :bind ([remap project-shell] . eat-project)
  :init
  (lgreen/leader-define-key
    "o t" '(eat :wk "Open term")
    "p s" '(eat-project :wk "Shell in project"))
  )

;;; _
(provide 'init-terminal)
