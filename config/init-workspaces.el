;; init-workspaces.el --- -*- lexical-binding: t; -*-

;;; Tab-bar
;; Put it on my tab
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "t t" '(toggle-frame-tab-bar :wk "Toggle tab bar")
    "TAB" '(:ignore t :wk "Workspaces")
    "TAB n" '(tab-bar-switch-to-next-tab  :wk "Tab next")
    "TAB p" '(tab-bar-switch-to-prev-tab  :wk "Tab previous")))

;;; Tabspaces
;; Make tabs work those spaces
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-include-buffers '("*scratch*"))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "TAB TAB" '(tabspaces-switch-or-create-workspace :wk "Switch workspace")
    "TAB b" '(tabspaces-switch-buffer-and-tab :wk "Switch to buffer")
    "TAB d" '(tabspaces-close-workspace :wk "Close workspace")
    "TAB o" '(tabspaces-open-or-create-project-and-workspace :wk "Open workspace")

    ;; Override `switch-project' keybinding
    "p p" '(tabspaces-open-or-create-project-and-workspace :wk "Switch project"))
;;;; -
  :config
  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
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

;; Local Variables:
;; jinx-local-words: "workspaces"
;; End:
