;;; init-org.el --- -*- lexical-binding: t; -*-


;;; Org
;; Taming the chaos
(use-package org
  :ensure nil
  :custom
  (org-return-follows-link t)
  (org-blank-before-new-entry '((heading . true) (plain-list-item . true)))
  :init
  (lgreen/local-leader-define-key
    :states 'normal
    :keymaps 'org-mode-map
    "t" '(org-todo :wk "todo")

    "c" '(:ignore t :wk "clock")
    "c c" 'org-clock-cancel
    "c d" '(:ignore t :wk "clock-display")
    "c d d" 'org-clock-display
    "c d r" 'org-clock-remove-overlays
    "c i" 'org-clock-in
    "c l" 'org-clock-in-last
    "c o" 'org-clock-out
    "c g" 'org-clock-goto
    "c r" 'org-clock-report

    "d" '(:ignore t :wk "deadline/date")
    "d d" 'org-deadline
    "d s" 'org-schedule
    "d t" 'org-time-stamp
    "d T" 'org-time-stamp-inactive

    "b" '(:ignore t :wk "tables")
    "b d" '(:ignore t :wk "delete")
    "b d c" 'org-table-delete-column
    "b d r" 'org-table-kill-row
    "b i" '(:ignore t :wk "insert")
    "b i c" 'org-table-insert-column
    "b i h" 'org-table-insert-hline
    "b i r" 'org-table-insert-row)

  (defun lgreen/setup-org-calendar-navigation ()
    (general-def
     :keymaps 'org-read-date-minibuffer-local-map
     :states '(normal insert)
     "C-k" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
     "C-j" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
     "C-l" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
     "C-h" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))))
  :hook (org-mode . lgreen/setup-org-calendar-navigation)
  :config
  (require 'org-tempo))

;;; Org-Indent
;; Line things up better
(use-package org-indent
  :ensure nil
  :after org
  :hook (org-mode . org-indent-mode))

;;; Org-Superstar
;; Hollywood doing bullet time
(use-package org-superstar
  :after org
  :custom
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-hide-leading-stars nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  :hook (org-mode . org-superstar-mode))

;;; Org-Pretty-Table
;; Draw pretty unicode tables
(use-package org-pretty-table
  :ensure (:fetcher github :repo "Fuco1/org-pretty-table")
  :after org
  :hook (org-mode . org-pretty-table-mode)
  )

;;; Evil-Org
;; Taming the chaos with HKJL
(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
	 (evil-org-mode . evil-org-set-key-theme))
  :config
  (evil-define-key 'normal evil-org-mode-map
    (kbd "M-k") #'org-metaup
    (kbd "M-j") #'org-metadown))

;;; _
(provide 'init-org)
