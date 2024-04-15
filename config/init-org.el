;;; init-org.el --- -*- lexical-binding: t; -*-


;;; Org
;; Taming the chaos
(use-package org
  :ensure nil
  :custom
  (org-blank-before-new-entry '((heading . true) (plain-list-item . true)))
  :config
  (require 'org-tempo))

;;; Org-Indent
;; Line things up better
(use-package org-indent
  :ensure nil
  :after (org)
  :hook
  (org-mode . org-indent-mode))

;;; Org-Superstar
;; Holywood doing bullet time
(use-package org-superstar
  :after (org)
  :custom
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-hide-leading-stars nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  :hook
  (org-mode . org-superstar-mode))

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
