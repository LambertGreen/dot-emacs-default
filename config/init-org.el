;;; init-org.el --- -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :custom
  (org-blank-before-new-entry '((heading . true) (plain-list-item . true)))
  :config
  (require 'org-tempo))

(use-package org-indent
  :ensure nil
  :after (org)
  :hook
  (org-mode . org-indent-mode))

(use-package org-superstar
  :ensure t
  :after (org)
  :custom
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-hide-leading-stars nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  :hook
  (org-mode . org-superstar-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
	 (evil-org-mode . evil-org-set-key-theme))
  :config
  (evil-define-key 'normal evil-org-mode-map
    (kbd "M-k") #'org-metaup
    (kbd "M-j") #'org-metadown))

;;; _
(provide 'init-org)
