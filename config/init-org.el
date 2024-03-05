;;; init-org.el --- -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :config
  (require 'org-tempo))

(use-package org-indent
  :ensure nil
  :after (org)
  :config
  (add-hook 'org-mode-hook #'(lambda () (org-indent-mode t))))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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
