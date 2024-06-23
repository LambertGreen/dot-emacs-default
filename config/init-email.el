;;; init-email.el --- -*- lexical-binding: t; -*-

;;; Mu4e
;; You got mail!
(use-package mu4e
  :ensure nil
  :after org
  :custom
  (mu4e-index-cleanup nil)
  (mu4e-ind
   ex-lazy-check t)
  )

;;; _
(provide 'init-email)
