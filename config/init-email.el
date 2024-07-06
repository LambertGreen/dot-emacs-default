;;; init-email.el --- -*- lexical-binding: t; -*-

;;; Mu4e
;; You got mail!
(use-package mu4e
  :ensure nil
  :after org
  :custom
  (mu4e-index-cleanup nil)
  (mu4e-index-lazy-check t)
  (mu4e-maildir "~/.mail/gmail")
  (mu4e-maildir-shortcuts
   '((:maildir "/inbox"         :key ?i)
     (:maildir "/All Mail"      :key ?a)
     (:maildir "/Drafts"        :key ?d)
     (:maildir "/Sent Mail"     :key ?s)
     (:maildir "/Trash"         :key ?t)))
  (mu4e-drafts-folder "/Drafts")
  (mu4e-trash-folder "/Trash")
  (mu4e-refile-folder "/All Mail")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-change-filenames-when-moving t) ;; needed for mbsync

  ;; Don't keep message buffers around
  (message-kill-buffer-on-exit t)

  :init
  (lgreen/leader-define-key
    "o m" '(mu4e :wk "Open mu4e")))

;;; Mu4e-Alert
(use-package mu4e-alert
  :after mu4e
  :hook
  (after-init . mu4e-alert-enable-notifications)
  (after-init . mu4e-alert-enable-mode-line-display)
  :config
  (mu4e-alert-set-default-style 'libnotify)
  )

;;; Smtpmail
;; Get it out there
(use-package smtpmail
  :ensure nil
  :custom
  (send-mail-function 'smtpmail-send-it)
  (message-send-mail-function 'smtpmail-send-it))

;;; _
(provide 'init-email)
