;; init-email.el --- -*- lexical-binding: t; -*-


;;; Mu4e
;; You got mail!
(use-package mu4e
  :ensure nil
  :after org
  :custom
  (mu4e-index-cleanup nil)
  (mu4e-index-lazy-check t)
  (mu4e-maildir "~/.mail")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-change-filenames-when-moving t) ;; needed for mbsync
  (message-kill-buffer-on-exit t) ;; Don't keep message buffers around
  :config
  (setq mu4e-contexts
        (list
         ;; First Account: lambert.green@gmail.com
         (make-mu4e-context
          :name "Primary"
          :enter-func (lambda () (mu4e-message "Switch to Primary context"))
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-maildir-shortcuts . (("/gmail/inbox" . ?i)
                                             ("/gmail/All Mail" . ?a)
                                             ("/gmail/Drafts" . ?d)
                                             ("/gmail/Sent Mail" . ?s)
                                             ("/gmail/Trash" . ?t)))
                  (mu4e-drafts-folder . "/gmail/Drafts")
                  (mu4e-trash-folder . "/gmail/Trash")
                  (mu4e-refile-folder . "/gmail/All Mail")
                  (mu4e-sent-folder . "/gmail/Sent Mail")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-user . "lambert.green@gmail.com")
                  (user-mail-address . "lambert.green@gmail.com")
                  (mu4e-compose-signature . "Lambert Green")
                  (smtpmail-auth-credentials . '("security find-internet-password -a lambert.green@gmail.com -r smtp -s smtp.gmail.com -P 587 -w"))))

         ;; Second Account: lambda.verda@gmail.com
         (make-mu4e-context
          :name "Secondary"
          :enter-func (lambda () (mu4e-message "Switch to Secondary context"))
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail2" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-maildir-shortcuts . (("/gmail2/inbox" . ?I)
                                             ("/gmail2/All Mail" . ?A)
                                             ("/gmail2/Drafts" . ?D)
                                             ("/gmail2/Sent Mail" . ?S)
                                             ("/gmail2/Trash" . ?T)))
                  (mu4e-drafts-folder . "/gmail2/Drafts")
                  (mu4e-trash-folder . "/gmail2/Trash")
                  (mu4e-refile-folder . "/gmail2/All Mail")
                  (mu4e-sent-folder . "/gmail2/Sent Mail")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)
                  (smtpmail-smtp-user . "lambda.verda@gmail.com")
                  (user-mail-address . "lambda.verda@gmail.com")
                  (mu4e-compose-signature . "Lambda Verda")
                  (smtpmail-auth-credentials . '("security find-internet-password -a lambda.verda@gmail.com -r smtp -s smtp.gmail.com -P 587 -w"))))))

  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)

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

;;; NotMuch
;; The answer to how much email do we want
(use-package notmuch
  :ensure nil
  :commands (notmuch)
  :config
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode 0))))

;; _
(provide 'init-email)

;; Local Variables:
;; jinx-local-words: "Verda gmail smtp"
;; End:
