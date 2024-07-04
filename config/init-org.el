;;; init-org.el --- -*- lexical-binding: t; -*-

;;; Org
;; My life in plain text.
(use-package org
  :ensure nil
  :custom
  (org-modules '(org-capture org-habit org-tempo))
  (org-return-follows-link t)
  (org-startup-folded 'show3levels)
  (org-ellipsis "⤵")
  (org-hide-emphasis-markers t)
  (org-blank-before-new-entry '((heading . t)
				(plain-list-item . auto)))
  (org-directory "~/dev/my/org")
  (org-todo-keywords
   '((sequence
      "TODO(t!)"  ; A task that needs doing & is ready to do
      "PROJ(p!)"  ; A project, which usually contains other tasks
      "LOOP(r!)"  ; A recurring task
      "STRT(s!)"  ; A task that is in progress
      "WAIT(w@/!)"  ; Something external is holding up this task
      "HOLD(h@/!)"  ; This task is paused/on hold because of me
      "IDEA(i)"  ; An unconfirmed and unapproved task or notion
      "|"
      "DONE(d!)"  ; Task successfully completed
      "KILL(k@/!)") ; Task was canceled, aborted or is no longer applicable
     (sequence
      "[ ](T)"   ; A task that needs doing
      "[-](S)"   ; Task is in progress
      "[?](W)"   ; Task is being held up or paused
      "|"
      "[X](D)")  ; Task was completed
     (sequence
      "|"
      "OKAY(o)"
      "YES(y)"
      "NO(n)")))
  ;; Log DONE with timestamp
  (org-log-done 'time)
  ;; Log state changes into drawer
  (org-log-into-drawer t)

  :init
  (general-def
    :keymaps 'org-mode-map
    [remap consult-imenu] 'consult-org-heading
    "C-," nil
    "C-'" nil
    )

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
    "b i r" 'org-table-insert-row

    "l" '(:ignore t :wk "link")
    "l i" 'org-id-store-link
    "l l" 'org-insert-link
    "l L" 'org-insert-all-links
    "l n" 'org-next-link
    "l p" 'org-previous-link
    "l s" 'org-store-link
    "l S" 'org-insert-last-stored-link
    "l t" 'org-toggle-link-display

    "p" '(:ignore t :wk "priority")
    "p d" 'org-priority-down
    "p p" 'org-priority
    "p u" 'org-priority-up

    "P" '(:ignore t :wk "publish")
    "P a" 'org-publish-all
    "P f" 'org-publish-current-file
    "P p" 'org-publish
    "P P" 'org-publish-current-project
    "P s" 'org-publish-sitemap

    "r" '(:ignore t :wk "refile")
    "r r" 'org-refile
    "r R" 'org-refile-reverse

    "s" '(:ignore t :wk "subtree")
    "s a" 'org-toggle-archive-tag
    "s b" 'org-tree-to-indirect-buffer
    "s c" 'org-clone-subtree-with-time-shift
    "s d" 'org-cut-subtree
    "s h" 'org-promote-subtree
    "s j" 'org-move-subtree-down
    "s k" 'org-move-subtree-up
    "s l" 'org-demote-subtree
    "s n" 'org-narrow-to-subtree
    "s r" 'org-refile
    "s s" 'org-sparse-tree
    "s A" 'org-archive-subtree
    "s N" 'widen
    "s S" 'org-sort)

  (defun lgreen/setup-org-calendar-navigation ()
    (general-def
      :keymaps 'org-read-date-minibuffer-local-map
      :states '(normal insert)
      "C-k" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
      "C-j" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
      "C-l" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
      "C-h" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))))
  :hook ((org-mode . visual-line-mode)
	 (org-mode . lgreen/setup-org-calendar-navigation))
  :config
  (org-load-modules-maybe t)
  (require 'ob-C)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (C . t)
     (dot . t)
     (gnuplot . t)
     (org . t)
     (js . t)
     (sqlite . t)
     (shell . t)
     ))

  (defun lgreen/org-font-setup ()
    "Sets the fonts to specific sizes for org-mode"
    (interactive)
    ;; TODO Is this needed?
    (require 'org-faces)

    ;; Make the document title a bit bigger
    (set-face-attribute 'org-document-title nil :font "Iosevka Nerd Font Propo" :weight 'bold :height 1.5)
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.3)
                    (org-level-2 . 1.20)
                    (org-level-3 . 1.17)
                    (org-level-4 . 1.15)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Iosevka Nerd Font Propo" :weight 'medium :height (cdr face)))

    ;; Make sure org-indent face is available
    (require 'org-indent)

    ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

  (defun lgreen/org-prettify-symbols ()
    "Beautify org mode keywords."
    (interactive)
    (setq prettify-symbols-alist '(("TODO" . "")
	                           ("STRT" . "")
	                           ("WAIT" . "")
				   ("KILL" . "")
				   ("DONE" . "")
				   ("PROJ" . " ")
				   ("[#A]" . "")
				   ("[#B]" . "")
				   ("[#C]" . "")
				   ("[ ]" . "")
				   ("[X]" . "")
				   ("[-]" . "")
				   ("#+BEGIN_SRC" . "")
				   ("#+END_SRC" . "")
				   ("#+BEGIN_QUOTE" . "")
				   ("#+END_QUOTE" . "")
				   (":PROPERTIES:" . "")
				   (":END:" . "―")
				   ("#+STARTUP:" . "")
				   ("#+TITLE: " . "⋮")
				   ("#+RESULTS:" . "")
				   ("#+NAME:" . "")
				   ("#+ROAM_TAGS:" . "")
				   ("#+FILETAGS:" . "")
				   ("#+HTML_HEAD:" . "")
				   ("#+SUBTITLE:" . "")
				   ("#+AUTHOR:" . "")
				   ("#+begin_src" . "")
				   ("#+end_src" . "")
				   ("#+begin_quote" . "")
				   ("#+end_quote" . "")
				   (":properties:" . "")
				   (":end:" . "―")
				   ("#+startup:" . "")
				   ("#+title: " . "⋮")
				   ("#+results:" . "")
				   ("#+name:" . "")
				   ("#+roam_tags:" . "")
				   ("#+filetags:" . "")
				   ("#+html_head:" . "")
				   ("#+subtitle:" . "")
				   ("#+author:" . "")
				   (":Effort:" . "")
				   ("SCHEDULED:" . "")
				   ("DEADLINE:" . "")))
    (prettify-symbols-mode))

  ;; Apply the function after loading Org mode
  (add-hook 'org-mode-hook 'lgreen/org-font-setup)

  ;; Below code snippet acquired from here:
  ;; - https://stackoverflow.com/questions/10969617/hiding-markup-elements-in-org-mode
  (defun lgreen/org-toggle-emphasis ()
    "Toggle hiding/showing of org emphasize markers."
    (interactive)
    (if org-hide-emphasis-markers
	(set-variable 'org-hide-emphasis-markers nil)
      (set-variable 'org-hide-emphasis-markers t))
    (org-mode-restart))

  (advice-add 'load-theme
	      :after 'lgreen/org-font-setup))

;;; Org-Contrib
(use-package org-contrib
  :config
  (require 'org-collector)
  (require 'org-protocol)
  ;; (require 'org-man)
  )

;;; Evil-Org
;; Taming the chaos with HKJL
(use-package evil-org
  :after (evil org)
  :hook ((org-mode . evil-org-mode)
	 (evil-org-mode . evil-org-set-key-theme))
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift leader todo heading)))

;;; Org-Agenda
;; I love it when a plan comes together
(use-package org-agenda
  :ensure nil
  :after org
  :custom
  (org-agenda-log-mode-items '(closed clock state))
  :init
  (lgreen/leader-define-key
    "o a" '(org-agenda :wk "Open agenda"))
  :config
  (setq org-agenda-files
	(append (directory-files-recursively (expand-file-name "personal" org-directory) "\\.org$")
		(directory-files-recursively (expand-file-name "work" org-directory) "\\.org$")))
  )

;;; Evil-Org-Agenda
;; Making evil plans
;; NOTE The `SPC' key gets overridden. I tried without sufficient success
;; to bring back `SPC' as leader key, but it never worked 100% consistently
;; and so I finally decided to rather keep a simpler config, and just not
;; have a leader key while the agenda window is active.
(use-package evil-org-agenda
  :ensure nil
  :after (evil-org org-agenda)
  ;; :hook (org-agenda-mode . lgreen/org-agenda-mode-setup)
  :config
  (evil-org-agenda-set-keys)
  ;; (defun lgreen/org-agenda-mode-setup ()
  ;;   ;; Ensure keys are defined correctly for default and motion states.
  ;;   (general-def
  ;;     :keymaps 'org-agenda-mode-map
  ;;     "SPC" nil
  ;;     "<tab>" 'org-agenda-show-and-scroll-up
  ;;     "RET" 'org-agenda-goto)
  ;;   (general-def
  ;;     :keymaps 'org-agenda-mode-map
  ;;     :states 'motion
  ;;     "SPC" nil
  ;;     "<tab>" 'org-agenda-show-and-scroll-up
  ;;     "RET" 'org-agenda-goto))
  )

;;; Org-Capture
;; Gotta capture them all
(use-package org-capture
  :ensure nil
  :after org
  :init
  (lgreen/leader-define-key
    "X" '(org-capture :wk "Org capture"))
  :config
  (setq lgreen/org-capture-todo-file (expand-file-name "personal/todo.org" org-directory)
	lgreen/org-capture-notes-file (expand-file-name "personal/notes.org" org-directory)
	lgreen/org-capture-journal-file (expand-file-name "personal/journal.org" org-directory)
	lgreen/org-capture-contacts-file (expand-file-name "personal/contacts.org" org-directory)
	org-capture-templates
	'(("t" "todo" entry
	   (file+headline lgreen/org-capture-todo-file "Inbox")
	   "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:FROM: %a\n:END:\n" :prepend t)
	  ("n" "notes" entry
	   (file+headline lgreen/org-capture-notes-file "Inbox")
	   "* %u %?\n:PROPERTIES:\n:CREATED: %U\n:FROM: %a\n:END:\n" :prepend t)
	  ("m" "email" entry
	   (file+olp lgreen/org-capture-todo-file "Inbox")
	   "* TODO Mail:%u %?\n%i\n%a" :prepend t)
	  ("j" "Journal" entry
	   (file+olp+datetree lgreen/org-capture-journal-file)
	   "* %U %?\n%i\n%a" :prepend t)
	  ("p" "Protocol" entry
	   (file+headline lgreen/org-capture-notes-file "Inbox")
	   "* %? [[%:link][%:description]] \nCaptured On: %U\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	  ("L" "Protocol Link" entry
	   (file+headline lgreen/org-capture-notes-file "Inbox")
	   "* %? [[%:link][%:description]] \nCaptured On: %U")))
  )

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
;; Draw pretty Unicode tables
(use-package org-pretty-table
  :ensure (:fetcher github :repo "Fuco1/org-pretty-table")
  :after org
  :hook (org-mode . org-pretty-table-mode)
  )

;;; Org-Contacts
;; Keeping connections
;; TODO I am disabling to see if this package is causing performance issues
(use-package org-contacts
  :disabled t
  :after org
  :custom (org-contacts-files (list (expand-file-name "personal/contacts.org" org-directory)
				    (expand-file-name "personal/contacts_imported_from_gmail.org" org-directory))))

;;; Org-Checklist
;; Auto-reset of checkbox state for repeating items
;; FIXME This package is not found by Elpaca
;; (use-package org-checklist
;;   :after org)

;;; Org-Appear
;; Seeing is believing
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;;; Org-Roam
;; Knowledge management system... did you not know?
(use-package org-roam
  :after org
  :custom (org-roam-directory "~/dev/my/org/roam")
  :init
  (lgreen/local-leader-define-key
    :states 'normal
    :keymaps 'org-mode-map
    "m" '(:ignore t :wk "roam")
    "m i" 'org-roam-node-insert
    "m f" 'org-roam-node-find
    "m m" 'org-roam-buffer-toggle

    "m d" '(:ignore t :wk "dailies")
    "m d d" 'org-roam-dailies-goto-date
    "m d t" 'org-roam-dailies-goto-today
    "m d y" 'org-roam-dailies-goto-yesterday

    "m o" '(:ignore t :wk "node properties")
    "m o a" 'org-roam-alias-add
    "m o A" 'org-roam-alias-remove
    "m o r" 'org-roam-ref-add
    "m o R" 'org-roam-ref-remove
    "m o t" 'org-roam-tag-add
    "m o T" 'org-roam-tag-remove
    ))

;;; Org-Roam-Ui
;; Seeing is believing
(use-package org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode)
  :config (org-roam-db-autosync-mode))

;;; Consult-Org-Roam
;; TODO Evaluate this package
;; Added this package primarily because when I tried adding
;; an org-roam note, I was not getting any completion candidates in
;; the minibuffer.  I do get them just fine in Doom Emacs. I should
;; debug and better learn what is going on rather than just relying
;; on this package.
(use-package consult-org-roam
  :after org-roam
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1))

;;; Ob-Http
(use-package ob-http
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((http . t))))

;;; Ob-Mermaid
(use-package ob-mermaid
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t))))

;;; _
(provide 'init-org)
