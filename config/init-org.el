;; init-org.el --- -*- lexical-binding: t; -*-


;;; Org
;; My life in plain text.
(use-package org
  :ensure t
  :after general
  :mode ("\\.org\\'" . org-mode)
  :commands (org-capture org-agenda)
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
  (org-export-backends '(ascii html icalendar latex odt md pandoc))
  (org-archive-location "%s_archive::datetree/")
  :hook ((org-mode . visual-line-mode)
         (org-mode . lgreen/setup-org-calendar-navigation)
         (org-mode . lgreen/org-font-setup)
         (org-mode . lgreen/org-prettify-symbols))
  :config
  (general-def
    :states '(normal)
    :keymaps 'org-mode-map
    [remap consult-imenu] 'consult-org-heading
    "C-," nil
    "C-'" nil
    ;; FIXME Pressing Enter on links is not working
    ;; "RET" '(:ignore lgreen/org-enter-key)

    :prefix "SPC z"
    :prefix-command 'org-outline
    :prefix-map 'org-visibility-map
    "a" '(org-fold-show-all :wk "show all")
    "m" '(org-overview :wk "fold all")
    "c" '(org-fold-hide-subtree :wk "hide subtree")
    "o" '(org-fold-show-subtree :wk "show subtree")
    "t" '(org-cycle :wk "toggle subtree")
    "r" '(org-reveal :wk "reveal subtree")
    "s" '(org-show-todo-tree :wk "show TODO tree")
    "d" '(org-toggle-link-display :wk "toggle links")
    "l" '(org-toggle-latex-fragment :wk "toggle LaTeX"))

  (lgreen/local-leader-define-key
    :states 'normal
    :keymaps 'org-mode-map

    "t" '(org-todo :wk "Todo")

    "c" '(:ignore t :wk "Clock")
    "c c" 'org-clock-cancel
    "c d" '(:ignore t :wk "clock-display")
    "c d d" 'org-clock-display
    "c d r" 'org-clock-remove-overlays
    "c i" 'org-clock-in
    "c l" 'org-clock-in-last
    "c o" 'org-clock-out
    "c g" 'org-clock-goto
    "c r" 'org-clock-report

    "d" '(:ignore t :wk "Deadline/Date")
    "d d" 'org-deadline
    "d s" 'org-schedule
    "d t" 'org-time-stamp
    "d T" 'org-time-stamp-inactive

    "b" '(:ignore t :wk "Tables")
    "b d" '(:ignore t :wk "delete")
    "b d c" 'org-table-delete-column
    "b d r" 'org-table-kill-row
    "b i" '(:ignore t :wk "insert")
    "b i c" 'org-table-insert-column
    "b i h" 'org-table-insert-hline
    "b i r" 'org-table-insert-row

    "l" '(:ignore t :wk "Link")
    "l i" 'org-id-store-link
    "l l" 'org-insert-link
    "l L" 'org-insert-all-links
    "l n" 'org-next-link
    "l p" 'org-previous-link
    "l s" 'org-store-link
    "l S" 'org-insert-last-stored-link
    "l t" 'org-toggle-link-display

    "p" '(:ignore t :wk "Priority")
    "p d" 'org-priority-down
    "p p" 'org-priority
    "p u" 'org-priority-up

    "P" '(:ignore t :wk "Publish")
    "P a" 'org-publish-all
    "P f" 'org-publish-current-file
    "P p" 'org-publish
    "P P" 'org-publish-current-project
    "P s" 'org-publish-sitemap

    "r" '(:ignore t :wk "Refile")
    "r r" 'org-refile
    "r R" 'org-refile-reverse

    "s" '(:ignore t :wk "Subtree")
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

  (defun lgreen/org-enter-key ()
    "Context-aware `RET` key for Org mode in Normal state."
    (interactive)
    (cond
     ;; Open links or other elements at point
     ((org-in-regexp org-link-any-re) (org-open-at-point))
     ;; Cycle visibility on a heading
     ((org-at-heading-p) (org-cycle))
     ;; Execute custom behaviors or fallback
     (t (org-cycle))))

  (defun lgreen/setup-org-calendar-navigation ()
    (general-def
      :keymaps 'org-read-date-minibuffer-local-map
      :states '(normal insert)
      "C-k" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
      "C-j" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
      "C-l" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
      "C-h" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))))

  (defun lgreen/org-font-setup ()
    "Sets the fonts to specific sizes for org-mode"
    (interactive)
    ;; TODO Is this needed?
    (require 'org-faces)

    ;; Make the document title a bit bigger
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.3)
                    (org-level-2 . 1.20)
                    (org-level-3 . 1.17)
                    (org-level-4 . 1.15)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))

    ;; Make sure org-indent face is available
    (require 'org-indent)

    ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
    (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

  ;; NOTE: The symbols unfortunately don't leave any breathing room before the next character is printed.
  ;; E.g. "This is a todo item.
  ;; TODO Update the symbols to use the "more breathing room" approach.
  ;; TODO Only enable these fancy symbols in GUI Emacs
  ;; The below webpage shows how to address the "not enough breathing room" issue:
  ;; - https://endlessparentheses.com/using-prettify-symbols-in-clojure-and-elisp-without-breaking-indentation.html
  (defun lgreen/org-prettify-symbols ()
    "Beautify org mode keywords."
    (interactive)
    (setq prettify-symbols-alist '(("TODO" . (? (Br . Bl) ?\s))
                                   ("STRT" . (? (Br . Bl) ?\s))
                                   ("WAIT" . (? (Br . Bl) ?\s))
                                   ("KILL" . (? (Br . Bl) ?\s))
                                   ("DONE" . (? (Br . Bl) ?\s))
                                   ("PROJ" . (? (Br . Bl) ?\s))
                                   ("[#A]" . "")
                                   ("[#B]" . "")
                                   ("[#C]" . "")
                                   ("[ ]" . (? (Br . Bl) ?\s))
                                   ("[X]" . (? (Br . Bl) ?\s))
                                   ("[-]" . (? (Br . Bl) ?\s))
                                   ("#+BEGIN_SRC" . (? (Br . Bl) ?\s))
                                   ("#+END_SRC" . (? (Br . Bl) ?\s))
                                   ("#+BEGIN_QUOTE" . (? (Br . Bl) ?\s))
                                   ("#+END_QUOTE" . (? (Br . Bl) ?\s))
                                   (":PROPERTIES:" . (? (Br . Bl) ?\s))
                                   (":END:" . "―")
                                   (":LOGBOOK:" . (?📝(Br . Bl) ?\s))
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

  (eval-after-load 'org
    '(org-load-modules-maybe t))

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

;;; Org-Habit
(use-package org-habit
  :ensure nil
  :after org
  :custom
  (org-habit-graph-column 60))

;;; Org-Contrib
(use-package org-contrib
  :after org
  :config
  (require 'org-collector)
  (require 'org-protocol)
  ;; TODO Is org-checklist causing perf issues?
  ;; (require 'org-checklist)
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
  :after (general org)
  :commands org-agenda
  :custom
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-window-setup 'only-window)
  :init
  (lgreen/leader-define-key
    "o a" '(org-agenda :wk "Open agenda"))
  :config
  ;; TODO See if we can include the roam directory
  ;; (setq org-agenda-files
  ;;       (append (directory-files-recursively (expand-file-name "personal" org-directory) "\\.org$")
  ;;               (directory-files-recursively (expand-file-name "work" org-directory) "\\.org$")
  ;;               (directory-files-recursively (expand-file-name "roam" org-directory) "\\.org$")))
  (setq org-agenda-files
        (append (directory-files-recursively (expand-file-name "personal/agenda" org-directory) "\\.org$")
                (directory-files-recursively (expand-file-name "work/agenda" org-directory) "\\.org$")))
  )

;;; Evil-Org-Agenda
;; Making evil plans
;;
;; NOTE The `SPC' key gets overridden. I tried without sufficient success
;; to bring back `SPC' as leader key, but it never worked 100% consistently
;; and so I finally decided to rather keep a simpler config, and just not
;; have a leader key while the agenda window is active.
(use-package evil-org-agenda
  :ensure nil
  :after evil-org
  :hook (org-agenda-mode . evil-org-agenda-set-keys)
  )

;;; Org-Capture
;; Gotta capture them all
(use-package org-capture
  :ensure nil
  :after org
  :commands org-capture
  :init
  (lgreen/leader-define-key
    "X" '(org-capture :wk "org capture"))
  :config
  (setq lgreen/org-capture-todo-file (expand-file-name "personal/todo.org" org-directory)
        lgreen/org-capture-notes-file (expand-file-name "personal/notes.org" org-directory)
        lgreen/org-capture-journal-file (expand-file-name "personal/journal.org" org-directory)
        lgreen/org-capture-contacts-file (expand-file-name "personal/contacts.org" org-directory)
        lgreen/org-capture-health-file (expand-file-name "personal/health.org" org-directory)
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
           "* %? [[%:link][%:description]] \nCaptured On: %U")
          ("b" "Blood Pressure Reading" entry
           (file+olp+datetree lgreen/org-capture-health-file "Blood Pressure" "Readings")
           "* BP Reading\n:PROPERTIES:\n:Captured: %U\n:Systolic: %^{Systolic}\n\
:Diastolic: %^{Diastolic}\n:Pulse_Rate: %^{Pulse Rate}\n:END:\n"
           :tree-type month
           :empty-lines 1))))

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
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-hide-leading-stars nil)
  (org-indent-mode-turns-on-hiding-stars nil))

;;; Org-Pretty-Table
;; Draw pretty Unicode tables
(use-package org-pretty-table
  :ensure (:fetcher github :repo "Fuco1/org-pretty-table")
  :after org
  :hook (org-mode . org-pretty-table-mode))

;;; Org-Contacts
;; Keeping connections
;; TODO I am disabling to see if this package is causing performance issues
(use-package org-contacts
  :disabled t
  :after org
  :custom (org-contacts-files (list (expand-file-name "personal/contacts.org" org-directory)
                                    (expand-file-name "personal/contacts_imported_from_gmail.org" org-directory))))

;;; Org-Appear
;; Seeing is believing
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;;; Org-Roam
;; Knowledge management system... did you not know?
(use-package org-roam
  :after org
  :commands (org-roam-node-insert
             org-roam-node-find org-roam-buffer-toggle
             org-roam-dailies-goto-date org-roam-dailies-goto-today
             org-roam-dailies-goto-yesterday org-roam-alias-add
             org-roam-alias-remove org-roam-ref-add org-roam-ref-remove
             org-roam-tag-add org-roam-tag-remove)
  :custom
  ;; TODO is the causing perf issues?
  (org-roam-db-autosync-mode nil)
  (org-roam-directory "~/dev/my/org/roam")
  (org-roam-dailies-capture-templates
   '(("d" "daily" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d, %A>\n"))))
  :init
  (lgreen/local-leader-define-key
    :states 'normal
    :keymaps 'org-mode-map
    "m" '(:ignore t :wk "Roam")
    "m i" 'org-roam-node-insert
    "m f" 'org-roam-node-find
    "m m" 'org-roam-buffer-toggle
    "m d" '(:ignore t :wk "Dailies")
    "m d d" 'org-roam-dailies-goto-date
    "m d t" 'org-roam-dailies-goto-today
    "m d y" 'org-roam-dailies-goto-yesterday
    "m o" '(:ignore t :wk "Node Properties")
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
  :commands (org-roam-ui-mode))

;;; Consult-Org-Roam
;; TODO Evaluate this package
;; Added this package primarily because when I tried adding
;; an org-roam note, I was not getting any completion candidates in
;; the minibuffer.  I do get them just fine in Doom Emacs. I should
;; debug and better learn what is going on rather than just relying
;; on this package.
(use-package consult-org-roam
  :after org-roam
  :commands (consult-org-roam-mode)
  :config
  (consult-org-roam-mode 1))

;;; Org Babel languages
;;;; Ob-Http
(use-package ob-http
  :after org
  :commands (org-babel-do-load-languages)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((http . t))))

;;;; Ob-Mermaid
(use-package ob-mermaid
  :after org
  :commands (org-babel-do-load-languages)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t))))

;; _
(provide 'init-org)

;; Local Variables:
;; jinx-local-words: "Elpaca Iosevka Propo defun filetags gmail goto html keymaps lgreen minibuffer nCaptured src subtree"
;; End:
