;; init-org.el --- -*- lexical-binding: t; -*-

;;; Org
;; My life in plain text.
(use-package org
  :ensure t
  :after general
  :mode ("\\.org\\'" . org-mode)
  :commands (org-capture org-agenda)
  :custom
  (org-modules
   '(org-checklist
     org-habit
     org-tempo))
  (org-return-follows-link t)
  (org-startup-folded 'show3levels)
  ;; NOTE: For org-ellipsis there are characters that look good but don't work consistently and so we
  ;; have to choose something that does not look as good but at least still works.
  ;; Chars that look good but don't work: ⤵, ⤸, ⤶, ⤷
  ;; Chars that do work: ►, ⯆, ↴, ↷, …, ⋱
  (org-ellipsis " ⬎")
  (org-hide-emphasis-markers t)
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . auto)))
  ;; Config for `org-id'
  ;; NOTE: I tried to various configs to try minimize the creation of IDs for headings as
  ;; I was wanting to rely mainly `org-roam' file level IDs, but ran into issues, and so
  ;; settled with embracing the IDs for reliable and consistent linking.
  ;; NOTE: (UPDATE: 01/09/26): I am confused by the above comment as now I am indeed relying
  ;; Org-Roam for atomic note creation with IDs, and while I am indeed using IDs for headings
  ;; in org files where I want Roam to process them, I am fine creating those IDs manually
  ;; as I don't want IDs for every heading.  So we now disable the below config.
  ;; (org-id-link-to-org-use-id t)
  ;; NOTE: The help doc indicates that using this is best for single instance use of Emacs
  ;; (org-id-track-globally t)

  (org-directory "~/dev/my/org")
  (org-todo-keywords
   '((sequence
      "TODO(t!)"     ; A task that needs doing & is ready to do
      "STRT(s!)"     ; A task that is in progress
      "WAIT(w@/!)"   ; Something external is holding up this task
      "HOLD(h@/!)"   ; This task is paused/on hold because of me
      "|"
      "DONE(d!)"     ; Task successfully completed
      "KILL(k@/!)")  ; Task was canceled, aborted or is no longer applicable

     ;; Separate sequence for non-actionable states
     (type
      "PROJ(P!)"     ; A project, which usually contains other tasks
      "IDEA(i)")     ; An unconfirmed and unapproved task or notion

     ;; Task states for checkboxes
     (sequence
      "[ ](T!)"       ; A task that needs doing
      "[-](S!)"       ; Task is in progress
      "[?](W)"        ; Task is being held up or paused
      "|"
      "[K](K!)"       ; Task was canceled
      "[X](D!)")      ; Task was completed

     ;; States for test/build runs
     (sequence
      "PEND(b)"
      "RUNG(r)"
      "|"
      "PASS(p)"
      "FAIL(f)"
      "CANC(c)")

     ;; Habit tracking sequence
     (sequence
      "LOOP(l!)"
      "|"
      "DONE(d!)"
      "KILL(k@/!)")))

  ;; Log DONE with timestamp
  (org-log-done 'time)
  ;; Log state changes into drawer
  (org-log-into-drawer t)
  (org-export-backends '(ascii html icalendar latex odt md pandoc))
  (org-archive-location "%s_archive::datetree/")
  (org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4)))
  (org-reverse-note-order t)
  (org-tag-alist '((:startgroup)
                   ("📖 read" . ?r)
                   ("🎬 watch" . ?w)
                   ("🎧 listen" . ?l)
                   ("💻 code" . ?c)
                   ("📚 study" . ?s)
                   ("📝 write" . ?t)
                   (:endgroup)
                   ("💼 work" . ?k)
                   ("🏠 personal" . ?p)
                   ("💪 health" . ?h)
                   ("🙏 spirit" . ?s)
                   ("🎉 fun" . ?f)
                   ("🧪 science" . ?i)
                   ("👥 social" . ?o)
                   ("🌴 vacation" . ?v)))
  :hook ((org-mode . visual-line-mode)
         (org-mode . lgreen/setup-org-calendar-navigation)
         (org-mode . lgreen/org-font-setup)
         (org-mode . lgreen/org-prettify-symbols)
         (before-save . lgreen/update-org-todo-statistics)
         (org-cycle . lgreen/update-todo-cookies-on-visibility-change))
  :config
;;;; Keymaps
  (general-def
    :states '(normal)
    :keymaps 'org-mode-map
    [remap consult-imenu] 'consult-org-heading
    "C-," nil
    "C-'" nil
    ;; FIXME Pressing Enter on links is not working
    ;; "RET" '(:ignore lgreen/org-enter-key)

;;;;; Outline
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
;;;;; _
  (lgreen/local-leader-define-key
    :keymaps 'org-mode-map
;;;;; Todo
    "t" '(org-todo :wk "Todo")

;;;;; Clock
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

;;;;; Deadline/Date
    "d" '(:ignore t :wk "Deadline/Date")
    "d d" 'org-deadline
    "d s" 'org-schedule
    "d t" 'org-time-stamp
    "d T" 'org-time-stamp-inactive

;;;;; Insert
    "i" '(:ignore t :wk "Insert")
    "i h" '(lgreen/org-insert-heading :wk "Heading")
    "i s" '(lgreen/org-insert-subheading :wk "Subheading")

;;;;; Tables
    "b" '(:ignore t :wk "Tables")
    "b d" '(:ignore t :wk "delete")
    "b d c" 'org-table-delete-column
    "b d r" 'org-table-kill-row
    "b i" '(:ignore t :wk "insert")
    "b i c" 'org-table-insert-column
    "b i h" 'org-table-insert-hline
    "b i r" 'org-table-insert-row

;;;;; Link
    "l" '(:ignore t :wk "Link")
    "l i" 'org-id-store-link
    "l l" 'org-insert-link
    "l L" 'org-insert-all-links
    "l n" 'org-next-link
    "l p" 'org-previous-link
    "l s" 'org-store-link
    "l S" 'org-insert-last-stored-link
    "l t" 'org-toggle-link-display

;;;;; Priority
    "p" '(:ignore t :wk "Priority")
    "p d" 'org-priority-down
    "p p" 'org-priority
    "p u" 'org-priority-up

;;;;; Publish
    "P" '(:ignore t :wk "Publish")
    "P a" 'org-publish-all
    "P f" 'org-publish-current-file
    "P p" 'org-publish
    "P P" 'org-publish-current-project
    "P s" 'org-publish-sitemap

;;;;; Refile
    "r" '(:ignore t :wk "Refile")
    "r r" 'org-refile
    "r R" 'org-refile-reverse

;;;;; Subtree
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

;;;; Functions
;;;;; Used by keybinds
  (defun lgreen/org-insert-heading ()
    "Insert a new Org heading and enter insert mode."
    (interactive)
    (org-insert-heading)
    (when (evil-normal-state-p)
      (evil-append 1)))
  (defun lgreen/org-insert-subheading (&optional arg)
    "Insert a subheading and enter insert mode immediately if in normal state.
    Passes ARG to `org-insert-subheading`."
    (interactive "P")  ;; allows handling prefix arguments like C-u for ARG
    (org-insert-subheading arg)
    (when (evil-normal-state-p)
      (evil-append 1)))
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

;;;;; Visuals
  (defun lgreen/org-font-setup ()
    "Sets the fonts to specific sizes for org-mode"
    (interactive)
    (if (not (display-graphic-p))
        (message "lgreen/org-font-setup: Skipping because we are in terminal mode.")

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
      (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)))
  ;; NOTE: The symbols unfortunately don't leave any breathing room before the next character is printed.
  ;; E.g. This is a `todo' item: "Item
  ;; TODO Update the symbols to use the "more breathing room" approach.
  ;; TODO Only enable these fancy symbols in GUI Emacs
  ;; The below webpage shows how to address the "not enough breathing room" issue:
  ;; - https://endlessparentheses.com/using-prettify-symbols-in-clojure-and-elisp-without-breaking-indentation.html
  (defun lgreen/org-prettify-symbols ()
    "Beautify org mode keywords."
    (interactive)
    (setq prettify-symbols-alist
          '(("TODO" . (? (Br . Bl) ?\s))
            ("STRT" . (? (Br . Bl) ?\s))
            ("WAIT" . (? (Br . Bl) ?\s))
            ("KILL" . (? (Br . Bl) ?\s))
            ("DONE" . (? (Br . Bl) ?\s))
            ("PROJ" . (? (Br . Bl) ?\s))
            ("LOOP" . ?↺)
            ("PEND" . (? (Br . Bl) ?\s))
            ("RUNG" . (? (Br . Bl) ?\s))
            ("FAIL" . (? (Br . Bl) ?\s))
            ("PASS" . (? (Br . Bl) ?\s))
            ("CANC" . (? (Br . Bl) ?\s))
            ("IDEA" . ?💡)
            ("[#A]" . "")
            ("[#B]" . "")
            ("[#C]" . "")
            ("[ ]" . (? (Br . Bl) ?\s))
            ("[X]" . (? (Br . Bl) ?\s))
            ("[-]" . ?◒)
            ("[K]" . (? (Br . Bl) ?\s))
            ("#+BEGIN_SRC" . (? (Br . Bl) ?\s))
            ("#+END_SRC" . (? (Br . Bl) ?\s))
            ("#+BEGIN_QUOTE" . (? (Br . Bl) ?\s))
            ("#+END_QUOTE" . (? (Br . Bl) ?\s))
            (":PROPERTIES:" . (? (Br . Bl) ?\s))
            (":END:" . "―")
            (":LOGBOOK:" . (?📝(Br . Bl) ?\s))
            ("#+STARTUP:" . "")
            ("#+TITLE: " . "⋮")
            ("#+CATEGORY:" . (? (Br . Bl) ?\s))
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
            ("#+category:" . (? (Br . Bl) ?\s))
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
  ;; Below code snippet acquired from here:
  ;; - https://stackoverflow.com/questions/10969617/hiding-markup-elements-in-org-mode
  (defun lgreen/org-toggle-emphasis ()
    "Toggle hiding/showing of org emphasize markers."
    (interactive)
    (if org-hide-emphasis-markers
        (set-variable 'org-hide-emphasis-markers nil)
      (set-variable 'org-hide-emphasis-markers t))
    (org-mode-restart))

;;;;; Todos
  (defun lgreen/update-org-todo-statistics (&rest _)
    "Update all `TODO' statistics cookies in the current Org buffer."
    (when (derived-mode-p 'org-mode)
      (org-update-all-dblocks) ;; Update any dynamic blocks if present
      (org-map-entries (lambda () (org-update-statistics-cookies nil)))))

  (defun lgreen/update-todo-cookies-on-visibility-change (&rest _)
    "Update all `TODO' statistics cookies when visibility changes."
    (when (derived-mode-p 'org-mode)
      (org-map-entries (lambda () (org-update-statistics-cookies nil)))))

  (defun lgreen/update-all-todo-cookies ()
    "Manually update all `TODO' statistics cookies in the current Org buffer."
    (interactive)
    (org-map-entries (lambda () (org-update-statistics-cookies nil))))

;;;; Modules
  (eval-after-load 'org
    '(org-load-modules-maybe t))

;;;; Advice
  (advice-add 'load-theme
              :after (lambda (&rest _)
                       (lgreen/org-font-setup))))

;;; Org-Contrib
;; NOTE: We use `org-checklist' which is added as a `org-module'
(use-package org-contrib
  :after org)

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
  ;; TODO Try get the org pretty symbols to load one time in org-agenda
  :hook  (org-agenda-mode . lgreen/org-prettify-symbols)
  :commands org-agenda
  :custom
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-window-setup 'only-window)
  (org-agenda-tags-column -150)
  (org-agenda-start-with-log-mode nil)
  (org-agenda-skip-scheduled-if-done nil)
  (org-agenda-skip-deadline-if-done nil)
  (org-agenda-skip-timestamp-if-done nil)
  (org-agenda-category-icon-alist
   `(("Inbox"          ,(list (propertize "📥")))
     ("Sprint"         ,(list (propertize "📈")))
     ("Project"        ,(list (propertize "🗂️")))
     ("Tracking"       ,(list (propertize "📊")))
     ("Maintenance"    ,(list (propertize "🛠️")))
     ("Training"       ,(list (propertize "📚")))
     ("Idea"           ,(list (propertize "💡")))
     ("Comms"          ,(list (propertize "💬")))
     ("Career"         ,(list (propertize "🌱")))
     ("Feedback"       ,(list (propertize "📝")))
     ("Archive"        ,(list (propertize "📦")))
     ("DeskEnv"        ,(list (propertize "🖥️")))
     ("Sci&Phi"        ,(list (propertize "🧪")))
     ("Fun&Games"      ,(list (propertize "🎮")))
     ("Vacation"       ,(list (propertize "🌴")))
     ("Misc."          ,(list (propertize "📎")))
     ("Spirit"         ,(list (propertize "🙏")))
     ("Health"         ,(list (propertize "💪")))
     ("House"          ,(list (propertize "🏠")))
     ("Social"         ,(list (propertize "👥")))
     ("Charity"        ,(list (propertize "🤲")))
     ("Politics"       ,(list (propertize "🇺🇸")))
     ("SoftDev"        ,(list (propertize "💻")))
     ("IDE"            ,(list (propertize "🔧")))
     ("Plan"           ,(list (propertize "📝")))
     ("Event"          ,(list (propertize "📅")))))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "o a" '(org-agenda :wk "Open agenda"))
  :config
;;;; Functions
  (defun lgreen/add-icons-to-tags-agenda ()
    "Display icons for certain tags in the Org Agenda view."
    (font-lock-add-keywords
     nil
     '(("\\(:work:\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "💼") nil)))
       ("\\(:personal:\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "🏠") nil)))
       ("\\(:read:\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "📖") nil)))
       ("\\(:watch:\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "🎬") nil)))
       ("\\(:listen:\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "🎧") nil)))
       ("\\(:code:\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "💻") nil)))
       ("\\(:study:\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "📚") nil)))
       ("\\(:write:\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "📝") nil))))))

  ;; Activate the font-lock customization in the agenda buffer
  (add-hook 'org-agenda-finalize-hook 'lgreen/add-icons-to-tags-agenda)

  ;; TODO See if we can include the roam directory
  ;; (setq org-agenda-files
  ;;       (append (directory-files-recursively (expand-file-name "personal" org-directory) "\\.org$")
  ;;               (directory-files-recursively (expand-file-name "work" org-directory) "\\.org$")
  ;;               (directory-files-recursively (expand-file-name "roam" org-directory) "\\.org$")))
  (setq org-agenda-files
        (append (directory-files-recursively (expand-file-name "agenda" org-directory) "\\.org$"))))

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
  :hook (org-agenda-mode . lgreen/setup-org-agenda-keys)
  :config
  (defun lgreen/setup-org-agenda-keys ()
    (evil-org-agenda-set-keys)

    (evil-define-key 'motion org-agenda-mode-map (kbd "SPC") nil)
    (evil-define-key 'motion evil-org-agenda-mode-map (kbd "SPC") nil)

;;;; Keymaps
    ;; Local leader bindings for Org Agenda
    (lgreen/local-leader-define-key
      :keymaps 'org-agenda-mode-map
      :states '(normal motion)

;;;;; View
      "v" '(org-agenda-view-mode-dispatch :wk "View")

;;;;; Todo
      "t" '(org-agenda-todo :wk "Todo")

;;;;; Clock
      "c" '(:ignore t :wk "Clock")
      "c i" '(org-agenda-clock-in :wk "clock in")
      "c o" '(org-agenda-clock-out :wk "clock out")
      "c g" '(org-agenda-clock-goto :wk "goto clocked task")

;;;;; Deadline/Date
      "d" '(:ignore t :wk "Deadline/Date")
      "d d" 'org-agenda-deadline
      "d s" 'org-agenda-schedule

;;;;; Miscellaneous
      "e" '(org-agenda-set-effort :wk "Set effort")
      "r" '(org-agenda-redo :wk "Refresh agenda"))
    ))

;;; Org-Habit
(use-package org-habit
  :ensure nil
  :after org-agenda
  :custom
  (org-habit-graph-column 100)
  (org-habit-show-done-always-green t)
  (org-habit-show-habits-only-for-today t)
  :config
  (setq org-agenda-custom-commands
        '(("m" "Missed Habits"
           agenda ""
           ((org-agenda-overriding-header "Missed Habits")
            (org-agenda-entry-types '(:habit)))))))

;;; Org-Capture
;; Gotta capture them all
(use-package org-capture
  :ensure nil
  :after org
  :commands org-capture
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "X" '(org-capture :wk "org capture"))
;;;; _
  :config
  (setq lgreen/org-capture-todo-file (expand-file-name "agenda/todo.org" org-directory)
        lgreen/org-capture-events-file (expand-file-name "agenda/events.org" org-directory)
        lgreen/org-capture-journal-file (expand-file-name "personal/journal.org" org-directory)
        lgreen/org-capture-contacts-file (expand-file-name "personal/contacts.org" org-directory)
        lgreen/org-capture-health-file (expand-file-name "personal/health.org" org-directory)
        org-capture-templates
        '(("t" "todo" entry
           (file+headline lgreen/org-capture-todo-file "Inbox 📥")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:FROM: %a\n:END:\n" :prepend t)
          ("m" "meeting" entry
           (file+headline lgreen/org-capture-events-file "Meetings 📅")
           "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:FROM: %a\n:END:\n" :prepend t)
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
;; FIXME I am disabling to see if this package is causing performance issues
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
  (org-roam-db-autosync-mode t)
  (org-roam-directory "~/dev/my/org")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("w" "Work Item")
     ("ws" "Story" plain "\n\n* Links\n+ [[%^{URL}][GUS Link]]\n"
      :target (file+head "work/gus/stories/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :work:gus:story: \n\n")
      :unnarrowed t)

     ("wb" "Bug" plain "\n\n* Links\n+ [[%^{URL}][GUS Link]]\n"
      :target (file+head "work/gus/bugs/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :work:gus:bug: \n\n")
      :unnarrowed t)

     ("p" "Personal Item")
     ("pe" "Entertainment Item")
     ("pem" "Movie" plain "%?"
      :target (file+head "entertainment/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :entertainment:movie:\n\n")
      :unnarrowed t)

     ("pet" "TV-Show" plain "%?"
      :target (file+head "entertainment/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :entertainment:tv-show:\n\n")
      :unnarrowed t)

     ("pea" "Actor" plain "%?"
      :target (file+head "entertainment/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :entertainment:cast_crew:actor:\n\n")
      :unnarrowed t)

     ("ped" "Director" plain "%?"
      :target (file+head "entertainment/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :entertainment:cast_crew:director:\n\n")
      :unnarrowed t)
     ))

  (org-roam-dailies-capture-templates
   '(("d" "daily" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d, %A>\n"))))
  :init
;;;; Keymaps
  (lgreen/local-leader-define-key
    :keymaps 'org-mode-map
    "m" '(:ignore t :wk "Roam")
    "m i" 'org-roam-node-insert
    "m f" 'org-roam-node-find
    "m m" 'org-roam-buffer-toggle

;;;;; Dailies
    "m d" '(:ignore t :wk "Dailies")
    "m d d" 'org-roam-dailies-goto-date
    "m d t" 'org-roam-dailies-goto-today
    "m d y" 'org-roam-dailies-goto-yesterday

;;;;; Node Properties
    "m o" '(:ignore t :wk "Node Properties")
    "m o a" 'org-roam-alias-add
    "m o A" 'org-roam-alias-remove
    "m o r" 'org-roam-ref-add
    "m o R" 'org-roam-ref-remove
    "m o t" 'org-roam-tag-add
    "m o T" 'org-roam-tag-remove
    )

  :config
  ;; FIXME: Put the Entertainment and any other project specific Elisp in a project
  ;; specific location, rather than in your global Emacs config
  (defun get-movies-node-id ()
    "Retrieve the ID of the 'Movies' Org-Roam node."
    (org-roam-node-id (org-roam-node-from-title-or-alias "Movies")))

  (defun add-movie-index-backlink ()
    "Add an 'Index' section with a backlink to the 'Movies' node in the current buffer."
    (when (and (boundp 'org-capture-current-plist)
               (string= "pm" (plist-get org-capture-current-plist :key)))
      (save-excursion
        (goto-char (point-max))
        (insert (format "\n* Index\n[[id:%s][Movies]]\n" (get-movies-node-id)))
        (save-buffer)))))

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

;;; Org Babel
(use-package org-babel
  :ensure nil
  :after org
  :commands org-babel-do-load-languages
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (C . t)
                              (java . t)
                              (dot . t)
                              (gnuplot . t)
                              (org . t)
                              (js . t)
                              (sql . t)
                              (sqlite . t)
                              (shell . t)
                              (java . t)
                              (plantuml . t)))
  :init
  (add-to-list 'org-src-lang-modes '("json" . json-ts))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;;;; Ob-Json
(use-package ob-json
  :ensure (:fetcher github :repo "sgpthomas/ob-json")
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(json . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;;;; Ob-Http
(use-package ob-http
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(http . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;;;; Ob-Mermaid
(use-package ob-mermaid
  :after org
  :init
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))


;;; Org-QL
;; A query language for your Org files
(use-package org-ql)

;;; Org-Pretty-Tags
(use-package org-pretty-tags
  :hook (org-mode . org-pretty-tags-global-mode)
  :custom
  (org-pretty-tags-surrogate-strings
   `(
     ("work" . "💼")
     ("personal" . "🏠")
     ("read" . "📖")
     ("watch" . "🎬")
     ("listen" . "🎧")
     ("code" . "💻")
     ("study" . "📚")
     ("write" . "✍️")
     ("daily" . "📅")
     ("meeting" . "📋")
     ("team" . "👥")
     ("spirit" . "🙏")
     ("maintenance" . "🛠️")
     )))


;;; _
(provide 'init-org)

;; Local Variables:
;; jinx-local-words: "Elpaca Iosevka Propo defun filetags gmail goto html keymaps lgreen minibuffer nCaptured src subtree"
;; End:
