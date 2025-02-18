;; init-spellchecking.el --- -*- lexical-binding: t; -*-

;;; Jinx
;; Misspelled it first!
;;
;; This package has an external dependency i.e. the `enchant' library
;; - install it using your package manager
;; Use cases:
;; - Common mistyped and misspelled words: hte (the), noone (none/noon)
(use-package jinx
  :unless (eq system-type 'windows-nt)
  :hook ((emacs-startup . global-jinx-mode)
         ((c++-mode c++-ts-mode) . lgreen/jinx-setup-for-c++-modes)
         (hack-local-variables . lgreen/override-jinx-local-words))

;;;; Keymaps
  :bind
  ([remap ispell-word] . 'jinx-correct)
  ("C-," . 'jinx-correct)
  :init
  (lgreen/leader-define-key
    "t s" '(global-jinx-mode :wk "toggle spellcheck")
    "x s" '(:ignore t :wk "Spelling")
    "x s s" '(jinx-correct :wk "correct")
    "x s a" '(jinx-correct-all :wk "correct all")
    "x s n" '(jinx-next :wk "correct-next")
    "x s p" '(jinx-previous :wk "correct-previous"))
  :config
;;;; Functions
  (defun lgreen/jinx-skip-c++-includes-for-system-headers (start)
    "Skip checking #include of system libraries."
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (looking-at "^#include <.*>$")))

  (defun lgreen/jinx-setup-for-c++-modes ()
    "Set up Jinx predicates and `camelCase' handling for C++ modes."
    (add-to-list 'jinx--predicates #'lgreen/jinx-skip-c++-includes-for-system-headers)
    (add-to-list 'jinx-camel-modes 'c++-mode)
    (add-to-list 'jinx-camel-modes 'c++-ts-mode))

  (defun lgreen/extract-and-remove-jinx-local-words ()
    "Extract `jinx-local-words` from the Local Variables section and remove it from the file.
If `jinx-local-words` is the only variable, remove the entire section.
Returns the extracted words as a plain string or nil if not found."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (rx bol ";; Local Variables:\n"
                 (group (zero-or-more (seq ";; " (one-or-more anything) "\n"))) ;; Group 1: before-vars
                 (seq ";; jinx-local-words: \"" (group (zero-or-more (not ?\"))) "\"\n") ;; Group 2: jinx-local-words
                 (group (zero-or-more (seq ";; " (one-or-more anything) "\n"))) ;; Group 3: after-vars
                 ";; End:")
             nil t)
        (let* ((before-vars (or (match-string 1) ""))
               (word-string (match-string 2))  ;; Extracted words (can be nil)
               (after-vars (or (match-string 3) "")))
          (when word-string  ;; Only proceed if jinx-local-words was found
            (setq word-string (substring-no-properties word-string))  ;; Strip metadata
            (if (and (string-empty-p before-vars) (string-empty-p after-vars))
                ;; Case 1: jinx-local-words is the only variable → Remove the full section
                (delete-region (match-beginning 0) (point))
              ;; Case 2: Other variables exist → Remove only `jinx-local-words` line
              (goto-char (match-beginning 2))
              (delete-region (line-beginning-position) (1+ (line-end-position)))))
          word-string)))) ;; Return clean text

  (defun lgreen/process-jinx-local-words ()
    "Extract `jinx-local-words`, add them to `.project-words`, and remove them from the file."
    (when-let* ((project-root (project-root (project-current)))
                (project-words-file (expand-file-name ".project-words" project-root))
                (word-string (lgreen/extract-and-remove-jinx-local-words)))  ;; Extract & modify file
      (let* ((words (split-string word-string " " t))  ;; Convert to list of words
             (existing-words (if (file-exists-p project-words-file)
                                 (with-temp-buffer
                                   (insert-file-contents project-words-file)
                                   (split-string (buffer-string) "\n" t))
                               nil))
             (all-words (delete-dups (sort (append existing-words words) #'string<)))) ;; Sort & dedupe
        ;; Write sorted, unique words to .project-words
        (with-temp-file project-words-file
          (insert (string-join all-words "\n") "\n"))
        (message "Added words to %s: %s" project-words-file (string-join words ", ")))))

  (defun lgreen/process-jinx-local-words-in-project ()
    "Process `jinx-local-words` for all files in the current project."
    (interactive)
    (when-let ((project-root (project-root (project-current))))
      (let* ((files (project-files (project-current)))
             (updated-files 0))  ;; Ensure it's inside the let* block
        (dolist (file files)
          (when (file-readable-p file)  ;; Ensure the file is accessible
            (with-temp-buffer
              (insert-file-contents file)
              (when (re-search-forward ";; jinx-local-words: " nil t)  ;; Only process relevant files
                (set-buffer-file-coding-system 'utf-8)  ;; Ensure encoding remains consistent
                (lgreen/process-jinx-local-words)  ;; Modify the buffer
                (goto-char (point-max))
                (skip-chars-backward " \t\n")  ;; Trim any extra blank lines
                (delete-region (point) (point-max))  ;; Remove them
                (write-region (point-min) (point-max) file)  ;; Save changes
                (setq updated-files (1+ updated-files)))))))
      (message "Processed `jinx-local-words` in %d files." updated-files)))

  (defvar lgreen-project-jinx-words nil
    "Holds project-wide words for `jinx-local-words`.")

  (defun lgreen/load-project-jinx-words ()
    "Load words from `.project-words` into `lgreen-project-jinx-words`."
    (when-let* ((project-root (project-root (project-current)))
                (project-words-file (expand-file-name ".project-words" project-root)))
      (when (file-exists-p project-words-file)
        (with-temp-buffer
          (insert-file-contents project-words-file)
          (setq lgreen-project-jinx-words
                (string-join (split-string (buffer-string) "\n" t) " ")))))) ;; Store as a global string

  (defun lgreen/override-jinx-local-words ()
    "Ensure `jinx-local-words` always includes project-wide words, overriding file-local values."
    (when (boundp 'jinx-local-words)
      (let ((file-local-words jinx-local-words)  ;; Capture file-local words
            (project-words (when (boundp 'lgreen-project-jinx-words)
                             lgreen-project-jinx-words))) ;; Use a project-wide variable
        (setq-local jinx-local-words
                    (if file-local-words
                        (concat file-local-words " " project-words) ;; Merge both
                      project-words))))))

;;; _
(provide 'init-spellchecking)
