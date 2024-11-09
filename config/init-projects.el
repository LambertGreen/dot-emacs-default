;; init-projects.el --- -*- lexical-binding: t; -*-

;;; Project
;; Things need to be done
(use-package project
  :ensure nil
  :preface
  (defvar lgreen/dev-directory-base-path "~/dev/"
    "Base path for development directories")

  (defun lgreen/project-open-default-file ()
    "Open any `README` file in project root if available, otherwise prompt."
    (interactive)
    (let* ((project-root (project-root (project-current t)))
           ;; Search for any file starting with 'README' in a case-insensitive manner
           (readme-file (seq-find (lambda (file)
                                    (string-match-p "\\`readme" (downcase file)))
                                  (directory-files project-root))))
      (if readme-file
          (find-file (expand-file-name readme-file project-root))
        (call-interactively #'project-find-file))))

  (defun lgreen/get-justfile-recipes ()
    "Return a list of recipes from the Justfile by calling `just --summary`."
    (let ((default-directory (project-root (project-current t))))
      (condition-case err
          (let* ((output (string-trim-right (shell-command-to-string "just --summary")))
                 (recipes (split-string output " " t)))
            (if recipes
                recipes
              (error "No recipes found in Justfile")))
        ;; Error handling
        (error
         (message "Error fetching recipes: %s" (error-message-string err))
         nil))))

  (defun lgreen/run-justfile-recipe ()
    "Prompt user to select and run a Justfile recipe."
    (interactive)
    (let* ((recipes (lgreen/get-justfile-recipes)))
      (if recipes
          (let ((recipe (completing-read "Select project task: " recipes)))
            (compile (concat "just " recipe)))
        (message "No valid recipes found or an error occurred."))))

  :custom
  (project-switch-commands 'lgreen/project-open-default-file)
  (project-vc-merge-submodules nil)
  (project-vc-extra-root-markers '(".project" ".projectile"))

  :init
  (lgreen/leader-define-key
    "p e" '(lgreen/run-justfile-recipe :wk "execute Just recipe")))

;;; _
(provide 'init-projects)

;; Local Variables:
;; jinx-local-words: "Dired Justfile"
;; End:
