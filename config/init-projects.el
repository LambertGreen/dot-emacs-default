;; init-projects.el --- -*- lexical-binding: t; -*-


;;; Project
;; Things need to be done
(use-package project
  :ensure nil
  :preface

  (defvar lgreen/dev-directory-base-path "~/dev/"
    "Base path for development directories")

  (defun lgreen/project-try-projectile (dir)
    "Determine if DIR is a Projectile project."
    (let ((projectile-file (expand-file-name ".projectile" dir)))
      (if (file-exists-p projectile-file)
          (list 'vc 'Git dir)
        nil)))

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
  (project-switch-commands 'project-find-file)
  (project-vc-merge-submodules nil)

  :init
  (lgreen/leader-define-key
    "p e" '(lgreen/run-justfile-recipe :wk "execute Just recipe"))

  :config (add-to-list 'project-find-functions 'lgreen/project-try-projectile))

;; _
(provide 'init-projects)

;; Local Variables:
;; jinx-local-words: "Dired Justfile"
;; End:
