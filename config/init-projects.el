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
    "Get a list of recipes from the Justfile by calling `just --summary`."
    (let ((default-directory (project-root (project-current t))))
      (let* ((output (string-trim-right (shell-command-to-string "just --summary")))
             (recipes (split-string output " " t)))
	recipes)))

  (defun lgreen/run-justfile-recipe ()
    "Prompt user to select a Justfile recipe and run it."
    (interactive)
    (let* ((recipes (lgreen/get-justfile-recipes))
           (recipe (completing-read "Select project task: " recipes)))
      (compile (concat "just " recipe))))

  :custom
  (project-switch-commands 'project-find-file)
  (project-vc-merge-submodules nil)

  :init
  (lgreen/leader-define-key
    "p p" '(project-switch-project :wk "Switch project")
    "p f" '(project-find-file :wk "Find file in project")
    "p c" '(project-compile :wk "Compile in project")
    "p d" '(project-dired :wk "Dired in project")
    "p b" '(project-switch-to-buffer :wk "Switch buffer in project")
    "p t" '(lgreen/run-justfile-recipe :wk "Run a Justfile recipe"))

  :config (add-to-list 'project-find-functions 'lgreen/project-try-projectile))

;; _
(provide 'init-projects)
