;; init-projects.el --- -*- lexical-binding: t; -*-


;;; Project
;; Things need to be done
(use-package project
  :ensure nil
  :preface
  (defvar lgreen/project-compile-command "make"
    "Default compile command for the project.")

  (defvar lgreen/project-test-command "make test"
    "Default test command for the project.")

  (defvar lgreen/dev-directory-base-path "~/dev/"
    "Base path for development directories")

  (defun lgreen/project-try-projectile (dir)
    "Determine if DIR is a Projectile project."
    (let ((projectile-file (expand-file-name ".projectile" dir)))
      (if (file-exists-p projectile-file)
	  (list 'vc 'Git dir)
	nil)))

  (defun lgreen/project-compile ()
    "Compile the current project."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (compile lgreen/project-compile-command)))

  (defun lgreen/project-test ()
    "Test the current project."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (compile lgreen/project-test-command)))

  :custom
  (project-switch-commands 'project-find-file)
  (project-vc-merge-submodules nil)

  :init
  (lgreen/leader-define-key
    "p p" '(project-switch-project :wk "Switch project")
    "p f" '(project-find-file :wk "Find file in project")
    "p c" '(lgreen/project-compile :wk "Compile in project")
    "p d" '(project-dired :wk "Dired in project")
    "p b" '(project-switch-to-buffer :wk "Switch buffer in project")
    "p t" '(lgreen/project-test :wk "Test in project"))

  :config (add-to-list 'project-find-functions 'lgreen/project-try-projectile))

;; _
(provide 'init-projects)
