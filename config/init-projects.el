;;; init-projects.el --- -*- lexical-binding: t; -*-


;; TODO Remove if finally deciding to move from projectile to project.el
;; ;;; Projectile
;; ;; Shooting projects into space
;; (use-package projectile
;;   :custom
;;   ;; We have too many projects to have project discovery done everytime
;;   (projectile-auto-discover nil)
;;   ;; Set search path and depth
;;   (projectile-project-search-path '(("~/dev" . 5)))
;;   (projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS"))
;;   (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
;;   (projectile-ignored-projects
;;    (append
;;     '("~/dev/my/dotfiles/emacs/dot-emacs.default/elpaca/repos/"
;;       )))
;;   ;; NOTE: If a project is already in the cache, then one has to manually
;;   ;; delete the cache file to have the project ignored.
;;   ;; This can be done using the following elisp command: (delete-file projectile-known-projects-file)
;;   (projectile-ignored-project-function
;;    (lambda (project-root)
;;      (or (string-prefix-p "~/dev/work/_bazel/" project-root)
;;	 (string-prefix-p "/usr/lib/node_modules/" project-root))))
;;   :init
;;   (lgreen/leader-define-key
;;     "p p" '(projectile-switch-project :wk "Switch project")
;;     "p f" '(projectile-find-file :wk "Find file in project")
;;     "p d" '(projectile-dired :wk "Dired in project")
;;     "p b" '(projectile-switch-to-buffer :wk "Switch buffer in project"))
;;   :config
;;   (projectile-mode +1))

;;; Project
;; Things need to be done
(use-package project
  :ensure nil
  :preface
  (defun lgreen/project-try-projectile (dir)
    "Determine if DIR is a Projectile project."
    (let ((projectile-file (expand-file-name ".projectile" dir)))
      (if (file-exists-p projectile-file)
	  (cons 'vc dir)
	nil)))
  :custom
  (project-switch-commands 'project-find-file)
  (project-vc-merge-submodules nil)
  :init
  (lgreen/leader-define-key
    "p p" '(project-switch-project :wk "Switch project")
    "p f" '(project-find-file :wk "Find file in project")
    "p c" '(project-compile :wk "Compile in project")
    "p d" '(project-dired :wk "Dired in project")
    "p b" '(project-switch-to-buffer :wk "Switch buffer in project"))
  :config (add-to-list 'project-find-functions 'lgreen/project-try-projectile))

;;; _
(provide 'init-projects)
