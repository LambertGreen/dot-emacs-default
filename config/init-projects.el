;;; init-projects.el --- -*- lexical-binding: t; -*-


;;; Projectile
;; Shooting projects into space
(use-package projectile
  :custom
  ;; We have too many projects to have project discovery done everytime
  (projectile-auto-discover nil)

  ;; Set search path and depth
  (projectile-project-search-path '(("~/dev" . 5)))
  (projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-ignored-projects
   (append
    '("~/dev/my/dotfiles/emacs/dot-emacs.default/elpaca/repos/"
      )))

  ;; NOTE: If a project is already in the cache, then one has to manually
  ;; delete the cache file to have the project ignored.
  ;; This can be done using the following elisp command: (delete-file projectile-known-projects-file)
  (projectile-ignored-project-function
   (lambda (project-root)
     (or (string-prefix-p "~/dev/work/_bazel/" project-root)
	 (string-prefix-p "/usr/lib/node_modules/" project-root))))

  :config
  (projectile-mode +1))

;;; _
(provide 'init-projects)
