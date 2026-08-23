;; init-package-manager.el --- -*- lexical-binding: t; -*-

;;; Use Elpaca as our package manager

;;; Short-circuit if user declines package installation
;; Only prompt if elpaca is not already installed
(unless noninteractive
  (let ((elpaca-dir (expand-file-name "elpaca/" user-emacs-directory)))
    (unless (file-exists-p elpaca-dir)
      (unless (y-or-n-p "Elpaca not found. Initialize package manager? (n for vanilla Emacs) ")
        (provide 'init-package-manager)
        (throw 'early-exit nil)))))

;; Suppress Elpaca version assignment warnings
(setq warning-suppress-log-types '((elpaca)))

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Disable GNU/NonGNU ELPA menus — those servers are frequently unreachable
(setq elpaca-menu-functions
      (seq-remove (lambda (f)
                    (memq f '(elpaca-menu-gnu-elpa elpaca-menu-nongnu-elpa)))
                  elpaca-menu-functions))

;;; Install use-package support
(setq use-package-verbose t
      use-package-compute-statistics t
      use-package-always-ensure t
      use-package-always-defer t
      )

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; Recipe overrides — packages from GNU/NonGNU ELPA need explicit repos
;;; since those archive servers are disabled above.
;;; Packages with use-package declarations carry their recipe via :ensure.
;;; These are transitive dependencies with no use-package form:
(elpaca (compat :host github :repo "emacs-compat/compat"))
(elpaca (queue :host github :repo "emacsmirror/queue"))
(elpaca (popon :host codeberg :repo "akib/emacs-popon"))
(elpaca (svg-lib :host github :repo "rougier/svg-lib"))

;; simple-httpd: MELPA recipe points to wrong repo (emacs-web-server instead of emacs-http-server)
(elpaca (simple-httpd :host github :repo "skeeto/emacs-http-server"))

;; Block until current queue processed.
(elpaca-wait)
;; Use-package can be used beyond this point

;;; _
(provide 'init-package-manager)
