;; init-os.el --- -*- lexical-binding: t; -*-


;;; Environment Setup
;;
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :defer 5
  :config
  ;; Use bash as the shell for exec-path-from-shell
  (setq shell-file-name "/bin/bash")
  (setq exec-path-from-shell-arguments '("-c" "-l"))
  ;; Add all the necessary environment variables in one loop
  (dolist (var
           '(
             "RIPGREP_CONFIG_PATH"
             "SSH_AGENT_PID"
             "SSH_AUTH_SOCK"
             "GPG_AGENT_INFO"
             ))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;; Darwin/MacOS
;; macOS specific config
(use-package emacs
  :ensure nil
  :if (memq window-system '(mac ns))
  :config
  (setq insert-directory-program "gls"
        dired-use-ls-dired t)
  ;; Set Homebrew prefix
  (defvar homebrew-prefix)
  (if (file-directory-p "/opt/homebrew/")
      (setq homebrew-prefix "/opt/homebrew/")
    (setq homebrew-prefix "/usr/local/"))
  ;; Add Homebrew Emacs site-lisp to load-path
  (let ((default-directory (concat homebrew-prefix "share/emacs/site-lisp")))
    (normal-top-level-add-subdirs-to-load-path))
  ;; Set auth-sources
  (setq auth-sources '(macos-keychain-generic macos-keychain-internet "~/.authinfo.gpg"))
  ;; Use `mdfind' instead of `locate'
  (setq consult-locate-args "mdfind -name "))

;;; GNU/Linux
;; Linux specific config
;; TODO Add config for Linux that is present in Mac but not here
(use-package emacs
  :ensure nil
  :if (eq system-type 'gnu/linux)
  :config
  (defvar homebrew-prefix)
  (setq homebrew-prefix "/home/linuxbrew/.linuxbrew/"))

;;; Windows-NT
;; Windows specific config
(use-package emacs
  :ensure nil
  :if (eq system-type 'windows-nt)
  :config
  ;; Set find program
  ;; TODO: Check if fd can be used since it so much faster.
  (when (eq system-type 'windows-nt)
    (setq find-program (expand-file-name "~/scoop/shims/find.exe")))
  ;; Set exec path
  (when (eq system-type 'windows-nt)
    (setq exec-path (cons "c:/Users/Lambert/scoop/shims/" exec-path)))
  ;; On Windows ignore any f15 key-press since we use Caffeine from time to time
  ;; and it uses the f15 key to keep the machine from falling asleep
  (when (eq system-type 'windows-nt)
    (global-set-key [f15] 'ignore))
  ;; On Windows: open a `cmd.exe' shell
  (when (eq system-type 'windows-nt)
    (defun command-shell ()
      "opens a shell which can run programs as if run from cmd.exe from Windows"
      (interactive)
      (let ((explicit-shell-file-name "cmdproxy")
            (shell-file-name "cmdproxy") (comint-dynamic-complete t))
        (shell)))))

;;; Info
;; Knowledge is acquired
(use-package info
  :ensure nil
  :if (memq window-system '(mac ns))
  :config
  ;; Add Homebrew Info to Info path
  (add-to-list 'Info-directory-list (concat homebrew-prefix "share/info/")))

;;; Ns-Auto-Titlebar
;; On macOS make the titlebar look better
;; The package essentially handles the below config:
;; (set-frame-parameter nil 'ns-appearance 'dark/light)
;; (set-frame-parameter nil 'ns-transparent-titlebar t)
(use-package ns-auto-titlebar
  :demand t
  :when (and (eq system-type 'darwin)
             (or (display-graphic-p) (daemonp)))
  :config (ns-auto-titlebar-mode))

;;; Mouse
;; We need Tom and Jerry
(use-package mouse
  :ensure nil
  :if (not window-system)  ;; Load only in terminal
  :config
  ;; Enable mouse support in the terminal
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (_))  ;; Necessary for mouse tracking
  (setq mouse-sel-mode t))

;;; _
(provide 'init-os)

;; Local Variables:
;; jinx-local-words: "auth authinfo cmd cmdproxy emacs exe fd gls gpg homebrew linuxbrew mdfind ns os titlebar usr"
;; End:
