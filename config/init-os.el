;; init-os.el --- -*- lexical-binding: t; -*-

;;; Environment Setup
;; We are products of our environment
(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x pgtk))   ; macOS + Linux GUIs
  :defer 1
  :config
  (setq shell-file-name "/bin/zsh")
  (setq exec-path-from-shell-arguments '("-c" "-l"))
  ;; Add all the necessary environment variables in one loop
  (dolist (var '(
                 "LANG"
                 "LC_CTYPE"
                 "PATH"
                 "HOMEBREW_CASK_OPTS"
                 "HOMEBREW_PREFIX"
                 "HOMEBREW_CELLAR"
                 "HOMEBREW_REPOSITORY"
                 "RIPGREP_CONFIG_PATH"
                 "GPG_AGENT_INFO"
                 "SSH_AGENT_PID"
                 "SSH_AUTH_SOCK"
                 ))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;; Darwin/MacOS
;; macOS specific config
(use-package emacs
  :ensure nil
  :when (memq window-system '(mac ns))
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
  (setq consult-locate-args "mdfind -name ")

;;;; Keybinds
;;;;; Cmd and Option
  ;; Remap Cmd to Super and Option to Meta
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)

;;;;; Copy/Paste
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-z") 'undo)

;;;;; Text size zoom
  (global-set-key (kbd "s--") 'text-scale-decrease)
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-set 0))))

;;; GNU/Linux
;; Linux specific config
;; TODO Add config for Linux that is present in Mac but not here
(use-package emacs
  :ensure nil
  :when (eq system-type 'gnu/linux)
  :config
  (defvar homebrew-prefix)
  (setq homebrew-prefix "/home/linuxbrew/.linuxbrew/"))

;;; Windows-NT
;; Windows specific config
(use-package emacs
  :ensure nil
  :when (eq system-type 'windows-nt)
  :config
  ;; Set find program
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
  :when (memq window-system '(mac ns))
  :config
  ;; Add Homebrew Info to Info path
  (add-to-list 'Info-directory-list (concat homebrew-prefix "share/info/")))

;;; Ns-Auto-Titlebar
;; On macOS make the `titlebar' look better
;; The package essentially handles the below config:
;; (set-frame-parameter nil 'ns-appearance 'dark/light)
;; (set-frame-parameter nil 'ns-transparent-titlebar t)
(use-package ns-auto-titlebar
  :demand t
  :if (eq system-type 'darwin)
  :hook (after-make-frame-functions . (lambda (frame)
                                        (with-selected-frame frame
                                          (when (display-graphic-p)
                                            (ns-auto-titlebar-mode)))))
  :config
  (unless (daemonp)
    (ns-auto-titlebar-mode)))

;;; Osx Trash
;; Use the system trash
(use-package osx-trash
  :if (eq system-type 'darwin)
  :config (osx-trash-setup))

;;; Mouse
;; We need Tom and Jerry
(use-package mouse
  :ensure nil
  :unless window-system  ;; Load only in terminal
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
