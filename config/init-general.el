;;; init-general.el --- -*- lexical-binding: t; -*-

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer lgreen/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :non-normal-prefix "M-SPC") ;; access leader in insert mode

  (lgreen/leader-keys
    ":" '(execute-extended-command :wk "M-x")
    "SPC" '(projectile-find-file :wk "Find file in project")
    "." '(find-file :wk "Find file"))

  ;; Find files and things
  (lgreen/leader-keys
    "f" '(:ignore t :wk "file")
    "f f" '(find-file :wk "Find file")
    "f r" '(recentf :wk "Recent files")
    "f t" '(consult-todo :wk "Find todos")
    "f T" '(consult-todo-all :wk "Find all todos")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.default/README.org")) :wk "Edit emacs config"))

  ;; Search in files
  (lgreen/leader-keys
    "s" '(:ignore t :wk "Search")
    "s b" '(consult-line :wk "Search buffer")
    "s p" '(consult-ripgrep :wk "Search project files")
    "s i" '(consult-imenu :wk "Jump to symbol")
    "s d" '(consult-locate :wk "Search current directory"))

  ;; Projects
  (lgreen/leader-keys
    "p" '(:ignore t :wk "project")
    "p p" '(projectile-switch-project :wk "Switch project")
    "p f" '(projectile-find-file :wk "Find file in project")
    "p d" '(projectile-dired :wk "Dired in project")
    "p b" '(projectile-switch-to-buffer :wk "Switch buffer in project")
    "p s" '(persp-switch :wk "Switch perspective")
    )

  ;; Diff
  (lgreen/leader-keys
    "d" '(:ignore t :wk "Diff")
    "d p" '(diff-hl-previous-hunk :wk "Diff previous")
    "d n" '(diff-hl-next-hunk :wk "Diff next"))

  ;; Git Interface
  (lgreen/leader-keys
    "g" '(:ignore t :wk "git")
    "g g" '(magit-status :wk "Status"))

  ;; Buffer Management
  (lgreen/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b d" '(kill-this-buffer :wk "Delete buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b N" '(evil-buffer-new :wk "New buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(save-buffer :wk "Save buffer")
    "b S" '(evil-write-all :wk "Save all buffers"))

  ;; Window Management
  (lgreen/leader-keys
    "w" '(:ignore t :wk "window")
    "w =" '(balance-windows :wk "Balance windows")
    "w h" '(evil-window-left :wk "Window left")
    "w k" '(evil-window-up :wk "Window up")
    "w j" '(evil-window-down :wk "Window down")
    "w l" '(evil-window-right :wk "Window right")
    "w q" '(evil-quit :wk "Window close")
    "w d" '(delete-window :wk "Delete window")
    "w s" '(split-window-below :wk "Split window below")
    "w v" '(split-window-right :wk "Split window right")
    "w o" '(delete-other-windows :wk "Delete other windows")
    "w f" '(toggle-frame-fullscreen :wk "Toggle fullscreen")
    "w m" '(toggle-frame-maximized :wk "Toggle maximized")
    "w w" '(evil-window-next :wk "Toggle maximized")
    "w H" '(evil-window-move-far-left :wk "Move window far left")
    "w K" '(evil-window-move-very-top :wk "Move window very top")
    "w J" '(evil-window-move-very-bottom :wk "Move window very bottom")
    "w L" '(evil-window-move-far-right :wk "Move window far right"))

  ;; Help
  (lgreen/leader-keys
    "h" '(:ignore t :wk "help")
    "h f" '(helpful-callable :wk "Describe function")
    "h v" '(helpful-variable :wk "Describe variable")
    "h k" '(helpful-key :wk "Describe key")
    "h c" '(helpful-command :wk "Describe command")
    "h m" '(describe-mode :wk "Describe mode")
    "h t" '(consult-theme :wk "Switch theme"))

  ;; Open
  (lgreen/leader-keys
    "o" '(:ignore t :wk "open")
    "o p" '(treemacs :wk "Open project tree")
    "o t" '(vterm-toggle :wk "Open vterm"))

  ;; Toggles
  (lgreen/leader-keys
    "t" '(:ignore t :wk "toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t i" '(indent-guide-global-mode :wk "Toggle indent guides")
    "t w" '(visual-line-mode :wk "Toggle truncated lines"))

  ;; Quit
  (lgreen/leader-keys
    "q" '(:ignore t :wk "quit")
    "q q" '(save-buffers-kill-terminal :wk "Quit")))

;;; _
(provide 'init-general)
