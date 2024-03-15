;;; init-general.el --- -*- lexical-binding: t; -*-


;;; General
;; Command the map
(use-package general
  :demand t
  :config
  (general-evil-setup)

;;;; Leader key definer
  ;; set up 'SPC' as the global leader key
  (general-create-definer lgreen/leader-keys
    :states '(normal insert visual emacs treemacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :non-normal-prefix "M-SPC") ;; access leader in insert mode

;;;; Local-leader key definer
  ;; Define a local leader for all modes
  (general-create-definer lgreen/local-leader-keys
    :states 'normal
    :prefix "SPC m")

;;;; Top Level Keys
  (lgreen/leader-keys
    ":" '(execute-extended-command :wk "M-x")
    "SPC" '(projectile-find-file :wk "Find file in project")
    ";" '(eval-expression :wk "Eval expression")
    "." '(find-file :wk "Find file"))

;;;; Local Leader
  (lgreen/leader-keys
    "m" '(:ignore t :wk "localleader"))

;;;; Find files and things
  (lgreen/leader-keys
    "f" '(:ignore t :wk "file")
    "f f" '(find-file :wk "Find file")
    "f r" '(recentf :wk "Recent files")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.default/README.org")) :wk "Edit emacs config"))

;;;; Search in files
  (lgreen/leader-keys
    "s" '(:ignore t :wk "search"))

;;;; Projects
  (lgreen/leader-keys
    "p" '(:ignore t :wk "project"))

;;;; Diff
  (lgreen/leader-keys
    "d" '(:ignore t :wk "Diff")
    "d p" '(diff-hl-previous-hunk :wk "Diff previous")
    "d n" '(diff-hl-next-hunk :wk "Diff next"))

;;;; Git Interface
  (lgreen/leader-keys
    "g" '(:ignore t :wk "git")
    "g g" '(magit-status :wk "Status"))

;;;; Buffer Management
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

;;;; Window Management
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

;;;; Help
  (lgreen/leader-keys
    "h" '(:ignore t :wk "help")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h m" '(describe-mode :wk "Describe mode"))

;;;; Open
  (lgreen/leader-keys
    "o" '(:ignore t :wk "open"))

;;;; Toggles
  (lgreen/leader-keys
    "t" '(:ignore t :wk "toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t i" '(indent-guide-global-mode :wk "Toggle indent guides")
    "t w" '(visual-line-mode :wk "Toggle truncated lines"))

;;;; Quit
  (lgreen/leader-keys
    "q" '(:ignore t :wk "quit")
    "q q" '(save-buffers-kill-terminal :wk "Quit")))


(elpaca-wait)
;; use-package declarations beyond this point may use the `:general' use-package keyword.

;;; _
(provide 'init-general)
