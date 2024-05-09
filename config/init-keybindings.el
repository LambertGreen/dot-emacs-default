;;; init-keybindings.el --- -*- lexical-binding: t; -*-


;;; Use General for convenient keybindings
;; Command the map
(use-package general
  :demand t
  :init
  (defvar lgreen/general-leader-key "SPC"
    "Leader key for Evil")
  (defvar lgreen/general-leader-alt-key "M-SPC"
    "Leader key for Emacs and Evil Insert states")
  (defvar lgreen/general-local-leader-key "SPC m"
    "Local leader key for major-mode specific commands")
  (defvar lgreen/general-local-leader-alt-key "M-SPC m"
    "Local leader key for major-mode specific commands for Emacs and Evil Insert states.")
  :config
  (general-evil-setup)

  ;; TODO Is this needed or taken care of by evil-collection?
  ;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;;; Leader key definer
  ;; set up 'SPC' as the global leader key
  (general-create-definer lgreen/leader-define-key
    :states '(normal insert visual emacs treemacs)
    :keymaps 'override
    :prefix lgreen/general-leader-key
    :non-normal-prefix lgreen/general-leader-alt-key)

;;;; Local-leader key definer
  ;; Define a local leader for all modes
  (general-create-definer lgreen/local-leader-define-key
    :states '(normal insert)
    :prefix lgreen/general-local-leader-key
    :non-normal-prefix lgreen/general-local-leader-alt-key)

  (lgreen/leader-define-key
;;;;; Top Level Keys
;;;;; --------------
    ":" '(execute-extended-command :wk "M-x")
    "SPC" '(projectile-find-file :wk "Find file in project")
    ";" '(eval-expression :wk "Eval expression")
    "." '(find-file :wk "Find file")
    "u" '(universal-argument :wk "Universal Argument")

;;;;; Local Leader
;;;;; ------------
    "m" '(:ignore t :wk "localleader")

;;;;; Find files and things
;;;;; ---------------------
    "f" '(:ignore t :wk "file")
    "f f" '(find-file :wk "Find file")
    "f r" '(recentf :wk "Recent files")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.default/README.org")) :wk "Edit emacs config")

;;;;; Search
;;;;; ---------------
    "s" '(:ignore t :wk "search")

;;;;; Projects
;;;;; --------
    "p" '(:ignore t :wk "project")

;;;;; Git Interface
;;;;; -------------
    "g" '(:ignore t :wk "git")

;;;;; Buffer Management
;;;;; -----------------
    "b" '(:ignore t :wk "buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b d" '(kill-this-buffer :wk "Delete buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b N" '(evil-buffer-new :wk "New buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(save-buffer :wk "Save buffer")
    "b S" '(evil-write-all :wk "Save all buffers")

;;;;; Window Management
;;;;; -----------------
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
    "w L" '(evil-window-move-far-right :wk "Move window far right")

;;;;; Insert
;;;;; ----
;;;;; TODO Add other useful bindings from Doom Emacs
    "i" '(:ignore t :wk "insert")
    "i f" '(lgreen/insert-current-filename :wk "Current filename")

;;;;; Help
;;;;; ----
    "h" '(:ignore t :wk "help")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h m" '(describe-mode :wk "Describe mode")

;;;;; Open
;;;;; ----
    "o" '(:ignore t :wk "open")

;;;;; Toggles
;;;;; -------
    "t" '(:ignore t :wk "toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t i" '(indent-guide-global-mode :wk "Toggle indent guides")
    "t w" '(visual-line-mode :wk "Toggle truncated lines")

;;;;; Quit
;;;;; ----
    "q" '(:ignore t :wk "quit")
    "q q" '(save-buffers-kill-terminal :wk "Quit")

;;;;; Org-Capture
;;;;; -----------
    "x" '(org-capture :wk "Capture")))

(elpaca-wait)
;; use-package declarations beyond this point may use the `:general' use-package keyword.

;;; _
(provide 'init-keybindings)
