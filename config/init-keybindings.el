;; init-keybindings.el --- -*- lexical-binding: t; -*-


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

;;;; Leader key definer
  ;; set up 'SPC' as the global leader key
  (general-create-definer lgreen/leader-define-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix lgreen/general-leader-key
    :non-normal-prefix lgreen/general-leader-alt-key)

;;;; Local-leader key definer
  ;; Define a local leader for all modes
  (general-create-definer lgreen/local-leader-define-key
    :states '(normal insert)
    :prefix lgreen/general-local-leader-key
    :non-normal-prefix lgreen/general-local-leader-alt-key)

;;;; Keymaps
  (lgreen/leader-define-key
;;;;; Top Level Keys
    "SPC" '(execute-extended-command :wk "execute command")
    "!" '(shell-command :wk "shell command")
    ":" '(pp-eval-expression :wk "eval expression")
    ";" '(comment-dwim :wk "comment")
    "." '(repeat :wk "repeat last command")
    "/" '(consult-line :wk "search buffer")
    "$" '(ispell-word :wk "spellcheck")
    "u" '(universal-argument :wk "universal argument")

;;;;; Files
    "f" '(:ignore t :wk "File")
    "f SPC" '(project-find-file :wk "find file [DWIM]")
    "f f" '(find-file :wk "find file")
    "f d" '(find-name-dired :wk "find file recursively")
    "f l" '(locate :wk "locate")
    "f r" '(recentf :wk "recent files")
    "f j" '(dired-jump :wk "jump to directory")

;;;;; Search
    "s" '(:ignore t :wk "Search")
    "s i" '(imenu :wk "symbols")
    "s b" '(isearch-forward :wk "search buffer")
    "s s" '(rgrep :wk "grep search")

;;;;; Code
    "c" '(:ignore t :wk "Code")
    "c C" '(compile :wk "compile")
    "c c" '(recompile :wk "rerun last compile")

;;;;; Projects
    "p" '(:ignore t :wk "Project")
    "p p" '(project-switch-project :wk "switch project")
    "p f" '(project-find-file :wk "find file")
    "p c" '(project-compile :wk "compile")
    "p d" '(project-dired :wk "dired")
    "p b" '(project-switch-to-buffer :wk "switch buffer")

;;;;; Git Interface
    "g" '(:ignore t :wk "Git")

;;;;; Buffer Management
    "b" '(:ignore t :wk "Buffer")
    "b b" '(switch-to-buffer :wk "switch buffer")
    "b d" '(evil-delete-buffer :wk "delete buffer")
    "b n" '(next-buffer :wk "next buffer")
    "b N" '(evil-buffer-new :wk "new buffer")
    "b p" '(previous-buffer :wk "previous buffer")
    "b r" '(revert-buffer :wk "reload buffer")
    "b R" '(rename-buffer :wk "rename buffer")
    "b s" '(save-buffer :wk "save buffer")
    "b S" '(evil-write-all :wk "save all buffers")

;;;;; Window Management
    "w" '(:ignore t :wk "Window")

    ;; Window creation
    "w s" '(split-window-below :wk "split window below")
    "w v" '(split-window-right :wk "split window right")

    ;; Window navigation
    "w w" '(evil-window-next :wk "next window")
    "w h" '(evil-window-left :wk "window left")
    "w k" '(evil-window-up :wk "window up")
    "w j" '(evil-window-down :wk "window down")
    "w l" '(evil-window-right :wk "window right")

    ;; Window moving
    "w H" '(evil-window-move-far-left :wk "move window far left")
    "w K" '(evil-window-move-very-top :wk "move window very top")
    "w J" '(evil-window-move-very-bottom :wk "move window very bottom")
    "w L" '(evil-window-move-far-right :wk "move window far right")

    ;; Window sizing
    "w =" '(balance-windows :wk "balance windows")
    "w f" '(toggle-frame-fullscreen :wk "toggle fullscreen")
    "w m" '(toggle-frame-maximized :wk "toggle maximized")

    ;; Window destruction
    "w q" '(evil-quit :wk "window close")
    "w d" '(delete-window :wk "delete window")
    "w o" '(delete-other-windows :wk "delete other windows")

;;;;; Act
    "a" '(:ignore t :wk "Act")

;;;;; Mode
    "m" '(:ignore t :wk "Mode")

;;;;; Outline
    "z" '(:ignore t :wk "Outline")

;;;;; Insert
    ;; TODO Add other useful bindings from Doom Emacs
    "i" '(:ignore t :wk "Insert")

;;;;; Help
    "h" '(:ignore t :wk "Help")
    "h b" '(describe-bindings :wk "describe bindings")
    "h m" '(describe-mode :wk "describe mode")

;;;;; Open
    "o" '(:ignore t :wk "Open")
    "o s" '(scratch-buffer :wk "scratch buffer")

;;;;; Toggles
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "toggle line numbers")
    "t h" '(hl-line-mode :wk "toggle highlight line")
    "t w" '(visual-line-mode :wk "toggle truncated lines")

;;;;; Checks
    "x" '(:ignore t :wk "Checks")

;;;;; Quit
    "q" '(:ignore t :wk "Quit")
    "q q" '(save-buffers-kill-terminal :wk "quit")))

;;;; -
(elpaca-wait)
;; use-package declarations beyond this point may use the `:general' use-package keyword.

;;; _
(provide 'init-keybindings)

;; Local Variables:
;; jinx-local-words: "dired eval fullscreen localleader"
;; End:
