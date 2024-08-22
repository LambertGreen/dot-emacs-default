;; init-tty.el --- -*- lexical-binding: t; -*-


(use-package emacs
  :ensure nil
  :config
  (unless (display-graphic-p)
    (setq xterm-set-window-title t)))

;;; Clipetty
;; Use OSC 52 escape sequences to enable copy/paste in the terminal
(use-package clipetty
  :config
  (global-clipetty-mode 1))

;;; Evil-Terminal-Cursor-Changer
;; Changing cursor shape and color in the terminal
(use-package evil-terminal-cursor-changer
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

;; _
(provide 'init-tty)

;; Local Variables:
;; jinx-local-words: "tty"
;; End:
