;; init-python.el --- -*- lexical-binding: t; -*-

;;; Pyenv
;; Setup a specific python environment
(use-package pyenv-mode
  :after python
  :hook ((python-mode . pyenv-mode)
         (python-mode . lgreen/update-python-shell-interpreter))
  :init
  ;; Function to update python-shell-interpreter from VIRTUAL_ENV
  (defun lgreen/update-python-shell-interpreter ()
    "Set python-shell-interpreter based on the active virtual environment."
    (when-let ((venv (getenv "VIRTUAL_ENV")))
      (setq-local python-shell-interpreter (concat venv "/bin/python3"))
      (message "Using virtual environment: %s" venv))))

;;; _
(provide 'init-python)
