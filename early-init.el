;;; early-init.el --- -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Increase GC threshold during startup to speed things up,
;; but revert to a better value post startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Restore `gc-cons-threshold'
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

;; HACK: Fix libgccjit native compilation on macOS
;; This should be handled by the emacs-mac-exp@31 Homebrew formula
;; but it doesn't set up the GUI app environment properly.
;; Remove this when the package is fixed.
(when (eq system-type 'darwin)
  (when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    ;; Set up environment for libgccjit to find GCC
    (setenv "LIBRARY_PATH" "/opt/homebrew/lib:/opt/homebrew/lib/gcc/current")
    (setenv "COMPILER_PATH" "/opt/homebrew/bin")

    ;; Suppress native compilation warnings
    (setq native-comp-async-report-warnings-errors 'silent)))
