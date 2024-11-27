;;; early-init.el --- -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Increase GC threshold during startup to speed things up,
;; but revert to a better value post startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Restore `gc-cons-threshold'
(add-hook 'after-init-hook
		  (lambda ()
			(setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))
