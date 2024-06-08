;;; early-init.el --- -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Increase GC threshold during startup to speed things up,
;; but revert to a better value post startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Restore `gc-cons-threshold'
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))))

;;; No Titlebar
;; Some UI elements we want to set sooner rather than later
;; Uncomment the below to remove the titlebar
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
