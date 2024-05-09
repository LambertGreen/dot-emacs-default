;;; early-init.el --- -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Increase GC threshold during startup, the `gcmh' package
;; will then dynamically control the value thereafter
(setq gc-cons-threshold most-positive-fixnum)
