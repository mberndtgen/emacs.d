;;; early-init.el --- Pre-init performance tweaks -*- lexical-binding: t; -*-

;; Reduce GC churn while Emacs loads packages and init files.
(setq gc-cons-threshold (* 1024 1024 32))

;; Let init.el control package loading (avoids double-loading elpa).
(setq package-enable-at-startup nil)

(provide 'early-init)
