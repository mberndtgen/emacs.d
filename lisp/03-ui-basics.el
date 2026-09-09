;;; 03-ui-basics.el -*- lexical-binding: t; -*-

;;; Code:

(setf backup-inhibited nil
      auto-save-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      echo-keystrokes 0.1
      delete-active-region nil
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold nil
      make-backup-files nil
      create-lockfiles nil
      ring-bell-function 'ignore
      auto-save-list-file-prefix "~/.emacs.d/auto-save/save-"
      backup-directory-alist `("." . ,(expand-file-name
                                       (concat user-emacs-directory "backups")))
      backup-inhibited nil
      set-fringe-mode 10)

;; set up visible bell
(setq visible-bell t)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(setq-default dired-allow-to-change-permissions t)

;; always pick latest version of the library to load
(setq load-prefer-newer t)

;; GUIs are for newbs
(dolist (mode'(menu-bar-mode tool-bar-mode tooltip-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Too distracting
(blink-cursor-mode -1)

;; overwrite text when highlighted
(delete-selection-mode t)
(global-display-line-numbers-mode t)

;; I never want to use this
(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))

;; I hate typing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Magit is the only front-end I care about
(setf vc-handled-backends nil
      ad-redefinition-action 'accept ; Don’t warn when advice is added for functions
      vc-follow-symlinks t)          ; Don’t warn for following symlinked files

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)
(setq-default truncate-lines t)

(column-number-mode t)
(global-auto-revert-mode t)
(setq-default comment-column 70) ; Set the default comment column to 70
(setq-default line-spacing 0.24)
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)
(setq-default frame-title-format '("%b - %f - %I")) ;; buffer name, full file name and size

;;; S - shift key
;;; M - Cmd key
;;; C - Ctrl key
;;; s - Option key

(electric-indent-mode +1) ;; indent after entering RET
(electric-pair-mode +1) ;; automatically add a closing paren
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '((".*" . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
(defvar bookmark-save-flag)
(setq bookmark-save-flag t)
(show-paren-mode t)
(add-hook 'cperl-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)
;; ediff single frame
(defvar ediff-window-setup-function)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq x-stretch-cursor t
      even-window-sizes nil)

(provide '03-ui-basics)
