;;; unannoy.el --- disable Emacs' annoying bits
;;; Commentary:
;;; Code:

(setf backup-inhibited t
      auto-save-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      dired-allow-to-change-permissions t
      echo-keystrokes 0.1
      delete-active-region nil
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32)
      ring-bell-function 'ignore
      auto-save-default nil
      auto-save-list-file-prefix "~/.emacs.d/auto-save/save-"
      backup-directory-alist (quote (("." . "~/.emacs.d/saves")))
      backup-inhibited nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; GUIs are for newbs
(dolist (mode'(menu-bar-mode tool-bar-mode tooltip-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Too distracting
(blink-cursor-mode -1)

;; overwrite text when highlighted
(delete-selection-mode +1)

;; I never want to use this
(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))

;; I hate typing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always use the one true encoding
(prefer-coding-system 'utf-8-unix)

;; Insert key is stupid
(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)

;; I hate hitting this by accident
(global-set-key (kbd "C-<up>") #'previous-line)
(global-set-key (kbd "C-<down>") #'next-line)

;; Magit is the only front-end I care about
(setf vc-handled-backends nil
      vc-follow-symlinks t)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)

(provide 'unannoy)

;;; unannoy.el ends here
