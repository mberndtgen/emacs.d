;;; init.el --- Modular Emacs configuration loader -*- lexical-binding: t; -*-

;;; Code:

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 2)
                  gc-cons-percentage 0.1)
            (message "Emacs ready in %s with %d GCs."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(let ((default-directory (expand-file-name user-emacs-directory)))
  (normal-top-level-add-to-load-path '("lisp")))

(load (expand-file-name "lisp/01-core.el" user-emacs-directory) nil 'nomessage)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 3) ("melpa" . 2) ("melpa-stable" . 1)))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose nil
      use-package-compute-statistics nil)

;; Must run after package-initialize (01-core loads too early for this).
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config (exec-path-from-shell-initialize))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (when (file-exists-p bootstrap-file)
    (load bootstrap-file nil 'nomessage)))

(defvar my/config-modules
  '("02-session"
    "03-ui-basics"
    "04-keys"
    "04-german"
    "05-helm"
    "05-swiper"
    "23-ui-navigation"
    "06-editing"
    "07-checks"
    "08-company"
    "12-org-ui"
    "10-org"
    "11-org-export"
    "20-ui-theme"
    "21-ui-fonts"
    "22-ui-tabs"
    "30-dired"
    "31-documents"
    "40-lisp-sly"
    "41-lisp-elisp"))

(defvar my/dev-modules nil
  "List of opt-in dev module names (files in lisp/dev/). Set in local.el.")

(dolist (module my/config-modules)
  (my/load-config-module module))

(when (file-exists-p (locate-user-emacs-file "local.el"))
  (load (locate-user-emacs-file "local.el") nil t))

(dolist (module (or my/dev-modules nil))
  (my/load-config-module (concat "dev/" module)))

(setq gc-cons-threshold (* 2 1000 1000)
      gc-cons-percentage 0.6)

(custom-set-variables
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   '("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(fci-rule-color "#2a2a2a")
 '(scroll-preserve-screen-position 'always)
 '(which-key-mode t))

(provide 'init)
;;; init.el ends here
