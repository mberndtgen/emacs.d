;;; 20-ui-theme.el -*- lexical-binding: t; -*-

;;; Code:

(use-package s)
(use-package dash)
(use-package logview)

;; (use-package command-log-mode
;;   :straight t
;;   :commands command-log-mode
;;   :config (global-command-log-mode t))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
  (doom-themes-neotree-config)     ; Enable custom neotree theme (all-the-icons must be installed!)
  :init
  (load-theme 'doom-wilmersdorf t)
  ;; for treemacs users (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; NOTE: 1st time you load config on a new machine,
;; remember to run 'M-x all-the-icons-install-fonts' first!
(use-package all-the-icons)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 10)
  (doom-modeline-bar-width 6)
;;(doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))
(provide '20-ui-theme)
