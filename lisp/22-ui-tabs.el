;;; 22-ui-tabs.el -*- lexical-binding: t; -*-

;;; Code:

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
                                        ;(string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "TAGS*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(use-package centaur-tabs
  :demand t
  :hook ((dired-mode . centaur-tabs-local-mode)
         ;;(dashboard-mode . centaur-tabs-local-mode)
         (term-mode . centaur-tabs-local-mode)
         ;;(calendar-mode . centaur-tabs-local-mode)
         (org-agenda-mode . centaur-tabs-local-mode)
         ;;(helpful-mode . centaur-tabs-local-mode)
         )
  :custom
  (centaur-tabs-style "box")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-plain-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  :config
  (centaur-tabs-hide-tab "TAGS*")
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "fira code" 120)
  (centaur-tabs-mode t)
  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward)
         ;;("C-c t s" . centaur-tabs-counsel-switch-group)
         ;;("C-c t p" . centaur-tabs-group-by-projectile-project)
         ;;("C-c t g" . centaur-tabs-group-buffer-groups)
         ))
(provide '22-ui-tabs)
