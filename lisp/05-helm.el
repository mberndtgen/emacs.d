;;; 05-helm.el --- Helm completion and navigation -*- lexical-binding: t; -*-

;;; Code:

(use-package hydra :demand t)

(use-package helm
  :diminish helm-mode
  :custom
  (helm-candidate-number-limit 100)
  (helm-input-idle-delay 0.01)
  (helm-ff-skip-boring-files t)
  :init
  (setq helm-idle-delay 0.0
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-autoresize-mode t
        helm-M-x-fuzzy-match t)
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x c o" . helm-occur)
         ("C-x c SPC" . helm-all-mark-rings))
  :config
  (ido-mode -1)
  (helm-mode))

(with-eval-after-load 'org
  (use-package helm-org
    :config
    (add-to-list 'helm-completing-read-handlers-alist
                 '(org-capture . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist
                 '(org-set-tags . helm-org-completing-read-tags))))

(add-hook 'helm-major-mode-hook
          (lambda () (setq auto-composition-mode nil)))

(provide '05-helm)
