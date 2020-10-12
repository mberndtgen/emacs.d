;;; other-languages.el --- enable other languages

;;; Commentary:
;;----------------------------------------------------------------------------
;; other language settings
;;----------------------------------------------------------------------------

;;; Code:

(use-package css-mode
  :ensure t
  :mode ("\\.css\\'" . css-mode)
  :config
  (custom-set-variables
   '(css-indent-offset 2)))

(use-package arduino-mode
  :ensure t
  :mode ("\\.ino\\'" . arduino-mode))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.ya?ml$\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local paragraph-separate ".*>-$\\|[   ]*$")
              (setq-local paragraph-start paragraph-separate))))

(use-package terraform-mode
  :ensure t
  :defer t
  :mode ("\\.tf\\'" . terraform-mode)
  :config (setf terraform-indent-level 4))

(use-package nasm-mode
  :ensure t
  :defer t
  :mode ("\\.n?asm$\\'" . nasm-mode)
  :config
  (add-hook 'nasm-mode-hook (lambda () (setf indent-tabs-mode t))))

(use-package asm-mode
  :config
  (add-hook 'asm-mode-hook (lambda () (setf indent-tabs-mode t
                                       tab-always-indent t))))

(use-package batch-mode
  :defer t)

(use-package lisp-mode
  :defer t
  :config
  (progn
    (defun ert-all ()
      (interactive)
      (ert t))
    (defun ielm-repl ()
      (interactive)
      (pop-to-buffer (get-buffer-create "*ielm*"))
      (ielm))
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm-repl)
    (defalias 'lisp-interaction-mode 'emacs-lisp-mode)
    (font-lock-add-keywords
     'emacs-lisp-mode
     `((,(concat "(\\(\\(?:\\(?:\\sw\\|\\s_\\)+-\\)?"
                 "def\\(?:\\sw\\|\\s_\\)*\\)\\_>"
                 "\\s-*'?" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
        (1 'font-lock-keyword-face)
        (2 'font-lock-function-name-face nil t)))
     :low-priority)))


(provide 'other-languages)
;;; other-languages.el ends here
