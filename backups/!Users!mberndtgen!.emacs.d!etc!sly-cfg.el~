;;; sly-cfg.el - enable sly

;;----------------------------------------------------------------------------
;; sly settings
;;----------------------------------------------------------------------------

(use-package sly
  :ensure t
  :mode "\\.lisp"
  :init
  (progn
    (require 'sly-autoloads)
    (setf inferior-lisp-program "/usr/local/bin/sbcl"))
  :config
  (progn
    (eval-after-load 'sly
      `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))
    (eval-after-load 'sly-mrepl
      `(define-key sly-mrepl-mode-map (kbd "C-c C-k") 'sly-mrepl-clear-recent-output))
    (after-load 'sly
      (setq sly-net-coding-system 'utf-8-unix)
      (setq sly-complete-symbol-function (quote slime-fuzzy-complete-symbol))
      (add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode)))
    (after-load 'sly-mrepl
      ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
      (after-load 'paredit
        (add-hook 'sly-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'sly-mode-hook 'enable-paredit-mode)
        (add-hook 'sly-repl-mode-hook 'enable-paredit-mode)
        (define-key sly-mrepl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
        (add-to-list 'auto-mode-alist '("sly-mrepl" . sly-mrepl-mode))
        ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
        (define-key sly-mrepl-mode-map (kbd "TAB") 'indent-for-tab-command)))

    (use-package sly-quicklisp
      :defer t
      :ensure t
      :init
      (progn
        require 'sly-quicklisp-autoloads))

    (use-package sly-company
      :defer t
      :ensure t
      :init
      (add-hook 'sly-mode-hook #'sly-company-mode))

    (add-hook 'sly-mode-hook
              (lambda ()
                (unless (sly-connected-p)
                  (save-excursion (sly)))))))

(provide 'sly-cfg)

;;; end of sly-cfg.el
