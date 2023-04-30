;;; package --- sly-cfg.el - enable sly

;;; Commentary:

;;----------------------------------------------------------------------------
;; sly settings
;;----------------------------------------------------------------------------

;;; Code:

(use-package sly
  :ensure t
  :defer t
  :mode "\\.lisp$"
  :hook ((sly-mode . rainbow-delimiters-mode)
         (sly-mode . enable-paredit-mode)
         (sly-mode . company-mode)
         (sly-mode . prettify-symbols-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-mode . company-mode)
         (lisp-mode . sly-symbol-completion-mode)
         ;; (sly-mode . (lambda () (unless (sly-connected-p)
         ;;                     (save-excursion (sly)))))
         )
  :bind (:map sly-prefix-map
              ("M-h" . sly-documentation-lookup))
  :config
  (setq inferior-lisp-program "sbcl"
        sly-net-coding-system 'utf-8-unix
        sly-complete-symbol-function 'sly-flex-completions)
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (add-to-list 'auto-mode-alist '("sly-mrepl" . sly-mrepl-mode))
  (eval-after-load "sly"  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup)))

(use-package sly-quicklisp
  :after sly
  :defer t
  :ensure t
  :config
  (require 'sly-quicklisp-autoloads))

(provide 'sly-cfg)

;;; sly-cfg.el ends here
