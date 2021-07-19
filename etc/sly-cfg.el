;;; sly-cfg.el - enable sly

;;----------------------------------------------------------------------------
;; sly settings
;;----------------------------------------------------------------------------

;;; Code:

(use-package sly
  :ensure t
  :mode "\\.lisp$"
  :hook ((sly-mode . rainbow-delimiters-mode)
         (sly-mode . enable-paredit-mode)
         (sly-repl-mode . enable-paredit-mode)
         (sly-mode . (lambda () (unless (sly-connected-p)
                             (save-excursion (sly)))))
         )
  :bind (:map sly-prefix-map
              ("M-h" . sly-documentation-lookup)
              ("TAB" . indent-for-tab-command))
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq sly-net-coding-system 'utf-8-unix)
  (setq sly-complete-symbol-function (quote slime-fuzzy-complete-symbol))
  (add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (add-to-list 'auto-mode-alist '("sly-mrepl" . sly-mrepl-mode)))

(use-package sly-quicklisp
  :after sly
  :defer t
  :ensure t
  :config
  (require 'sly-quicklisp-autoloads))

(use-package lisp-mode
  ;; stolen from https://github.com/jcf/emacs.d/blob/master/init-languages.org
  :mode (("\\.lisp$" . lisp-mode))
  :bind (:map lisp-mode-map ("C-c l" . lispdoc))
  :config
  (defun lispdoc ()
    "Searches lispdoc.com for SYMBOL, which is by default the symbol currently under the cursor."
    (interactive)
    (let* ((word-at-point (word-at-point))
     (symbol-at-point (symbol-at-point))
     (default (symbol-name symbol-at-point))
     (inp (read-from-minibuffer
     (if (or word-at-point symbol-at-point)
         (concat "Symbol (default " default "): ")
       "Symbol (no default): "))))
      (if (and (string= inp "") (not word-at-point) (not
                 symbol-at-point))
    (message "you didn't enter a symbol!")
  (let ((search-type (read-from-minibuffer
          "full-text (f) or basic (b) search (default b)? ")))
    (browse-url (concat "http://lispdoc.com?q="
            (if (string= inp "")
          default
        inp)
            "&search="
            (if (string-equal search-type "f")
          "full+text+search"
        "basic+search"))))))))

(provide 'sly-cfg)

;;; sly-cfg.el ends here
