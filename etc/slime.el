;;; slime.el - enable slime

;;----------------------------------------------------------------------------
;; slime settings
;;----------------------------------------------------------------------------

(use-package slime
  :ensure t
  :commands slime
  :init
  (progn
    (add-to-list 'load-path "~/Documents/src/git/slime")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program (executable-find "clisp"))
    (setq slime-lisp-implementations
          '((clisp ("/usr/local/bin/clisp"))
            (sbcl ("/usr/local/bin/sbcl"))
            (alisp "/usr/local/bin/alisp"))))
  :config
  (progn
    (add-hook 'slime-load-hook #'(lambda ()
                                   (slime-setup '(slime-fancy
                                                  slime-banner
                                                  slime-repl
                                                  slime-fuzzy
                                                  slime-asdf
                                                  slime-autodoc
                                                  slime-c-p-c
                                                  slime-editing-commands
                                                  slime-fancy-inspector
                                                  slime-highlight-edits
                                                  slime-parse
                                                  slime-presentation-streams
                                                  slime-presentations
                                                  slime-references
                                                  slime-sbcl-exts
                                                  slime-package-fu
                                                  slime-fontifying-fu
                                                  slime-mdot-fu
                                                  slime-scratch
                                                  slime-tramp
                                                  slime-enclosing-context
                                                  slime-typeout-frame
                                                  slime-xref-browser))))
    (setq slime-net-coding-system 'utf-8-unix)
    (setq slime-default-lisp 'sbcl)

    (defun start-slime ()
      (interactive)
      (unless (slime-connected-p)
        (save-excursion (slime))))

    (add-hook 'slime-mode-hook 'start-slime)
    (add-hook 'slime-load-hook #'(lambda () (require 'slime-fancy)))
    (add-hook 'inferior-lisp-mode-hook #'(lambda () (inferior-slime-mode t)))

    ;; Slime and Auto-Complete
    (use-package ac-slime
      :ensure t
      :init
      (progn
        (add-hook 'slime-mode-hook 'set-up-slime-ac)
        (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
        (add-hook 'slime-load-hook #'(lambda ()
                                       (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))))
      :config
      (progn
        (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode))))))



(provide 'slime)

;;; end of slime.el
