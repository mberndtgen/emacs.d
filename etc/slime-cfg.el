;;; slime-cfg.el - enable slime

;;----------------------------------------------------------------------------
;; slime settings
;;----------------------------------------------------------------------------

(use-package slime
  :ensure t
  :mode "\\.lisp\\'"
  :config
  (progn
    ;;(add-to-list 'load-path "~/.emacs.d/slime/")
    ;;(add-to-list 'load-path "~/Documents/src/git/slime/")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setf inferior-lisp-program "/usr/local/bin/sbcl")

    (require 'slime)
    (require 'slime-company)
    (require 'slime-autoloads)

    (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
    (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
    (add-hook 'slime-load-hook #'(lambda ()
                                   (slime-setup '(slime-fancy
                                                  slime-banner
                                                  slime-repl
                                                  slime-fuzzy
                                                  slime-asdf
                                                  ;; slime-autodoc
                                                  slime-c-p-c
                                                  ;;slime-editing-commands
                                                  slime-fancy-inspector
                                                  slime-highlight-edits
                                                  ;; slime-parse
                                                  ;; slime-presentation-streams
                                                  ;; slime-presentations
                                                  ;; slime-references
                                                  slime-sbcl-exts
                                                  slime-package-fu
                                                  slime-fontifying-fu
                                                  ;; slime-mdot-fu
                                                  ;; slime-scratch
                                                  slime-tramp
                                                  ;; slime-enclosing-context
                                                  ;; slime-typeout-frame
                                                  slime-xref-browser
                                                  slime-cl-indent
                                                  slime-sprof
                                                  slime-autodoc
                                                  hippie-expand-slime))))

    (after-load 'slime
      (setq slime-net-coding-system 'utf-8-unix)
      (setq slime-startup-animation t)
      (setq slime-kill-without-query-p t)

      (setq slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
      (setq slime-ros-completion-function (quote ido-completing-read))
      (global-set-key "\C-cs" 'slime-selector)
      (global-set-key "\C-ch" 'common-lisp-hyperspec)
      (add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
      (define-key slime-mode-map (kbd "C-c m") 'slime-macroexpand-1)
      (setq slime-complete-symbol*-fancy t))


    (after-load 'slime-repl
      ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
      (after-load 'paredit
        (add-hook 'slime-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'slime-mode-hook 'enable-paredit-mode)
        (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
        (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
        (add-to-list 'auto-mode-alist '("slime-repl" . slime-repl-mode))
        ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
        (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)
        (setq slime-repl-history-file "~/.emacs.d/.slime-history.eld")
        (setq slime-repl-history-size 2000)
        (setq slime-repl-only-save-lisp-buffers nil))

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
          (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode)))))))

(provide 'slime-cfg)

;;; end of slime-cfg.el
