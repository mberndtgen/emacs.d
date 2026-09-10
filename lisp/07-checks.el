;;; 07-checks.el -*- lexical-binding: t; -*-

;;; Code:

(use-package writegood-mode
  :bind (("C-c g" . writegood-mode)
         ("C-c C-g g" . writegood-grade-level)
         ("C-c C-g e" . writegood-reading-ease)))
(with-eval-after-load "flycheck-mode"
  (flycheck-define-checker proselint
    "A linter for prose"
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
        (id (one-or-more (not (any " "))))
        (message (one-or-more not-newline)
           (zero-or-more "\n" (any " ") (one-or-more not-newline)))
        line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))
(use-package flycheck
  :defer 10
  :init
  (setq flycheck-global-modes '(not org-mode)) ;; prevents unmatched brackets error when saving
  (global-flycheck-mode 1) 
  :custom
  (flycheck-display-errors-delay .3)
  :config
  (bind-key "H-!"
            (defhydra hydra-toggle (:color amaranth)
              "
  _c_ Check buffer      _x_ Explain error
  _n_ Next error        _h_ Show error
  _p_ Previous error
  _l_ Show all errors   _s_ Select syntax checker
  _C_ Clear errors      _?_ Describe syntax checker
  "
              ("c" flycheck-buffer)
              ("n" flycheck-next-error)
              ("p" flycheck-previous-error)
              ("l" flycheck-list-errors)
              ("C" flycheck-clear-errors)
              ("x" flycheck-explain-error-at-point)
              ("h" flycheck-display-error-at-point)
              ("s" flycheck-select-checker)
              ("?" flycheck-describe-checker)
              ("q" nil))))
(provide '07-checks)
