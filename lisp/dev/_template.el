;;; dev/_template.el --- Template for opt-in dev modules -*- lexical-binding: t; -*-

;; Copy to dev/my-lang.el and add "my-lang" to `my/dev-modules' in local.el.

(use-package some-mode
  :defer t
  :mode ("\\.ext\\'" . some-mode)
  :hook (some-mode . (lambda () (message "some-mode hook"))))

(provide 'dev/_template)
