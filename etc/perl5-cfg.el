;;; perl5-cfg.el --- provide perl5 functions
;;; Commentary:
;;; Code:
(use-package cperl-mode
  :ensure t
  :bind
  (:map cperl-mode-map
        ("C-c d" . helm-perldoc))
  :hook
  ((cperl-mode . #'flycheck-mode)
   (cperl-mode . #'auto-complete-mode)
   (cperl-mode . #'turn-on-eldoc-mode))
  :mode ("\.pl$" . cperl-mode)
  :after
  (add-hook 'cperl-mode-hook 'perltidy-mode)
  :init
  (autoload 'perltidy "perltidy-mode" nil t)
  (autoload 'perltidy-mode "perltidy-mode" nil t)
  :config
  ;; cperl-mode
  (setq cperl-indent-level 4
        cperl-continued-statement-offset 4
        cperl-close-paren-offset -4
        cperl-label-offset -4
        cperl-comment-column 40
        cperl-highlight-variables-indiscriminately t
        cperl-indent-parens-as-block t
        cperl-tab-always-indent nil
        cperl-font-lock t
        cperl-set-style "PerlStyle")
  ;; auto-complete
  (make-variable-buffer-local 'ac-sources)
  (setq ac-sources '(ac-source-perl-completion))
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  ;; helm-perldoc
  (helm-mode 1)
  (indent-guide-mode 1)
  (helm-perldoc:setup))

(provide 'perl5-cfg)

;;; perl5-cfg.el ends here
