;;; perl6-cfg.el - enable perl 6

;;----------------------------------------------------------------------------
;; perl6 settings
;;----------------------------------------------------------------------------

(use-package perl6-mode
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (use-package flycheck-perl6
    :ensure t))

(provide 'perl6-cfg)

;;; end of perl6-cfg.el
