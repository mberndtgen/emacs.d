;;; haskell-cfg.el - enable slime

;;----------------------------------------------------------------------------
;; haskell settings
;;----------------------------------------------------------------------------

(use-package intero
  :ensure t
  :mode ("\\.l?hsc?$'" . haskell-mode)
  :config
  (progn
    (add-hook 'haskell-mode-hook 'intero-mode)
    (add-hook 'haskell-mode-hook 'hindent-mode)
    (setq intero-global-mode 1)))

;; (use-package haskell-mode
;;    :mode ("\\.l?hsc?$'" . haskell-mode)
;;    :ensure t
;;    :commands haskell-mode
;;    :bind ("C-c C-s" . fix-imports)
;;    :config
;;
 ;; (progn
   ;;    (use-package flycheck-haskell
   ;;      :ensure t
   ;;      :commands flycheck-haskell-setup
   ;;      :config
   ;;      (eval-after-load "auto-complete" ( require 'flycheck-hdevtools)))

   ;;    (use-package hindent
   ;;      :commands hindent-mode
   ;;      :load-path "lisp")

   ;;    (defun fix-imports ()
   ;;      "fixes imports"
   ;;      (interactive)
   ;;      (sort-lines nil (region-beginning) (region-end))
   ;;      (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)#-"))

   ;;    (custom-set-variables
   ;;     '(haskell-ask-also-kill-buffers nil)
   ;;     '(haskell-process-type (quote stack-ghci))
   ;;     '(haskell-interactive-popup-errors nil))

   ;;    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
   ;;    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
   ;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
   ;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
   ;;    (add-hook 'haskell-mode-hook 'flycheck-mode)
   ;;    (add-hook 'haskell-mode-hook 'hindent-mode)
   ;;    (add-hook 'haskell-mode-hook (lambda ()
   ;;                                   (add-hook 'before-save-hook 'haskell-mode-format-imports nil t)
   ;;                                   (add-hook 'before-save-hook 'hindent-reformat-buffer)))
   ;;    (add-hook 'haskell-mode-hook (lambda ()
   ;;                                   (set (make-local-variable 'company-backends)
   ;;                                        (append '((company-capf company-dabbrev-code))
   ;;                                                company-backends))))
   ;;    (eval-after-load 'haskell-mode
   ;;      '(progn
   ;;         (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
   ;;         (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
   ;;         (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
   ;;         (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)))
   ;;    (eval-after-load 'haskell-cabal
   ;;      '(progn
   ;;         (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
   ;;         (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
   ;;         (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

   ;;    (use-package haskell-interactive-mode
   ;;      :commands haskell-interactive-mode
   ;;      :config
   ;;      (define-key haskell-interactive-mode-map (kbd "C-c C-t") nil))



(provide 'haskell-cfg)

;;; end of haskell-cfg.el
