;;; 08-company.el -*- lexical-binding: t; -*-

;;; Code:

(use-package company
  :defer 0.1
  :bind (("C-M-i" . company-complete)
         :map company-mode-map ("<backtab>" . company-ysnippet))
  ;; :hook (after-init . global-company-mode)
  :config
  ;; (setq-default
  ;;  company-minimum-prefix-length 0
  ;;  ;; get only preview
  ;;  company-frontends '(company-preview-frontend)
  ;;  ;; also get a drop down
  ;;  company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend))
  :init
  (setq global-company-mode nil
        company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        ;; company-global-modes '(not erc-mode message-mode help-mode
        ;;                            gud-mode eshell-mode shell-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)))


;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode 1))

;; 
(use-package company-box
  :hook (company-mode . company-box-mode))


(defun my-derived-lang-name ()
  "Return a derived language name for the current buffer."
  (let ((name (if (listp mode-name)
                  (car mode-name)
                mode-name)))
    (replace-regexp-in-string "\\(/.*\\|-ts-mode\\|-mode\\)$" "" (substring-no-properties name))))

(setq company-quickhelp-use-propertized-text t) ;; Optional

(with-eval-after-load 'company
  (advice-add 'company-quickhelp--doc :around
              (lambda (orig-fun &rest args)
                (let ((mode-name (my-derived-lang-name)))
                  (apply orig-fun args))))
  (add-hook 'company-mode-hook
            (lambda ()
              (setq-local mode-name (my-derived-lang-name))))
  (add-to-list 'company-backends 'company-capf))
(provide '08-company)
