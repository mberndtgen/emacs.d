;;; golang-cfg.el - enable golang

;;----------------------------------------------------------------------------
;; golang settings
;;----------------------------------------------------------------------------

(use-package ensime
  :ensure t
  :commands ensime
  :init
  (setq-default ensime-startup-notification nil)
  (setq-default ensime-startup-snapshot-notification nil))

(use-package go-eldoc
  :ensure t)

(use-package golint
  :ensure t
  :config
  (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs")))


(use-package go-mode
  :commands go-mode
  :ensure t
  :mode "\\.go$\\'"
  :init
  (setq compile-command "go build -v && go test -v && go vet && golint")
  ;(define-key (current-local-map) "\C-c\C-c" 'compile)
  :config
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (local-set-key (kbd "M-.") 'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;;(add-hook 'go-mode-hook 'go-mode-setup)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook (lambda () (progn
                                  (setq gofmt-command "goimports")
                                  (set (make-local-variable 'company-backends) '(company-go))
                                  (company-mode)
                                  (add-hook 'before-save-hook 'gofmt-before-save)))))

(use-package project-explorer
  :ensure t
  :config
  (global-set-key (kbd "M-e") 'project-explorer-toggle))

(provide 'golang-cfg)

;;; end of golang-cfg.el
