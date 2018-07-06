;;; package ---- golang-cfg.el - enable golang

;;----------------------------------------------------------------------------
;; golang settings
;; heavily inspired by
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;; http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2/
;; http://arenzana.org/2015/Emacs-for-Go/
;;
;; install these packages for some go fun:
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef/...
;; go get -u golang.org/x/tools/cmd/guru -- former oracle package
;; go get -u github.com/dougm/goflymake
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/goimports
;;
;; using guru - see https://www.mirrorservice.org/sites/stable.melpa.org/packages/go-guru-readme.txt
;;                  and http://golang.org/s/using-guru
;;----------------------------------------------------------------------------

(use-package go-eldoc
  :ensure t)

(use-package golint
  :ensure t
  :config
  (add-to-list 'load-path (concat (getenv "HOME") "/go/src/github.com/golang/lint/misc/emacs")))


(use-package go-mode
  :commands go-mode
  :ensure t
  :mode ("\\.go$\\'" . go-mode)
  :init
  (progn
    (setq compile-command "go generate && go build -v && go test -v && go vet")
    (define-key go-mode-map (kbd "C-c C-c") 'compile))
  :config
  (progn
    (add-to-list 'load-path (concat (getenv "HOME") "/go/bin"))
    (go-eldoc-setup)
    ;; Use goimports instead of go-fmt
    (setq gofmt-command "goimports")
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-*") 'pop-tag-mark)
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook (lambda () (progn
                                    (setq gofmt-command "goimports")
                                    (set (make-local-variable 'company-backends) '(company-go))
                                    (company-mode)
                                    (auto-complete-mode 1)
                                    (flycheck-mode)
                                    (add-hook 'before-save-hook 'gofmt-before-save))))
    (use-package go-guru
      :ensure t)

    (eval-after-load "go-guru"
      '(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

    (eval-after-load "auto-complete"
      '(use-package go-autocomplete
         :ensure t))))

(provide 'golang-cfg)

;;; golang-cfg ends here
