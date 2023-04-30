;;; golang-cfg.el --- Summary
;;; Commentary:
;;; golang-cfg.el ---- golang-cfg.el - enable golang

;;----------------------------------------------------------------------------
;; golang settings
;; heavily inspired by
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;; http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2/
;; http://arenzana.org/2015/Emacs-for-Go/
;; https://lupan.pl/dotemacs/
;;
;; install these packages for some go fun:
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef/...
;; go get -u golang.org/x/tools/cmd/guru -- former oracle package
;; go get -u github.com/dougm/goflymake
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/golang/lint/golint
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
;; go get   golang.org/x/tools/cmd/guru
;; go get golang.org/x/tools/gopls@latest
;; go get -u github.com/go-delve/delve/cmd/dlv
;;
;; using guru - see https://www.mirrorservice.org/sites/stable.melpa.org/packages/go-guru-readme.txt
;;                  and http://golang.org/s/using-guru
;;----------------------------------------------------------------------------

;;; Code:

(use-package go-eldoc
  :defer)

(defun my-go-electric-brace ()
  "Insert an opening brace may be with the closing one.
If there is a space before the brace also adds new line with
properly indented closing brace and moves cursor to another line
inserted between the braces between the braces."
  (interactive)
  (insert "{")
  (when (looking-back " {" 10)
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (insert "}")
      (indent-according-to-mode))))

;; (use-package golint
;;   :ensure t
;;   :config
;;   (progn
;;     (setenv "GOPATH" (concat (getenv "HOME") "/Documents/src/go"))
;;     (add-to-list 'load-path (concat (getenv "HOME") "/Documents/src/go/src/github.com/golang/lint/misc/emacs"))))

(defun my-godoc-package ()
  "Display godoc for given package (with completion)."
  (interactive)
  (godoc (or (helm :sources (helm-build-sync-source "Go packages"
                              :candidates (go-packages))
                   :buffer "*godoc packages*")
             (signal 'quit nil))))

(use-package go-mode
  :commands go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-c e g" . godoc)
              ("C-c P" . my-godoc-package)
              ("C-c C-c" . compile)
              ("M-." . godef-jump))
  :mode ("\\.go$\\'" . go-mode)
  :hook (go-mode . lsp)
  :init
  (setq compile-command "go generate && go build -v && go test -v && go vet")
  (setq compilation-read-command nil)
  :config
  (defvar lsp-gopls-staticcheck)
  (defvar lsp-eldoc-render-all)
  (defvar lsp-gopls-complete-unimported)
  (add-to-list 'load-path (concat (getenv "HOME") "/go/bin"))
  ;; (local-set-key (kbd "M-*") 'pop-tag-mark)
  (add-hook 'go-mode-hook (lambda () (progn
                                  (add-hook 'before-save-hook #'lsp-format-buffer t t)
                                  (add-hook 'before-save-hook #'lsp-organize-imports t t)
                                  (auto-complete-mode 1))))
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (setq compilation-window-height 14)
  (setq compilation-scroll-output t)
  (setq lsp-gopls-staticcheck t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-gopls-complete-unimported t)
  (helm-mode 1)
  (indent-guide-mode 1))


(use-package go-guru
  :ensure t
  :after go-mode)

(use-package protobuf-mode
  :after go-mode
  :mode ("\\.proto$\\'" . protobuf-mode))

(with-eval-after-load 'dap-mode
  (require 'dap-go)
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra))))

(with-eval-after-load 'go-guru
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))

(add-hook 'compilation-mode-hook 'my-compilation-hook)

;; (defun my-show-doc-in-frame (buffer options)
;;   ;; Get the frame named 'compilation' or create one if such a frame does not exist
;;   (let ((help-frame (select-frame (or (cdr (assoc-string "compilation" (make-frame-names-alist)))
;;                                       (make-frame '((name . "compilation")))))))
;;     ;; This assumes you want to display just one window in the dedicated frame
;;     (set-window-buffer (car (window-list help-frame))  buffer nil)))

;; (add-to-list 'display-buffer-alist (cons "compilation" (cons #'my-show-doc-in-frame nil)))

(provide 'golang-cfg)

;;; golang-cfg.el ends here
