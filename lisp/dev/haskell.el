;;; dev/haskell.el -*- lexical-binding: t; -*-
;; (use-package haskell-mode
;;   :delight "λ"
;;   :after haskell-font-lock

;;   :config
;;   ;; Flycheck is usually slow for Haskell stuff - only run on save.
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save))

;;   :init
;;   (defun rvl/enable-subword-mode ()
;;     "Navigate within identifier names"
;;     (subword-mode +1))

;;   (defun rvl/stylish-on-save ()
;;     (setq haskell-stylish-on-save t))

;;   :hook ((haskell-mode . rvl/display-fill-column)
;;          (haskell-mode . rvl/stylish-on-save)
;;          (haskell-mode . rvl/font-lock-keywords)
;;          (haskell-mode . direnv-update-environment)

;;          (haskell-mode . rvl/enable-subword-mode)
;;          (haskell-mode . haskell-indentation-mode)
;;          (haskell-mode . imenu-add-menubar-index))

;;   :bind (:map haskell-mode-map
;;               ("C-c C-c" . haskell-process-cabal-build)
;;               ("C-c c" . haskell-process-cabal)
;;               ("C-c v c" . haskell-cabal-visit-file)
;;               ("C-c i" . haskell-navigate-imports)

;;               ;; YMMV with haskell-interactive-mode - LSP is a better bet
;;               ("C-`" . haskell-interactive-bring)
;;               ("C-c C-l" . haskell-process-load-file)
;;               ("C-c C-t" . haskell-process-do-type)
;;               ("C-c C-i" . haskell-process-do-info)
;;               ("C-c C-k" . haskell-interactive-mode-clear)

;;               ;; These are usually set by default, but just make sure:
;;               ("M-." . xref-find-definitions)
;;               ("M-," . xref-pop-marker-stack)
;;               ("M-," . xref-find-references)

;;               :map haskell-cabal-mode-map
;;               ("C-c C-c" . haskell-process-cabal-build)
;;               ("C-c c" . haskell-process-cabal))

;;   :custom
;;   (haskell-process-log t))

;; (use-package lsp-haskell
;;   :after (haskell-mode lsp-mode)
;;   :config
;;   ;; Comment/uncomment this line to see interactions between lsp client/server.
;;   ;; (setq lsp-log-io t)
;;   :custom
;;   ;;(lsp-haskell-process-args-hie '("-d" "-l" "/tmp/hie.log"))
;;   ;;(lsp-haskell-server-args ())
;;   (lsp-haskell-server-path "haskell-language-server"))


;; (use-package direnv
;;   :config
;;   ;; enable globally
;;   (direnv-mode)
;;   ;; exceptions
;;   ;; (add-to-list 'direnv-non-file-modes 'foobar-mode)
;;   ;; nix-shells make too much spam -- hide
;;   (setq direnv-always-show-summary nil)
;;   :hook
;;   ;; ensure direnv updates before flycheck and lsp
;;   ;; https://github.com/wbolster/emacs-direnv/issues/17
;;   (flycheck-before-syntax-check . direnv-update-environment)
;;   (lsp-before-open-hook . direnv-update-environment)
;;   :custom
;;   ;; quieten logging
;;   (warning-suppress-types '((direnv))))

;; Uncomment blocks above, then add "haskell" to my/dev-modules in local.el.
