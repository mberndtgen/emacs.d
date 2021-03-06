;;; clojure-cfg.el --- enable clojure / cider

;;; Code:

;;----------------------------------------------------------------------------
;; clojure and cider settings
;;----------------------------------------------------------------------------

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\'" . clojure-mode)
  :config
  (progn
    (add-hook 'clojure-mode-hook 'enable-paredit-mode) ; Enable paredit for Clojure
    (add-hook 'clojure-mode-hook 'subword-mode) ; useful for working with camel-case tokens, like names of
    (use-package clojure-mode-extra-font-locking  ; A little more syntax highlighting
      :ensure t)
    (add-hook 'clojure-mode-hook ; syntax hilighting for midje
              (lambda ()
                (setq inferior-lisp-program "lein repl")
                (font-lock-add-keywords
                 nil
                 '(("(\\(facts?\\)"
                    (1 font-lock-keyword-face))
                   ("(\\(background?\\)"
                    (1 font-lock-keyword-face))))
                (define-clojure-indent (fact 1))
                (define-clojure-indent (facts 1))))))

(use-package cider
  :ensure t
  :hook ((cider-mode . subword-mode)
         (cider-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (cider-mode . rainbow-delimiters-mode)
         (cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . cider-company-enable-fuzzy-completion))
  :config
  (progn
    (setq cider-known-endpoints '("localhost" "127.0.0.1" "58289"))
    (setq nrepl-log-messages nil)
    (setq cider-repl-pop-to-buffer-on-connect t) ; go right to the REPL buffer when it's finished connecting
    ;; When there's a cider error, show its buffer and switch to it
    (setq cider-show-error-buffer t)
    (setq cider-auto-select-error-buffer t)
    (setq cider-repl-history-file "~/.emacs.d/cider-history") ; Where to store the cider history.
    (setq cider-repl-wrap-history t) ; Wrap when navigating history.
    (add-hook 'cider-repl-mode-hook 'paredit-mode) ; enable paredit in your REPL
    ;; Use clojure mode for other extensions
    (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.cljs?.*$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

    ;; key bindings
    (defun cider-start-http-server ()
      (interactive)
      ;;(cider-load-current-buffer)
      (let ((ns (cider-current-ns)))
        (cider-repl-set-ns ns)
        (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
        (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

    (defun cider-refresh ()
      (interactive)
      (cider-interactive-eval (format "(user/reset)")))

    (defun cider-user-ns ()
      (interactive)
      (cider-repl-set-ns "user"))

    (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
    (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
    (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
    (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)

    (use-package ac-cider
      :ensure t
      :hook ((cider-mode . ac-flyspell-workaround)
             (cider-mode . ac-cider-setup)
             (cider-repl-mode-hook . ac-cider-setup))
      :config
      (eval-after-load "auto-complete"
        '(progn
           (add-to-list 'ac-modes 'cider-mode)
           (add-to-list 'ac-modes 'cider-repl-mode))))))


(use-package anakondo
  :ensure t
  :commands anakondo-minor-mode)

(with-eval-after-load 'cider-mode
  ;; Enable anakondo-minor-mode in all Clojure buffers
  (add-hook 'clojure-mode-hook #'anakondo-minor-mode)
  ;; Enable anakondo-minor-mode in all ClojureScript buffers
  (add-hook 'clojurescript-mode-hook #'anakondo-minor-mode)
  ;; Enable anakondo-minor-mode in all cljc buffers
  (add-hook 'clojurec-mode-hook #'anakondo-minor-mode))

(provide 'clojure-cfg)

;;; clojure-cfg.el ends here
