;;; misc.el - enable miscellaneous packages

;;----------------------------------------------------------------------------
;; misc packages
;;----------------------------------------------------------------------------

(use-package jekyll
  :demand t
  :functions httpd-send-header
  :config
  (progn
    (setf jekyll-home "~/src/skeeto.github.com/")
    (when (file-exists-p jekyll-home)
      (require 'simple-httpd)
      (setf httpd-root (concat jekyll-home "_site"))
      (ignore-errors
        (httpd-start)
        (jekyll/start))
      (defservlet robots.txt text/plain ()
        (insert "User-agent: *\nDisallow: /\n")))))

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map (kbd "f") #'push-first-button))

(use-package gamegrid
  :defer t
  :init
  (setf gamegrid-user-score-file-directory (locate-user-emacs-file "games")))

(use-package apt-sources-mode
  :defer t
  :mode "sources.list$")

(use-package pov-mode
  :ensure t
  :defer t
  :init
  (autoload 'irfc-mode "irfc" nil t)
  (autoload 'irfc-visit "irfc" nil t)
  (setf irfc-directory (locate-user-emacs-file "local/rfc")
        irfc-assoc-mode t)
  (mkdir irfc-directory t))

(use-package ospl-mode
  :init
  (autoload 'ospl-mode "ospl-mode"))

(use-package sql
  :init
  (setf sql-product 'sqlite))

(use-package enriched
  :config
  (define-key enriched-mode-map "\C-m" nil))


;; depends on slime, clojure-, and other modes
(use-package eval-in-repl
  :ensure t
  :config
  (add-hook 'lisp-mode-hook '(lambda () (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))
  (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)
  (setq cider-overlays-use-font-lock t)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  ;; ielm support (for emacs lisp)
  (require 'eval-in-repl-ielm)
  ;; for .el files
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; for *scratch*
  (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; for M-x info
  (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm))


;; prettify symbols for various modes
(add-hook 'clojure-mode-hook 'my-add-pretty-lambda)
(add-hook 'haskell-mode-hook 'my-add-pretty-lambda)
(add-hook 'shen-mode-hook 'my-add-pretty-lambda)
(add-hook 'tex-mode-hook 'my-add-pretty-lambda)
(global-prettify-symbols-mode 1) ; display “lambda” as “λ”

;; make tab complete without losing ability to manually indent
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)


(provide 'misc)

;;; end of misc.el
