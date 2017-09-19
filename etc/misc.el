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
  ;;(add-hook 'lisp-mode-hook '(lambda () (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))
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


;; for interactively building regular expressions
(use-package re-builder
  :ensure t
  :config
  (setf reb-re-syntax 'read))

;; enable aggressive-indenting for some modes
;; see https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'css-mode-hook #'aggressive-indent-mode)
    (add-hook 'clojure-mode #'aggressive-indent-mode)
    (add-hook 'lisp-mode #'aggressive-indent-mode)))

;; visually display kill ring
;; see https://github.com/browse-kill-ring/browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :config
  (global-set-key "\C-cy" 'browse-kill-ring))

;; show vertical lines to guide indentation
;; see https://github.com/zk-phi/indent-guide
(use-package indent-guide
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'indent-guide-mode)
    (add-hook 'css-mode-hook #'indent-guide-mode)
    (add-hook 'clojure-mode #'indent-guide-mode)
    (add-hook 'lisp-mode #'indent-guide-mode)))

;; open project explorer frame
(use-package project-explorer
  :ensure t
  :config
  (global-set-key (kbd "M-e") 'project-explorer-toggle))

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
