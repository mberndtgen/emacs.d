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

(use-package org
  ;; taken from https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out))
  :init (add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))
  :config
  (progn
    ;; The GTD part of this config is heavily inspired by
    ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    (setq org-directory "~/org")
    (setq org-agenda-files
          (mapcar (lambda (path) (concat org-directory path))
                  '("/org.org"
                    "/gtd/gtd.org"
                    "/gtd/inbox.org"
                    "/gtd/tickler.org")))
    (setq org-log-done 'time)
    (setq org-src-fontify-natively t)
    (setq org-use-speed-commands t)
    (setq org-capture-templates
          '(("t" "Todo [inbox]" entry
             (file+headline "~/org/gtd/inbox.org" "Tasks")
             "* TODO %i%?")
            ("T" "Tickler" entry
             (file+headline "~/org/gtd/tickler.org" "Tickler")
             "* %i%? \n %^t")))
    (setq org-refile-targets
          '(("~/org/gtd/gtd.org" :maxlevel . 3)
            ("~/org/gtd/someday.org" :level . 1)
            ("~/org/gtd/tickler.org" :maxlevel . 2)))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-agenda-custom-commands
          '(("@" "Contexts"
             ((tags-todo "@email"
                         ((org-agenda-overriding-header "Emails")))
              (tags-todo "@phone"
                         ((org-agenda-overriding-header "Phone")))))))
    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)))

(use-package org-inlinetask
  :bind (:map org-mode-map
              ("C-c C-x t" . org-inlinetask-insert-task))
  :after (org)
  :commands (org-inlinetask-insert-task))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


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

(use-package aggressive-indent
  :ensure t
  :config
  ;; enable aggressive-indenting for some modes
  ;; see https://github.com/Malabarba/aggressive-indent-mode
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode #'aggressive-indent-mode)
  (add-hook 'lisp-mode #'aggressive-indent-mode))

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
