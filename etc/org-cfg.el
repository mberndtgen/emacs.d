;;; org-cfg.el --- small extra functions -*- lexical-binding: t; -*-
;;; Commentary:


;;; Code:

(use-package org
  ;; taken from https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org
  :after flyspell
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out)
         ("M-C-g" . org-plot/gnuplot)
         ("M-q" . toggle-truncate-lines))
  :config
  ;; Global
  (setq org-agenda-files (mapcar (lambda (path) (concat org-directory path))
                                 '("/work.org"
                                   "/home.org"))
        org-blank-before-new-entry '((heading) (plain-list-item . auto))
        org-clone-delete-id t
        org-cycle-include-plain-lists t
        org-cycle-separator-lines 2
        org-deadline-warning-days 30
        org-directory "~/Dropbox/orgfiles"
        org-ellipsis "…" ;;; replace the "..." with "…" for collapsed org-mode content
        org-enforce-todo-dependencies t
        org-hide-emphasis-markers t
        org-id-method 'uuidgen
        org-image-actual-width nil ;;; See https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
        org-insert-heading-respect-content nil
        org-log-done t
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-return-follows-link t ;;; RET follows hyperlinks in org-mode:
        org-reverse-note-order nil
        org-src-fontify-natively t
        org-src-window-setup 'other-window
        org-show-following-heading t
        org-show-hierarchy-above t
        org-show-siblings '((default))
        org-startup-align-all-tables t ;;; Can be set per file basis with: #+STARTUP: noalign (or align). Same as doing C-c C-c in a table.
        org-startup-indented t
        org-table-export-default-format "orgtbl-to-csv"
        org-use-speed-commands t
        )
  )

;; Add languages
(use-package ob-ipython :ensure t)
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (ditaa . t)
                               (R . t)
                               (ipython . t)
                               (ruby . t)
                               (gnuplot . t)
                               (clojure . t)
                               (shell . t)
                               (ledger . t)
                               (org . t)
                               (plantuml . t)
                               (shell . t)
                               (haskell . t)
                               (js . t)
                               (C . t)
                               (sql . t)
                               (latex . t)))

(use-package org-inlinetask
  :bind (:map org-mode-map
              ("C-c C-x t" . org-inlinetask-insert-task))
  :after (org)
  :commands (org-inlinetask-insert-task))

(eval-after-load "org"
  '(progn
     (require 'ox-md nil t)
     ;; reveal support
     ;; manual see https://github.com/yjwen/org-reveal
     ;; reveal.js home: https://github.com/hakimel/reveal.js/
     (use-package ox-reveal
       :ensure ox-reveal)
     (require 'ox-beamer)
     (require 'ox-latex)))

;;(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.5.0/")
(if (eq system-type 'gnu/linux)
    (setq org-reveal-root "file:///home/mberndtgen/Documents/src/emacs/reveal.js/"))
(if (eq system-type 'darwin)
    (setq org-reveal-root "file:///Users/v236177/Dropbox/dev/reveal.js/"))
(setq org-reveal-mathjax t)

;;Org-export to LaTeX
(eval-after-load "ox-latex"
  '(progn
     (message "Now loading org-latex export settings")
     ;; use with: #+LATEX_CLASS: myclass
     ;;#+LaTeX_CLASS_OPTIONS: [a4paper,twoside,twocolumn]
     (add-to-list 'org-latex-classes
                  '("myclass" "\\documentclass[11pt,a4paper]{article}
                     [NO-DEFAULT-PACKAGES]
                     [NO-PACKAGES]"
                    ("\\usepackage[utf8]{inputenc}")
                    ("\\usepackage[T1]{fontenc}")
                    ("\\usepackage{graphicx}")
                    ("\\usepackage{longtable}")
                    ("\\usepackage{amssymb}")
                    ("\\usepackage[colorlinks=true,urlcolor=SteelBlue4,linkcolor=Firebrick4]{hyperref}")
                    ("\\usepackage[hyperref,x11names]{xcolor}")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  )

(provide 'org-cfg)

;;; org-cfg.el ends here
