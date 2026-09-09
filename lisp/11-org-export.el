;;; 11-org-export.el -*- lexical-binding: t; -*-

;;; Code:

(with-eval-after-load 'org
  (require 'ob-js)
;;  (require 'org-re-reveal-ref)
;;  (require 'oer-reveal-publish)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; insert structure template blocks
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  )

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(with-eval-after-load 'org
  ;; manual see https://github.com/yjwen/org-reveal
  ;; reveal.js home: https://github.com/hakimel/reveal.js/
  (use-package helm-org
    :config
    (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))
  )

(if (eq system-type 'gnu/linux)
    (setq org-reveal-root "file:///home/mberndtgen/Documents/src/emacs/reveal.js/"))
(if (eq system-type 'darwin)
    (setq org-reveal-root "file:///Users/v236177/Dropbox/orgfiles/reveal.js/"))
;;(setq org-reveal-mathjax t)

;;Org-export to LaTeX
(with-eval-after-load 'ox-latex
  (message "Now loading org-latex export settings")
  ;; page break after toc
  (setq org-latex-toc-command "\\tableofcontents \\clearpage"
        org-latex-listings t)
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
                 ("\\usepackage{tikzposter}")
                 ("\\usepackage[colorlinks=true,urlcolor=SteelBlue4,linkcolor=Firebrick4]{hyperref}")
                 ("\\usepackage[hyperref,x11names]{xcolor}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-packages-alist '())
  (add-to-list 'org-latex-packages-alist '("" "color" t))
  (add-to-list 'org-latex-packages-alist '("" "tabularx" t))
  (add-to-list 'org-latex-packages-alist '("" "longtable" t))
  (add-to-list 'org-latex-packages-alist '("" "array" t))
  (add-to-list 'org-latex-packages-alist '("" "tabu" t))
  (add-to-list 'org-latex-packages-alist '("" "fontenc" t))
  (add-to-list 'org-latex-packages-alist '("" "multirow" t)))
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
(provide '11-org-export)
