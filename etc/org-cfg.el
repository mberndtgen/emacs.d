;;; extras.el --- small extra functions -*- lexical-binding: t; -*-
;;; Commentary:


;;; Code:

(use-package org
  ;; taken from https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out))
  :init
  (progn
    ;;(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))
    ;;(add-hook 'org-mode-hook 'turn-on-flyspell)
    ;;;(add-hook 'org-mode-hook 'turn-on-auto-fill)
    ;;(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
    ;;; for org-eww, clone it with git clone git://orgmode.org/org-mode.git
    ;;; (add-to-list 'load-path "~/Documents/src/emacs/org-mode/lisp/")
    )

  :config
  (progn
    ;;   ;; The GTD part of this config is heavily inspired by
    ;;   ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    ;;   (require 'ob)
    (setq org-directory "~/Dropbox/orgfiles")
    (setq org-agenda-files
          (mapcar (lambda (path) (concat org-directory path))
                  '("/work.org"
                    "/home.org")))
    (setq org-log-done t)
    ;;   (setq org-src-fontify-natively t)
    ;;   (setq org-use-speed-commands t)
    ;;   (setq org-ellipsis "…")               ;;; replace the "..." with "…" for collapsed org-mode content
    ;;   (setq org-return-follows-link t)      ;;; RET follows hyperlinks in org-mode:
    ;;   (setq org-startup-align-all-tables t) ;;; Can be set per file basis with: #+STARTUP: noalign (or align).
    ;;                                       ;;; Same as doing C-c C-c in a table.
    ;;   (setq org-capture-templates
    ;;         '(("t" "Todo [inbox]" entry
    ;;            (file+headline "~/org/gtd/inbox.org" "Tasks")
    ;;            "* TODO %i%?")
    ;;           ("T" "Tickler" entry
    ;;            (file+headline "~/org/gtd/tickler.org" "Tickler")
    ;;            "* %i%? \n %^t")))
    ;;   (setq org-refile-targets
    ;;         '(("~/org/gtd/gtd.org" :maxlevel . 3)
    ;;           ("~/org/gtd/someday.org" :level . 1)
    ;;           ("~/org/gtd/tickler.org" :maxlevel . 2)))
    ;;   (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    ;;   ;; (setq org-clock-persist t)
    ;;   (org-clock-persistence-insinuate)
    (custom-set-variables
        '(org-directory "~/Dropbox/orgfiles")
    ;;    '(org-default-notes-file (concat org-directory "/notes.org"))
    ;;    '(org-export-html-postamble nil)
    ;;    '(org-export-allow-bind-keywords t)
    ;;    '(org-latex-listings 'minted)
    ;;    '(org-hide-leading-stars t)
    ;;    '(org-startup-folded (quote overview))
    ;;    '(org-startup-indented t)
        '(org-babel-load-languages
          '((emacs-lisp . t)
            (python . t)
            (shell . t)
            (haskell . t)
            (js . t)
            (latex . t)
            (gnuplot . t)
            (C . t)
            (sql . t)
            (ditaa . t)))
        '(org-confirm-babel-evaluate nil))


    ;;   (setq org-file-apps (append '(("\\.pdf$" . "evince %s")) org-file-apps ))
    
    ;;   (setq org-src-fontify-natively t) ;;; fontlock src blocks even when outside
    ;;   (setq org-edit-src-content-indentation 0) ;;; no extra indentation for contents in src code blocks

    ;;   ;; (setq org-agenda-custom-commands
    ;;   ;;       '(("c" "Simple agenda view"
    ;;   ;;          ((agenda "")
    ;;   ;;           (alltodo "")))))

    ;;   ;; (use-package org-ac
    ;;   ;;   :ensure t
    ;;   ;;   :init
    ;;   ;;   (progn
    ;;   ;;     (require 'org-ac)
    ;;   ;;     (org-ac/config-default)))


    ;;   (defadvice org-capture-finalize
    ;;       (after delete-capture-frame activate)
    ;;     "Advise capture-finalize to close the frame"
    ;;     (if (equal "capture" (frame-parameter nil 'name))
    ;;         (delete-frame)))
    
    ;;   (defadvice org-capture-destroy
    ;;       (after delete-capture-frame activate)
    ;;     "Advise capture-destroy to close the frame"
    ;;     (if (equal "capture" (frame-parameter nil 'name))
    ;;         (delete-frame)))

    ;;   ;; (use-package noflet
    ;;   ;;   :ensure t)
    
    ;;   (defun make-capture-frame ()
    ;;     "Create a new frame and run org-capture."
    ;;     (interactive)
    ;;     (make-frame '((name . "capture")))
    ;;     (select-frame-by-name "capture")
    ;;     (delete-other-windows)
    ;;     (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    ;;             (org-capture)))

    
    ;;   ;; wrap text in org-mode block (see http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/)

    ;;   (defun org-begin-template ()
    ;;     "Make a template at point."
    ;;     (interactive)
    ;;     (if (org-at-table-p)
    ;;         (call-interactively 'org-table-rotate-recalc-marks)
    ;;       (let* ((choices '(("s" . "SRC")
    ;;                         ("e" . "EXAMPLE")
    ;;                         ("q" . "QUOTE")
    ;;                         ("v" . "VERSE")
    ;;                         ("c" . "CENTER")
    ;;                         ("l" . "LaTeX")
    ;;                         ("h" . "HTML")
    ;;                         ("a" . "ASCII")))
    ;;              (key
    ;;               (key-description
    ;;                (vector
    ;;                 (read-key
    ;;                  (concat (propertize "Template type: " 'face 'minibuffer-prompt)
    ;;                          (mapconcat (lambda (choice)
    ;;                                       (concat (propertize (car choice) 'face 'font-lock-type-face)
    ;;                                               ": "
    ;;                                               (cdr choice)))
    ;;                                     choices
    ;;                                     ", ")))))))
    ;;         (let ((result (assoc key choices)))
    ;;           (when result
    ;;             (let ((choice (cdr result)))
    ;;               (cond
    ;;                ((region-active-p)
    ;;                 (let ((start (region-beginning))
    ;;                       (end (region-end)))
    ;;                   (goto-char end)
    ;;                   (insert "#+END_" choice "\n")
    ;;                   (goto-char start)
    ;;                   (insert "#+BEGIN_" choice "\n")))
    ;;                (t
    ;;                 (insert "#+BEGIN_" choice "\n")
    ;;                 (save-excursion (insert "#+END_" choice))))))))))

    ;;   ;;bind to key
    ;;   (define-key org-mode-map (kbd "C-<") 'org-begin-template)
    
    ;;   ;;(require 'org-eww)
    ))

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
;; (eval-after-load "ox-latex"
;;   '(progn
;;      (message "Now loading org-latex export settings")
;;      ;; use with: #+LATEX_CLASS: myclass
;;      ;;#+LaTeX_CLASS_OPTIONS: [a4paper,twoside,twocolumn]
;;      ;; (add-to-list 'org-latex-classes
;;      ;;              '("myclass" "\\documentclass[11pt,a4paper]{article}
;;      ;;                 [NO-DEFAULT-PACKAGES]
;;      ;;                 [NO-PACKAGES]"
;;      ;;                ("\\usepackage[utf8]{inputenc}")
;;      ;;                ("\\usepackage[T1]{fontenc}")
;;      ;;                ("\\usepackage{graphicx}")
;;      ;;                ("\\usepackage{longtable}")
;;      ;;                ("\\usepackage{amssymb}")
;;      ;;                ("\\usepackage[colorlinks=true,urlcolor=SteelBlue4,linkcolor=Firebrick4]{hyperref}")
;;      ;;                ("\\usepackage[hyperref,x11names]{xcolor}")
;;      ;;                ("\\section{%s}" . "\\section*{%s}")
;;      ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;      ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;      ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;      ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;      (unless (boundp 'org-latex-classes)
;;        (setq org-latex-classes nil))

;;      (setq org-latex-classes t)

;;      (setq org-latex-pdf-process
;;            '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;      (add-to-list 'org-latex-packages-alist '("" "minted"))

;;      ;; you must include the listings package
;;      ;;(add-to-list 'org-export-latex-packages-alist '("" "listings")) ;; don't work in Org 8?

;;      ;; if you want colored source code then you need to include the color package
;;      ;;(add-to-list 'org-export-latex-packages-alist '("" "color"))
;;      ))


;; Org Publish to Stat Blog to Jekyll config Added 26 Mar 2015
;; http://orgmode.org/worg/org-tutorials/org-jekyll.html
;; Thanks to Ian Barton
;; (setq org-publish-project-alist
;;       '(
;;         ("org-mberndtgen"
;;          ;; Path to your org files.
;;          :base-directory "~/Dropbox/orgfiles/GitHubPages/org/"
;;          :base-extension "org"

;;          ;; Path to your Jekyll project.
;;          :publishing-directory "~/Dropbox/orgfiles/GitHubPages/jekyll/"
;;          :recursive t
;;          :publishing-function org-publish-org-to-html
;;          :headline-levels 4
;;          :html-extension "html"
;;          :body-only t ;; Only export section between <body> </body>
;;          )

;;         ("org-static-mberndtgen"
;;          :base-directory "~/Dropbox/orgfiles/GitHubPages/org/"
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
;;          :publishing-directory "~/Dropbox/GitHubPages/"
;;          :recursive t
;;          :publishing-function org-publish-attachment)

;;         ("mberndtgen" :components ("org-mberndtgen" "org-static-mberndtgen"))
;;         ))

;; (use-package htmlize
;;   :ensure t)


(provide 'org-cfg)

;;; org-cfg.el ends here
