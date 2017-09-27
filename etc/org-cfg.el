;;; extras.el --- small extra functions -*- lexical-binding: t; -*-

;;; Code:

(use-package org
  ;; taken from https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org
  :defer 4
  :commands (org-babel-load-file
             org-babel-tangle-file)
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
    (add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))
    (add-hook 'org-mode-hook 'turn-on-flyspell)
    (add-hook 'org-mode-hook 'turn-on-auto-fill))
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
    (org-clock-persistence-insinuate)
    (custom-set-variables
     '(org-directory "~/Dropbox/orgfiles")
     '(org-default-notes-file (concat org-directory "/notes.org"))
     '(org-export-html-postamble nil)
     '(org-hide-leading-stars t)
     '(org-startup-folded (quote overview))
     '(org-startup-indented t))

    (setq org-file-apps (append '(("\\.pdf\\'" . "evince %s")) org-file-apps ))

    (global-set-key "\C-ca" 'org-agenda)

    (setq org-agenda-custom-commands
          '(("c" "Simple agenda view"
             ((agenda "")
              (alltodo "")))))

    (use-package org-ac
      :ensure t
      :init
      (progn
        (require 'org-ac)
        (org-ac/config-default)))

    (global-set-key (kbd "C-c c") 'org-capture)
    (defadvice org-capture-finalize
        (after delete-capture-frame activate)
      "Advise capture-finalize to close the frame"
      (if (equal "capture" (frame-parameter nil 'name))
          (delete-frame)))

    (defadvice org-capture-destroy
        (after delete-capture-frame activate)
      "Advise capture-destroy to close the frame"
      (if (equal "capture" (frame-parameter nil 'name))
          (delete-frame)))

    (use-package noflet
      :ensure t)
    
    (defun make-capture-frame ()
      "Create a new frame and run org-capture."
      (interactive)
      (make-frame '((name . "capture")))
      (select-frame-by-name "capture")
      (delete-other-windows)
      (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
	(org-capture)))))

(use-package org-inlinetask
  :bind (:map org-mode-map
              ("C-c C-x t" . org-inlinetask-insert-task))
  :after (org)
  :commands (org-inlinetask-insert-task))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(eval-after-load 'org
  (use-package org-babel
    :ensure t
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (emacs-lisp . t)
       (python . t)
       (shell . t)
       (haskell . t)
       (js . t)
       (latex . t)
       (gnuplot . t)
       (C . t)
       (sql . t)
       (ditaa . t)))
    ))

;; reveal support
;; manual see https://github.com/yjwen/org-reveal
;; reveal.js home: https://github.com/hakimel/reveal.js/
(use-package ox-reveal
  :ensure ox-reveal)

;;(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.5.0/")
(if (eq system-type 'gnu/linux)
    (setq org-reveal-root "file:///home/mberndtgen/Documents/src/emacs/reveal.js/"))
(if (eq system-type 'darwin)
    (setq org-reveal-root "file:///Users/mberndtgen/Documents/src/emacs/reveal.js/"))
(setq org-reveal-mathjax t)

(use-package htmlize
  :ensure t)

(provide 'org-cfg)

;;; org-cfg.el ends here
