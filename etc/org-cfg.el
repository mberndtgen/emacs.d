;; org-cfg.el --- small extra functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; parts taken from https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org
;;; most configs taken from http://doc.norang.ca/org-mode.html

;;; Code:

;; (use-package bbdb
;;   :ensure t
;;   ;;:bind (("<f9> p" . bh/phone-call))
;;   )

;; (use-package bbdb-com
;;   :ensure t)


(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org
  :commands (org-capture org-agenda)
  :hook ((org-mode . efs/org-mode-setup)
         (org-agenda-mode . (lambda () (hl-line-mode 1))))
  :mode (("\\.org$'" . org-mode)
         ("\\.org_archive$'" . org-mode)
         ("\\.txt$'" . org-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out)
         ("M-C-g" . org-plot/gnuplot)
         ("M-q" . toggle-truncate-lines)
         ("<f12>" . org-agenda)
         ;;("<f5>" . bh/org-todo)
         ;;("<S-f5>" . bh/widen)
         ("<C-f6>" . (lambda () (interactive) (bookmark-set "SAVED")))
         ("<f6>" . (lambda () (interactive) (bookmark-jump "SAVED")))
         ;;("<f7>" . bn/set-truncate-lines)
         ;;("<f7>" . bh/set-truncate-lines)
         ("<f8>" . org-cycle-agenda-files)
         ;;("<f9> <f9>" . bh/show-org-agenda)
         ("<f9> b" . bbdb)
         ("<f9> c" . calendar)
         ("<f9> f" . boxquote-insert-file)
         ("<f9> g" . gnus)
         ;;("<f9> h" . bh/hide-other)
         ;;("<f9> n" . bh/toggle-next-task-display)
         ;;("<f9> I" . bh/punch-in)
         ;;("<f9> O" . bh/punch-out)
         ;;("<f9> o" . bh/make-org-scratch)
         ("<f9> r" . boxquote-region)
         ;;("<f9> s" . bh/switch-to-scratch)
         ;;("<f9> t" . bh/insert-inactive-timestamp)
         ;;("<f9> T" . bh/toggle-insert-inactive-timestamp)
         ("<f9> v" . visible-mode)
         ("<f9> l" . org-toggle-link-display)
         ;;("<f9> SPC" . bh/clock-in-last-task)
         ("C-<f9>" . previous-buffer)
         ("M-<f9>" . org-toggle-inline-images)
         ("C-x n r" . narrow-to-region)
         ("C-<f10>" . next-buffer)
         ("<f11>" . org-clock-goto)
         ("C-<f11>" . org-clock-in)
         ;;("C-s-<f12>" . bh/save-then-publish)
         )
  :custom
  (org-directory "~/Dropbox/orgfiles")
  ;; Compact the block agenda view
  (org-agenda-compact-blocks t)
  (org-agenda-custom-commands '(("d" "Dashboard"
                                 ((agenda "" ((org-deadline-warning-days 7)))
                                  (todo "NEXT"
                                        ((org-agenda-overriding-header "Next Tasks")))
                                  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

                                ("n" "Next Tasks"
                                 ((todo "NEXT"
                                        ((org-agenda-overriding-header "Next Tasks")))))

                                ("W" "Work Tasks" tags-todo "+work-email")

                                ;; Low-effort next actions
                                ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
                                 ((org-agenda-overriding-header "Low Effort Tasks")
                                  (org-agenda-max-todos 20)
                                  (org-agenda-files org-agenda-files)))

                                ("w" "Workflow Status"
                                 ((todo "WAIT"
                                        ((org-agenda-overriding-header "Waiting on External")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "REVIEW"
                                        ((org-agenda-overriding-header "In Review")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "PLAN"
                                        ((org-agenda-overriding-header "In Planning")
                                         (org-agenda-todo-list-sublevels nil)
                                         (org-agenda-files org-agenda-files)))
                                  (todo "BACKLOG"
                                        ((org-agenda-overriding-header "Project Backlog")
                                         (org-agenda-todo-list-sublevels nil)
                                         (org-agenda-files org-agenda-files)))
                                  (todo "READY"
                                        ((org-agenda-overriding-header "Ready for Work")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "ACTIVE"
                                        ((org-agenda-overriding-header "Active Projects")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "COMPLETED"
                                        ((org-agenda-overriding-header "Completed Projects")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "CANC"
                                        ((org-agenda-overriding-header "Cancelled Projects")
                                         (org-agenda-files org-agenda-files)))))))
  (org-agenda-files (mapcar (lambda (path) (concat org-directory path))
                            '("/work.org"
                              "/home.org")))
  (org-agenda-persistent-filter t)
  (org-ascii-links-to-notes nil)
  (org-ascii-headline-spacing (quote (1 . 1)))
  ;; (org-blank-before-new-entry '((heading)
  ;;                               (plain-list-item . auto)))
  (org-capture-templates '(("t" "todo" entry (file org-default-notes-file)
                            "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                           ("r" "respond" entry (file org-default-notes-file)
                            "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                           ("n" "note" entry (file org-default-notes-file)
                            "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                           ("j" "Journal" entry (file+datetree "~/Dropbox/orgfiles/diary.org")
                            "* %?\n%U\n" :clock-in t :clock-resume t)
                           ("w" "org-protocol" entry (file org-default-notes-file)
                            "* TODO Review %c\n%U\n" :immediate-finish t)
                           ("m" "Meeting" entry (file org-default-notes-file)
                            "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                           ("p" "Phone call" entry (file org-default-notes-file)
                            "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                           ("h" "Habit" entry (file org-default-notes-file)
                            "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
  ;; Do not prompt to resume an active clock
  (org-clone-delete-id t)
  (org-cycle-include-plain-lists t)
  ;; handling blank lines
  (org-cycle-separator-lines 0)
  ;; agenda visibility
  (org-default-notes-file "~/Dropbox/orgfiles/refile.org")
  ;; Separate drawers for clocking and logs
  (org-drawers '("PROPERTIES" "LOGBOOK"))
  ;; enable task blocking - prevents tasks from changing to DONE if any subtasks are still open
  (org-enforce-todo-dependencies t )
  (org-ellipsis " ▾")
  (org-export-with-smart-quotes t)
  (org-hide-emphasis-markers t)
  (org-html-table-default-attributes '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none"))
  ;; attachments
  (org-id-method 'uuidgen)
  ;; See https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
  (org-image-actual-width nil)
  ;; Use the current window for indirect buffer display
  (org-indirect-buffer-display 'current-window)
  (org-insert-heading-respect-content nil)
  ;; logging
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  ;; Targets complete directly with IDO
  (org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes (quote confirm))
  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (org-refile-use-outline-path t)
  ;; RET follows hyperlinks in org-mode:
  (org-return-follows-link t)
  ;; notes at the top
  (org-reverse-note-order nil)
  ;; searching and showing results
  (org-show-following-heading t)
  (org-show-hierarchy-above t)
  (org-show-siblings '((default)))
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window)
  ;; Can be set per file basis with: #+STARTUP: noalign (or align). Same as doing C-c C-c in a table.
  (org-startup-align-all-tables t)
  (org-startup-indented t)
  (org-startup-truncated t)
  ;; tags with fast selection keys
  (org-tag-alist '((:startgroup) ;; put mutually exclusive tasks here
                   ("@errand" . ?e)
                   ("@office" . ?o)
                   ("@home" . ?H)
                   ("@farm" . ?f)
                   (:endgroup)
                   ("AGENDA" . ?a)
                   ("PLANNING" . ?p)
                   ("WAITING" . ?w)
                   ("HOLD" . ?h)
                   ("PERSONAL" . ?P)
                   ("WORK" . ?W)
                   ("FARM" . ?F)
                   ("ORG" . ?O)
                   ("NORANG" . ?N)
                   ("crypt" . ?E)
                   ("NOTE" . ?n)
                   ("IDEA" . ?i)
                   ("CANCELLED" . ?c)
                   ("FLAGGED" . ??)))
  (org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                  ("WAITING" ("WAITING" . t))
                                  ("HOLD" ("WAITING") ("HOLD" . t))
                                  (done ("WAITING") ("HOLD"))
                                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                  ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  ;; exporting tables to csv
  (org-table-export-default-format "orgtbl-to-csv")
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "BACKLOG(b)" "PLAN(p)" "ACTIVE(a)" "REVIEW(v)" "MEETING(m)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(k@)" "COMPLETED(c)")))
  (org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                            ("NEXT" :foreground "blue" :weight bold)
                            ("DONE" :foreground "forest green" :weight bold)
                            ("WAITING" :foreground "orange" :weight bold)
                            ("HOLD" :foreground "magenta" :weight bold)
                            ("CANCELLED" :foreground "forest green" :weight bold)
                            ("MEETING" :foreground "forest green" :weight bold)
                            ("PHONE" :foreground "forest green" :weight bold)))
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-use-fast-todo-selection t)
  (org-use-speed-commands t)
  (org-time-stamp-rounding-minutes '(1 1)) ;bh/keep-clock-running nil
  (org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM") ; Set default column view headings: Task Effort Clock_Summary
  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                           ("STYLE_ALL" . "habit")))
  (org-fast-tag-selection-single-key 'expert) ; Allow setting single tags without the menu
  (org-archive-location "%s_archive::* Archived Tasks") ; archiving
  (org-babel-results-keyword "results") ; babel
  (org-babel-default-header-args '((:eval . "never-export")))
  (org-agenda-text-search-extra-files '(agenda-archives)) ; Include agenda archive files when searching for things
  (org-special-ctrl-a/e t) ; editing and special key Handling
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-hide-leading-stars nil) ; show leading stars
  ;; minimize emacs frames
  (org-link-frame-setup '((vm . vm-visit-folder)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)))
  (org-src-window-setup 'current-window) ; Use the current window for C-c ' source editing
  ;; modules for habit tracking
  (org-modules '(ol-bbdb
                 ol-bibtex
                 org-crypt
                 ol-gnus
                 org-id
                 ol-info
                 org-habit
                 org-inlinetask
                 ol-irc
                 ol-mhe
                 org-protocol
                 ol-rmail
                 ol-w3m
                 org-tempo))
  (require-final-newline t)
  
  :config
  ;; Global
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq
   ;; Inline images in HTML instead of producting links to the image
   org-html-inline-images t
   ;; Do not use sub or superscripts - I currently don't need this functionality in my documents
   org-export-with-sub-superscripts nil
   ;; Use org.css from the norang website for export document stylesheets
   ;; org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />"
   org-html-head-include-default-style nil
   ;; remove xml header line for html exports
   org-html-xml-declaration '(("html" . "")
                              ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                              ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))

   ;; Do not generate internal css formatting for HTML exports
   org-export-htmlize-output-type 'css
   ;; Export with LaTeX fragments
   org-export-with-LaTeX-fragments t
   ;; Increase default number of headings to export
   org-export-headline-levels 6
   ;; allow #+BIND: vars to be set on export w/o confirmation
   org-export-allow-BIND t
   ;; narrowing
   org-show-entry-below '((default))
   org-agenda-include-diary nil
   org-agenda-diary-file "~/Dropbox/orgfiles/diary.org"
   org-agenda-insert-diary-extract-time t
   ;; Show all future entries for repeating tasks
   org-agenda-repeating-timestamp-show-all t
   ;; Show all agenda dates - even if they are empty
   org-agenda-show-all-dates t
   ;; Sorting order for tasks on the agenda
   org-agenda-sorting-strategy '((agenda habit-down time-up user-defined-up effort-up category-keep)
                                 (todo category-up effort-up)
                                 (tags category-up effort-up)
                                 (search category-up))
   ;; Start the weekly agenda on Monday
   org-agenda-start-on-weekday 1
   org-agenda-start-with-log-mode t
   ;; Enable display of the time grid so we can see the marker for the current time
   org-agenda-time-grid '((daily today remove-match)
                          #("----------------" 0 16 (org-heading t))
                          (0900 1100 1300 1500 1700))
   ;; Display tags farther right
   org-agenda-tags-column -102
   ;; Agenda sorting functions
   ;;org-agenda-cmp-user-defined 'bh/agenda-sort
   ;; Use sticky agenda's so they persist
   org-agenda-sticky t
   ;; position the habit graph on the agenda to the right of the default
   org-habit-graph-column 60
   ;; speed commands
   org-use-speed-commands t
   org-speed-commands-user '(("0" . ignore)
                             ("1" . ignore)
                             ("2" . ignore)
                             ("3" . ignore)
                             ("4" . ignore)
                             ("5" . ignore)
                             ("6" . ignore)
                             ("7" . ignore)
                             ("8" . ignore)
                             ("9" . ignore)

                             ("a" . ignore)
                             ("d" . ignore)
                             ;;("h" . bh/hide-other)
                             ("i" progn
                              (forward-char 1)
                              (call-interactively 'org-insert-heading-respect-content))
                             ("k" . org-kill-note-or-show-branches)
                             ("l" . ignore)
                             ("m" . ignore)
                             ;;("q" . bh/show-org-agenda)
                             ("r" . ignore)
                             ("s" . org-save-all-org-buffers)
                             ("w" . org-refile)
                             ("x" . ignore)
                             ("y" . ignore)
                             ("z" . org-add-note)

                             ("A" . ignore)
                             ("B" . ignore)
                             ("E" . ignore)
                             ;;("F" . bh/restrict-to-file-or-follow)
                             ("G" . ignore)
                             ("H" . ignore)
                             ("J" . org-clock-goto)
                             ("K" . ignore)
                             ("L" . ignore)
                             ("M" . ignore)
                             ;;("N" . bh/narrow-to-org-subtree)
                             ;;("P" . bh/narrow-to-org-project)
                             ("Q" . ignore)
                             ("R" . ignore)
                             ("S" . ignore)
                             ;;("T" . bh/org-todo)
                             ;;("U" . bh/narrow-up-one-org-level)
                             ("V" . ignore)
                             ;;("W" . bh/widen)
                             ("X" . ignore)
                             ("Y" . ignore)
                             ("Z" . ignore))
   org-remove-highlights-with-change t
   org-read-date-prefer-future 'time
   ;; automatically change list bullets
   org-list-demote-modify-bullet '(("+" . "-")
                                   ("*" . "-")
                                   ("1." . "-")
                                   ("1)" . "-")
                                   ("A)" . "-")
                                   ("B)" . "-")
                                   ("a)" . "-")
                                   ("b)" . "-")
                                   ("A." . "-")
                                   ("B." . "-")
                                   ("a." . "-")
                                   ("b." . "-"))

   ;; remove indentation on agenda tags view
   org-tags-match-list-sublevels t
   org-table-use-standard-references 'from
   ;; Overwrite the current window with the agenda
   org-agenda-window-setup 'current-window
   org-clone-delete-id t
   org-startup-folded t
   ;; preserve source block indentation
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 0
   org-catch-invisible-edits 'error
   ;; keep utf9 as default coding system
   org-export-coding-system 'utf-8
   default-process-coding-system '(utf-8-unix . utf-8-unix)
   org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
   org-use-sub-superscripts t)
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  (efs/org-font-setup)
  :init
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)
)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (require 'ob-js)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (js . t)
     (lisp . t)
     (perl . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; insert structure template blocks
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("pl" . "src perl"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(with-eval-after-load 'org
  ;; reveal support
  (require 'emacs-reveal)
  ;; manual see https://github.com/yjwen/org-reveal
  ;; reveal.js home: https://github.com/hakimel/reveal.js/
  (use-package helm-org
    :config
    (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))
  ;; The following custom-set-faces create the highlights
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t)))

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
		 ("\\usepackage[colorlinks=true,urlcolor=SteelBlue4,linkcolor=Firebrick4]{hyperref}")
		 ("\\usepackage[hyperref,x11names]{xcolor}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'org-cfg)

;;; org-cfg.el ends here
