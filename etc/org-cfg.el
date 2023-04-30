;;; org-cfg.el --- small extra functions -*- lexical-binding: t; -*-
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
(use-package org
  :hook (
         (org-agenda-mode . (lambda () (hl-line-mode 1)))
         ((message-mode orgstruct-mode) . append)
         ((message-mode turn-on-auto-fill) . append)
         ((message-mode bbdb-define-all-aliases) . append)
         ((message-mode orgtbl-mode) . append)
         ((message-mode (lambda () (setq fill-column 72))) . append))
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
  :config
  ;; Global
  (setq org-directory "~/Dropbox/orgfiles"
        org-agenda-files (mapcar (lambda (path) (concat org-directory path))
                                 '("/work.org"
                                   "/home.org"))
        org-ascii-links-to-notes nil
        org-ascii-headline-spacing (quote (1 . 1))
        org-blank-before-new-entry '((heading) (plain-list-item . auto))
        org-clone-delete-id t
        org-cycle-include-plain-lists t
        ;; handling blank lines
        org-cycle-separator-lines 0
        org-blank-before-new-entry '((heading)
                                     (plain-list-item . auto))
        ;; deadlines and agenda visibility
        org-deadline-warning-days 30

        org-ellipsis "…" ;;; replace the "..." with "…" for collapsed org-mode content
        org-export-with-smart-quotes t
        org-enforce-todo-dependencies t ;; enable task blocking - prevents tasks from changing to DONE if any subtasks are still open
        org-hide-emphasis-markers t
        org-html-table-default-attributes '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none")
        ;; attachments
        org-id-method 'uuidgen
        org-image-actual-width nil ;;; See https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
        org-insert-heading-respect-content nil
        ;; logging
        org-log-done 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))
        org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                                 ("NEXT" :foreground "blue" :weight bold)
                                 ("DONE" :foreground "forest green" :weight bold)
                                 ("WAITING" :foreground "orange" :weight bold)
                                 ("HOLD" :foreground "magenta" :weight bold)
                                 ("CANCELLED" :foreground "forest green" :weight bold)
                                 ("MEETING" :foreground "forest green" :weight bold)
                                 ("PHONE" :foreground "forest green" :weight bold))
        org-return-follows-link t ;;; RET follows hyperlinks in org-mode:
        ;; notes at the top
        org-reverse-note-order nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'other-window
        ;; searching and showing results
        org-show-following-heading t
        org-show-hierarchy-above t
        org-show-siblings '((default))
        org-startup-align-all-tables t ;;; Can be set per file basis with: #+STARTUP: noalign (or align). Same as doing C-c C-c in a table.
        org-startup-indented t
        org-startup-truncated t
        ;; exporting tables to csv
        org-table-export-default-format "orgtbl-to-csv"
        org-use-speed-commands t
        org-agenda-custom-commands '(("c" "Simple agenda view"
                                      ((agenda "")
                                       (alltodo ""))))
        org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                       ("WAITING" ("WAITING" . t))
                                       ("HOLD" ("WAITING") ("HOLD" . t))
                                       (done ("WAITING") ("HOLD"))
                                       ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))
        org-default-notes-file "~/Dropbox/orgfiles/refile.org"
        org-capture-templates '(("t" "todo" entry (file org-default-notes-file)
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
                                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))
        ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        ;; Use full outline paths for refile targets - we file directly with IDO
        org-refile-use-outline-path t
        ;; Targets complete directly with IDO
        org-outline-path-complete-in-steps nil
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes (quote confirm)
        ;; Use IDO for both buffer and file completion and ido-everywhere to t
        ;;org-completion-use-ido t
        ;;ido-everywhere t
        ;;ido-max-directory-size 100000
        ;;ido-mode 'both
        ;; Use the current window when visiting files and buffers with ido
        ;;ido-default-file-method 'selected-window
        ;;ido-default-buffer-method 'selected-window
        ;; Use the current window for indirect buffer display
        org-indirect-buffer-display 'current-window
        ;;org-refile-target-verify-function 'bh/verify-refile-target
        ;; Do not dim blocked tasks
        org-agenda-dim-blocked-tasks nil
        ;; Compact the block agenda view
        org-agenda-compact-blocks t
        ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
        org-clock-history-length 23
        ;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Change tasks to NEXT when clocking in
        ;;org-clock-in-switch-to-state 'bh/clock-in-to-next
        ;; Separate drawers for clocking and logs
        org-drawers '("PROPERTIES" "LOGBOOK")
        ;; Save clock data and state changes and notes in the LOGBOOK drawer
        org-clock-into-drawer t
        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;; Clock out when moving task to a done state
        org-clock-out-when-done t
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        org-clock-persist t
        ;; Do not prompt to resume an active clock
        org-clock-persist-query-resume nil
        ;; Enable auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        ;; Include current clocking task in clock reports
        org-clock-report-include-clocking-task t
        ;;bh/keep-clock-running nil
        org-time-stamp-rounding-minutes '(1 1)
        org-agenda-clock-consistency-checks '(
                                              :max-duration "4:00"
                                              :min-duration 0
                                              :max-gap 0
                                              :gap-ok-around ("4:00")
                                              )
        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;; Agenda clock report parameters
        org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
        ;; Set default column view headings: Task Effort Clock_Summary
        org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
        ;; global Effort estimate values
        ;; global STYLE property values for completion
        org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                ("STYLE_ALL" . "habit"))
        ;; Agenda log mode items to display (closed and state changes by default)
        org-agenda-log-mode-items '(closed state)

        ;; tags with fast selection keys
        org-tag-alist '((:startgroup)
                        ("@errand" . ?e)
                        ("@office" . ?o)
                        ("@home" . ?H)
                        ("@farm" . ?f)
                        (:endgroup)
                        ("WAITING" . ?w)
                        ("HOLD" . ?h)
                        ("PERSONAL" . ?P)
                        ("WORK" . ?W)
                        ("FARM" . ?F)
                        ("ORG" . ?O)
                        ("NORANG" . ?N)
                        ("crypt" . ?E)
                        ("NOTE" . ?n)
                        ("CANCELLED" . ?c)
                        ("FLAGGED" . ??))
        ;; Allow setting single tags without the menu
        org-fast-tag-selection-single-key 'expert
        ;; For tag searches ignore tasks with scheduled and deadline dates
        org-agenda-tags-todo-honor-ignore-options t
        org-agenda-span 'day
        org-stuck-projects '("" nil nil "")
        ;; archiving
        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archived Tasks"
        org-alphabetical-lists t
        ;; ditaa & plantuml
        org-ditaa-jar-path "~/Dropbpx/orgfiles/modules/ditaa.jar"
        org-plantuml-jar-path "~/Dropbpx/orgfiles/modules/plantuml.jar"
        ;; babel
        org-babel-results-keyword "results"
        org-babel-default-header-args '((:eval . "never-export"))
        ;;org-startup-with-inline-images nil
        ;; experimenting with docbook exports - not finished
        org-export-docbook-xsl-fo-proc-command "fop %s %s"
        org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s" ; ##TODO##
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
        ;; Keep tasks with dates on the global todo lists
        org-agenda-todo-ignore-with-date nil
        ;; Keep tasks with deadlines on the global todo lists
        org-agenda-todo-ignore-deadlines nil
        ;; Keep tasks with scheduled dates on the global todo lists
        org-agenda-todo-ignore-scheduled nil
        ;; Keep tasks with timestamps on the global todo lists
        org-agenda-todo-ignore-timestamp nil
        ;; Remove completed deadline tasks from the agenda view
        org-agenda-skip-deadline-if-done t
        ;; Remove completed scheduled tasks from the agenda view
        org-agenda-skip-scheduled-if-done t
        ;; Remove completed items from search results
        org-agenda-skip-timestamp-if-done t
        ;; use diary for holidays and appointments
        org-agenda-include-diary nil
        org-agenda-diary-file "~/Dropbox/orgfiles/diary.org"
        org-agenda-insert-diary-extract-time t
        ;; Include agenda archive files when searching for things
        org-agenda-text-search-extra-files '(agenda-archives)
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
        ;; show leading stars
        org-hide-leading-stars nil
        ;; editing and special key Handling
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-yank-adjusted-subtrees t
        ;; minimize emacs frames
        org-link-frame-setup '((vm . vm-visit-folder)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file))
        ;; Use the current window for C-c ' source editing
        org-src-window-setup 'current-window
        ;; modules for habit tracking
        org-modules '(ol-bbdb
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
                      ol-w3m)
        ;; position the habit graph on the agenda to the right of the default
        org-habit-graph-column 50
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
        require-final-newline t
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
        org-agenda-persistent-filter t
        org-link-mailto-program '(compose-mail "%a" "%s")
        org-agenda-skip-additional-timestamps-same-entry t
        org-table-use-standard-references 'from

        ;; Overwrite the current window with the agenda
        org-agenda-window-setup 'current-window
        org-clone-delete-id t
        org-cycle-include-plain-lists t

        ;; insert structure template blocks
        org-structure-template-alist '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
                                       ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
                                       ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
                                       ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
                                       ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
                                       ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
                                       ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
                                       ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
                                       ("H" "#+html: " "<literal style=\"html\">?</literal>")
                                       ("a" "#+begin_ascii\n?\n#+end_ascii")
                                       ("A" "#+ascii: ")
                                       ("i" "#+index: ?" "#+index: ?")
                                       ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))

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

        org-use-sub-superscripts t
        )

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  :init
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)
  )

(eval-after-load "org"
  '(progn
     ;;(require 'ox-md nil t)
     ;; reveal support
     (require 'emacs-reveal)
     ;; manual see https://github.com/yjwen/org-reveal
     ;; reveal.js home: https://github.com/hakimel/reveal.js/
     ;; (use-package ox-reveal
     ;;   :ensure t)
     ;; (require 'ox-beamer)
     ;; (require 'ox-html)
     ;; (require 'ox-latex)
     ;; (require 'ox-ascii)
     ;;(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
     (use-package helm-org
       :ensure t
       :config
       (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
       (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))
     ;; The following custom-set-faces create the highlights
     (custom-set-faces
      ;; custom-set-faces was added by Custom.
      ;; If you edit it by hand, you could mess it up, so be careful.
      ;; Your init file should contain only one such instance.
      ;; If there is more than one, they won't work right.
      '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))))

(if (eq system-type 'gnu/linux)
    (setq org-reveal-root "file:///home/mberndtgen/Documents/src/emacs/reveal.js/"))
(if (eq system-type 'darwin)
    (setq org-reveal-root "file:///Users/v236177/Dropbox/orgfiles/reveal.js/"))
;;(setq org-reveal-mathjax t)

;;Org-export to LaTeX
(eval-after-load "ox-latex"
  '(progn
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
  )

(provide 'org-cfg)

;;; org-cfg.el ends here
