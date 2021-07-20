;;; org-cfg.el --- small extra functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; parts taken from https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org

;;; Code:

(use-package bbdb
  :ensure t
  :bind (("<f9> p" . bh/phone-call)))

;; (use-package bbdb-com
;;   :ensure t)

(use-package org
  :hook ((org-clock-out . bh/remove-empty-drawer-on-clock-out)
         (org-clock-out . bh/clock-out-maybe)
         (org-agenda-mode . (lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen)))))
         (org-agenda-mode . (lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project)))
         (org-agenda-mode . (lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock)))
         (org-agenda-mode . (lambda () (hl-line-mode 1)))
         (org-agenda-mode . (lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow)))
         (org-agenda-mode . (lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree)))
         (org-agenda-mode . (lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level)))
         (org-agenda-mode . (lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))))
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
         ("<f5>" . bh/org-todo)
         ("<S-f5>" . bh/widen)
         ("<f7>" . bn/set-truncate-lines)
         ("<f7>" . bh/set-truncate-lines)
         ("<f8>" . org-cycle-agenda-files)
         ("<f9> <f9>" . bh/show-org-agenda)
         ("<f9> b" . bbdb)
         ("<f9> c" . calendar)
         ("<f9> f" . boxquote-insert-file)
         ("<f9> g" . gnus)
         ("<f9> h" . bh/hide-other)
         ("<f9> n" . bh/toggle-next-task-display)
         ("<f9> I" . bh/punch-in)
         ("<f9> O" . bh/punch-out)
         ("<f9> o" . bh/make-org-scratch)
         ("<f9> r" . boxquote-region)
         ("<f9> s" . bh/switch-to-scratch)
         ("<f9> t" . bh/insert-inactive-timestamp)
         ("<f9> T" . bh/toggle-insert-inactive-timestamp)
         ("<f9> v" . visible-mode)
         ("<f9> l" . org-toggle-link-display)
         ("<f9> SPC" . bh/clock-in-last-task)
         ("C-<f9>" . previous-buffer)
         ("M-<f9>" . org-toggle-inline-images)
         ("C-x n r" . narrow-to-region)
         ("C-<f10>" . next-buffer)
         ("<f11>" . org-clock-goto)
         ("C-<f11>" . org-clock-in)
         ("C-s-<f12>" . bh/save-then-publish)
         )
  :config
  ;;
  ;; Phone capture template handling with BBDB lookup
  ;; Adapted from code by Gregory J. Grubbs
  (defun bh/phone-call()
    "Return name and company info for caller from bbdb lookup"
    (interactive)
    (let* (name rec caller)
      (setq name (completing-read "Who is calling? "
                                  (bbdb-hashtable)
                                  'bbdb-completion-predicate
                                  'confirm))
      (when (> (length name) 0)
                                        ; Something was supplied - look it up in bbdb
        (setq rec
              (or (first
                   (or (bbdb-search (bbdb-records) name nil nil)
                       (bbdb-search (bbdb-records) nil name nil)))
                  name)))

      ;; Build the bbdb link if we have a bbdb record, otherwise just return the name
      (setq caller (cond ((and rec (vectorp rec))
                          (let ((name (bbdb-record-name rec))
                                (company (bbdb-record-company rec)))
                            (concat "[[bbdb:"
                                    name "]["
                                    name "]]"
                                    (when company
                                      (concat " - " company)))))
                         (rec)
                         (t "NameOfCaller")))
      (insert caller)))

  (defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun bh/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (bh/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun bh/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun bh/is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun bh/list-sublevels-for-projects-indented ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun bh/list-sublevels-for-projects ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels t)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defvar bh/hide-scheduled-and-waiting-next-tasks t)

  (defun bh/toggle-next-task-display ()
    (interactive)
    (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
    (when  (equal major-mode 'org-agenda-mode)
      (org-agenda-redo))
    (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

  (defun bh/skip-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  nil
                next-headline)) ; a stuck project, has subtasks but no next task
          nil))))

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    ;; (bh/list-sublevels-for-projects-indented)
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    ;; (bh/list-sublevels-for-projects-indented)
    (if (save-excursion (bh/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              nil)
             ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
              nil)
             (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun bh/skip-non-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-task-p)
          nil)
         (t
          next-headline)))))

  (defun bh/skip-project-trees-and-habits ()
    "Skip trees that are projects"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-projects-and-habits-and-single-tasks ()
    "Skip trees that are projects, tasks that are habits, single non-project tasks"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((org-is-habit-p)
          next-headline)
         ((and bh/hide-scheduled-and-waiting-next-tasks
               (member "WAITING" (org-get-tags-at)))
          next-headline)
         ((bh/is-project-p)
          next-headline)
         ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
          next-headline)
         (t
          nil)))))

  (defun bh/skip-project-tasks-maybe ()
    "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max))))
             (limit-to-project (marker-buffer org-agenda-restrict-begin)))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (not limit-to-project)
               (bh/is-project-subtree-p))
          subtree-end)
         ((and limit-to-project
               (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-project-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         ((bh/is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-project-tasks ()
    "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         ((not (bh/is-project-subtree-p))
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-projects-and-habits ()
    "Skip trees that are projects and tasks that are habits"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-subprojects ()
    "Skip trees that are not projects"
    (let ((next-headline (save-excursion (outline-next-heading))))
      (if (bh/is-subproject-p)
          nil
        next-headline)))

  (defun bh/skip-non-archivable-tasks ()
    "Skip trees that are not available for archiving"
    (save-restriction
      (widen)
      ;; Consider only tasks with done todo headings as archivable candidates
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (if (member (org-get-todo-state) org-todo-keywords-1)
            (if (member (org-get-todo-state) org-done-keywords)
                (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                       (a-month-ago (* 60 60 24 (+ daynr 1)))
                       (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                       (this-month (format-time-string "%Y-%m-" (current-time)))
                       (subtree-is-current (save-excursion
                                             (forward-line 1)
                                             (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                  (if subtree-is-current
                      subtree-end ; Has a date in this month or last month, skip it
                    nil))  ; available to archive
              (or subtree-end (point-max)))
          next-headline))))
  (defun bh/set-truncate-lines ()
    "Toggle value of truncate-lines and refresh window display."
    (interactive)
    (setq truncate-lines (not truncate-lines))
    ;; now refresh window display (an idiom from simple.el):
    (save-excursion
      (set-window-start (selected-window)
                        (window-start (selected-window)))))

  (defun bh/make-org-scratch ()
    (interactive)
    (find-file "/tmp/publish/scratch.org")
    (gnus-make-directory "/tmp/publish"))

  (require 'gnus-util)

  (defun bh/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  ;; Remove empty LOGBOOK drawers on clock out
  (defun bh/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "LOGBOOK" (point))))

  (defun bh/org-auto-exclude-function (tag)
    "Automatic task exclusion in the agenda with / RET"
    (and (cond
          ((string= tag "hold")
           t)
          ((string= tag "farm")
           t))
         (concat "-" tag)))

;;;; Refile settings
  ;; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (defun bh/clock-in-to-next (kw)
    "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
             (bh/is-task-p))
        "NEXT")
       ((and (member (org-get-todo-state) (list "NEXT"))
             (bh/is-project-p))
        "TODO"))))

  (defun bh/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun bh/punch-in (arg)
    "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
    (interactive "p")
    (setq bh/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
        ;;
        ;; We're in the agenda
        ;;
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (bh/clock-in-organization-task-as-default)))
      ;;
      ;; We are not in the agenda
      ;;
      (save-restriction
        (widen)
                                        ; Find the tags on the current task
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
            (org-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))))

  (defun bh/punch-out ()
    (interactive)
    (setq bh/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))

  (defun bh/clock-in-default-task ()
    (save-excursion
      (org-with-point-at org-clock-default-task
        (org-clock-in))))

  (defun bh/clock-in-parent-task ()
    "Move point to the parent (project) task if any and clock in"
    (let ((parent-task))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (not parent-task) (org-up-heading-safe))
            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
              (setq parent-task (point))))
          (if parent-task
              (org-with-point-at parent-task
                (org-clock-in))
            (when bh/keep-clock-running
              (bh/clock-in-default-task)))))))

  (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

  (defun bh/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16))))

  (defun bh/clock-out-maybe ()
    (when (and bh/keep-clock-running
               (not org-clock-clocking-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
      (bh/clock-in-parent-task)))

  (defun bh/clock-in-task-by-id (id)
    "Clock in a task by id"
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil)))

  (defun bh/clock-in-last-task (arg)
    "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
    (interactive "p")
    (let ((clock-in-to-task
           (cond
            ((eq arg 4) org-clock-default-task)
            ((and (org-clock-is-active)
                  (equal org-clock-default-task (cadr org-clock-history)))
             (caddr org-clock-history))
            ((org-clock-is-active) (cadr org-clock-history))
            ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
            (t (car org-clock-history)))))
      (widen)
      (org-with-point-at clock-in-to-task
        (org-clock-in nil))))

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
        org-enforce-todo-dependencies t
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
        org-refile-target-verify-function 'bh/verify-refile-target
        ;; Do not dim blocked tasks
        org-agenda-dim-blocked-tasks nil
        ;; Compact the block agenda view
        org-agenda-compact-blocks t
        ;; Custom agenda command definitions
        org-agenda-custom-commands '(("N" "Notes" tags "NOTE"
                                      ((org-agenda-overriding-header "Notes")
                                       (org-tags-match-list-sublevels t)))
                                     ("h" "Habits" tags-todo "STYLE=\"habit\""
                                      ((org-agenda-overriding-header "Habits")
                                       (org-agenda-sorting-strategy
                                        '(todo-state-down effort-up category-keep))))
                                     (" " "Agenda"
                                      ((agenda "" nil)
                                       (tags "REFILE"
                                             ((org-agenda-overriding-header "Tasks to Refile")
                                              (org-tags-match-list-sublevels nil)))
                                       (tags-todo "-CANCELLED/!"
                                                  ((org-agenda-overriding-header "Stuck Projects")
                                                   (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                                   (org-agenda-sorting-strategy
                                                    '(category-keep))))
                                       (tags-todo "-HOLD-CANCELLED/!"
                                                  ((org-agenda-overriding-header "Projects")
                                                   (org-agenda-skip-function 'bh/skip-non-projects)
                                                   (org-tags-match-list-sublevels 'indented)
                                                   (org-agenda-sorting-strategy
                                                    '(category-keep))))
                                       (tags-todo "-CANCELLED/!NEXT"
                                                  ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                                         (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                             ""
                                                                                           " (including WAITING and SCHEDULED tasks)")))
                                                   (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                                   (org-tags-match-list-sublevels t)
                                                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-sorting-strategy
                                                    '(todo-state-down effort-up category-keep))))
                                       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                                  ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                                         (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                             ""
                                                                                           " (including WAITING and SCHEDULED tasks)")))
                                                   (org-agenda-skip-function 'bh/skip-non-project-tasks)
                                                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-sorting-strategy
                                                    '(category-keep))))
                                       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                                  ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                                         (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                             ""
                                                                                           " (including WAITING and SCHEDULED tasks)")))
                                                   (org-agenda-skip-function 'bh/skip-project-tasks)
                                                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-sorting-strategy
                                                    '(category-keep))))
                                       (tags-todo "-CANCELLED+WAITING|HOLD/!"
                                                  ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                                         (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                             ""
                                                                                           " (including WAITING and SCHEDULED tasks)")))
                                                   (org-agenda-skip-function 'bh/skip-non-tasks)
                                                   (org-tags-match-list-sublevels nil)
                                                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                                       (tags "-REFILE/"
                                             ((org-agenda-overriding-header "Tasks to Archive")
                                              (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                                              (org-tags-match-list-sublevels nil))))
                                      nil))
        org-agenda-auto-exclude-function 'bh/org-auto-exclude-function

        ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
        org-clock-history-length 23
        ;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Change tasks to NEXT when clocking in
        org-clock-in-switch-to-state 'bh/clock-in-to-next
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
        org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s"
        ;; Inline images in HTML instead of producting links to the image
        org-html-inline-images t
        ;; Do not use sub or superscripts - I currently don't need this functionality in my documents
        org-export-with-sub-superscripts nil
        ;; Use org.css from the norang website for export document stylesheets
        org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />"
        org-html-head-include-default-style nil
        ;; Do not generate internal css formatting for HTML exports
        org-export-htmlize-output-type (quote css)
        ;; Export with LaTeX fragments
        org-export-with-LaTeX-fragments t
        ;; Increase default number of headings to export
        org-export-headline-levels 6
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
        org-agenda-cmp-user-defined 'bh/agenda-sort
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
                                  ("h" . bh/hide-other)
                                  ("i" progn
                                   (forward-char 1)
                                   (call-interactively 'org-insert-heading-respect-content))
                                  ("k" . org-kill-note-or-show-branches)
                                  ("l" . ignore)
                                  ("m" . ignore)
                                  ("q" . bh/show-org-agenda)
                                  ("r" . ignore)
                                  ("s" . org-save-all-org-buffers)
                                  ("w" . org-refile)
                                  ("x" . ignore)
                                  ("y" . ignore)
                                  ("z" . org-add-note)

                                  ("A" . ignore)
                                  ("B" . ignore)
                                  ("E" . ignore)
                                  ("F" . bh/restrict-to-file-or-follow)
                                  ("G" . ignore)
                                  ("H" . ignore)
                                  ("J" . org-clock-goto)
                                  ("K" . ignore)
                                  ("L" . ignore)
                                  ("M" . ignore)
                                  ("N" . bh/narrow-to-org-subtree)
                                  ("P" . bh/narrow-to-org-project)
                                  ("Q" . ignore)
                                  ("R" . ignore)
                                  ("S" . ignore)
                                  ("T" . bh/org-todo)
                                  ("U" . bh/narrow-up-one-org-level)
                                  ("V" . ignore)
                                  ("W" . bh/widen)
                                  ("X" . ignore)
                                  ("Y" . ignore)
                                  ("Z" . ignore))
        )

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  (defun bh/hide-other ()
    (interactive)
    (save-excursion
      (org-back-to-heading 'invisible-ok)
      (hide-other)
      (org-cycle)
      (org-cycle)
      (org-cycle)))

  (defun bh/display-inline-images ()
    (condition-case nil
        (org-display-inline-images)
      (error nil)))

  ;; Add languages
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (ditaa . t)
                                 (R . t)
                                 ;; (ipython . t)
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
                                 (go . t)
                                 (latex . t)))

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  ;;(setq org-confirm-babel-evaluate nil)

  ;; Use fundamental mode when editing plantuml blocks with C-c '
  (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

  ;; I'm lazy and don't want to remember the name of the project to publish when I modify
  ;; a file that is part of a project.  So this function saves the file, and publishes
  ;; the project that includes this file
  ;;
  ;; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
  (defun bh/save-then-publish (&optional force)
    (interactive "P")
    (save-buffer)
    (org-save-all-org-buffers)
    (let ((org-html-head-extra)
          (org-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"))
      (org-publish-current-project force)))

  ;; Skeletons
  ;;
  ;; sblk - Generic block #+begin_FOO .. #+end_FOO
  (define-skeleton skel-org-block
    "Insert an org block, querying for type."
    "Type: "
    "#+begin_" str "\n"
    _ - \n
    "#+end_" str "\n")

  (define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

  ;; splantuml - PlantUML Source block
  (define-skeleton skel-org-block-plantuml
    "Insert a org plantuml block, querying for filename."
    "File (no extension): "
    "#+begin_src plantuml :file " str ".png :cache yes\n"
    _ - \n
    "#+end_src\n")

  (define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

  (define-skeleton skel-org-block-plantuml-activity
    "Insert a org plantuml block, querying for filename."
    "File (no extension): "
    "#+begin_src plantuml :file " str "-act.png :cache yes :tangle " str "-act.txt\n"
    (bh/plantuml-reset-counters)
    "@startuml\n"
    "skinparam activity {\n"
    "BackgroundColor<<New>> Cyan\n"
    "}\n\n"
    "title " str " - \n"
    "note left: " str "\n"
    "(*) --> \"" str "\"\n"
    "--> (*)\n"
    _ - \n
    "@enduml\n"
    "#+end_src\n")

  (defvar bh/plantuml-if-count 0)

  (defun bh/plantuml-if ()
    (incf bh/plantuml-if-count)
    (number-to-string bh/plantuml-if-count))

  (defvar bh/plantuml-loop-count 0)

  (defun bh/plantuml-loop ()
    (incf bh/plantuml-loop-count)
    (number-to-string bh/plantuml-loop-count))

  (defun bh/plantuml-reset-counters ()
    (setq bh/plantuml-if-count 0
          bh/plantuml-loop-count 0)
    "")

  (define-abbrev org-mode-abbrev-table "sact" "" 'skel-org-block-plantuml-activity)

  (define-skeleton skel-org-block-plantuml-activity-if
    "Insert a org plantuml block activity if statement"
    ""
    "if \"\" then\n"
    "  -> [condition] ==IF" (setq ifn (bh/plantuml-if)) "==\n"
    "  --> ==IF" ifn "M1==\n"
    "  -left-> ==IF" ifn "M2==\n"
    "else\n"
    "end if\n"
    "--> ==IF" ifn "M2==")

  (define-abbrev org-mode-abbrev-table "sif" "" 'skel-org-block-plantuml-activity-if)

  (define-skeleton skel-org-block-plantuml-activity-for
    "Insert a org plantuml block activity for statement"
    "Loop for each: "
    "--> ==LOOP" (setq loopn (bh/plantuml-loop)) "==\n"
    "note left: Loop" loopn ": For each " str "\n"
    "--> ==ENDLOOP" loopn "==\n"
    "note left: Loop" loopn ": End for each " str "\n" )

  (define-abbrev org-mode-abbrev-table "sfor" "" 'skel-org-block-plantuml-activity-for)

  (define-skeleton skel-org-block-plantuml-sequence
    "Insert a org plantuml activity diagram block, querying for filename."
    "File appends (no extension): "
    "#+begin_src plantuml :file " str "-seq.png :cache yes :tangle " str "-seq.txt\n"
    "@startuml\n"
    "title " str " - \n"
    "actor CSR as \"Customer Service Representative\"\n"
    "participant CSMO as \"CSM Online\"\n"
    "participant CSMU as \"CSM Unix\"\n"
    "participant NRIS\n"
    "actor Customer"
    _ - \n
    "@enduml\n"
    "#+end_src\n")

  (define-abbrev org-mode-abbrev-table "sseq" "" 'skel-org-block-plantuml-sequence)

  ;; sdot - Graphviz DOT block
  (define-skeleton skel-org-block-dot
    "Insert a org graphviz dot block, querying for filename."
    "File (no extension): "
    "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
    "graph G {\n"
    _ - \n
    "}\n"
    "#+end_src\n")

  (define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

  ;; sditaa - Ditaa source block
  (define-skeleton skel-org-block-ditaa
    "Insert a org ditaa block, querying for filename."
    "File (no extension): "
    "#+begin_src ditaa :file " str ".png :cache yes\n"
    _ - \n
    "#+end_src\n")

  (define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

  ;; selisp - Emacs Lisp source block
  (define-skeleton skel-org-block-elisp
    "Insert a org emacs-lisp block"
    ""
    "#+begin_src emacs-lisp\n"
    _ - \n
    "#+end_src\n")

  (define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

  ;; subtree narrowing
  (defun bh/org-todo (arg)
    (interactive "p")
    (if (equal arg 4)
        (save-restriction
          (bh/narrow-to-org-subtree)
          (org-show-todo-tree nil))
      (bh/narrow-to-org-subtree)
      (org-show-todo-tree nil)))

  (global-set-key (kbd "<S-f5>") 'bh/widen)

  (defun bh/widen ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-agenda-remove-restriction-lock)
          (when org-agenda-sticky
            (org-agenda-redo)))
      (widen)))

  (defun bh/restrict-to-file-or-follow (arg)
    "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
    (interactive "p")
    (if (equal arg 4)
        (org-agenda-follow-mode)
      (widen)
      (bh/set-agenda-restriction-lock 4)
      (org-agenda-redo)
      (beginning-of-buffer)))

  (defun bh/narrow-to-org-subtree ()
    (widen)
    (org-narrow-to-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock)))

  (defun bh/narrow-to-subtree ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (org-get-at-bol 'org-hd-marker)
            (bh/narrow-to-org-subtree))
          (when org-agenda-sticky
            (org-agenda-redo)))
      (bh/narrow-to-org-subtree)))

  (defun bh/narrow-up-one-org-level ()
    (widen)
    (save-excursion
      (outline-up-heading 1 'invisible-ok)
      (bh/narrow-to-org-subtree)))

  (defun bh/get-pom-from-agenda-restriction-or-point ()
    (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
        (org-get-at-bol 'org-hd-marker)
        (and (equal major-mode 'org-mode) (point))
        org-clock-marker))

  (defun bh/narrow-up-one-level ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
            (bh/narrow-up-one-org-level))
          (org-agenda-redo))
      (bh/narrow-up-one-org-level)))

  (defun bh/narrow-to-org-project ()
    (widen)
    (save-excursion
      (bh/find-project-task)
      (bh/narrow-to-org-subtree)))

  (defun bh/narrow-to-project ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
            (bh/narrow-to-org-project)
            (save-excursion
              (bh/find-project-task)
              (org-agenda-set-restriction-lock)))
          (org-agenda-redo)
          (beginning-of-buffer))
      (bh/narrow-to-org-project)
      (save-restriction
        (org-agenda-set-restriction-lock))))

  (defvar bh/project-list nil)

  (defun bh/view-next-project ()
    (interactive)
    (let (num-project-left current-project)
      (unless (marker-position org-agenda-restrict-begin)
        (goto-char (point-min))
                                        ; Clear all of the existing markers on the list
        (while bh/project-list
          (set-marker (pop bh/project-list) nil))
        (re-search-forward "Tasks to Refile")
        (forward-visible-line 1))

                                        ; Build a new project marker list
      (unless bh/project-list
        (while (< (point) (point-max))
          (while (and (< (point) (point-max))
                      (or (not (org-get-at-bol 'org-hd-marker))
                          (org-with-point-at (org-get-at-bol 'org-hd-marker)
                            (or (not (bh/is-project-p))
                                (bh/is-project-subtree-p)))))
            (forward-visible-line 1))
          (when (< (point) (point-max))
            (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
          (forward-visible-line 1)))

                                        ; Pop off the first marker on the list and display
      (setq current-project (pop bh/project-list))
      (when current-project
        (org-with-point-at current-project
          (setq bh/hide-scheduled-and-waiting-next-tasks nil)
          (bh/narrow-to-project))
                                        ; Remove the marker
        (setq current-project nil)
        (org-agenda-redo)
        (beginning-of-buffer)
        (setq num-projects-left (length bh/project-list))
        (if (> num-projects-left 0)
            (message "%s projects left to view" num-projects-left)
          (beginning-of-buffer)
          (setq bh/hide-scheduled-and-waiting-next-tasks t)
          (error "All projects viewed.")))))

  (defun bh/set-agenda-restriction-lock (arg)
    "Set restriction lock to current task subtree or file if prefix is specified"
    (interactive "p")
    (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
           (tags (org-with-point-at pom (org-get-tags-at))))
      (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
        (save-restriction
          (cond
           ((and (equal major-mode 'org-agenda-mode) pom)
            (org-with-point-at pom
              (org-agenda-set-restriction-lock restriction-type))
            (org-agenda-redo))
           ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
            (org-agenda-set-restriction-lock 'file))
           (pom
            (org-with-point-at pom
              (org-agenda-set-restriction-lock restriction-type))))))))

  (defun bh/agenda-sort (a b)
    "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
    (let (result num-a num-b)
      (cond
                                        ; time specific items are already sorted first by org-agenda-sorting-strategy

                                        ; non-deadline and non-scheduled items next
       ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

                                        ; deadlines for today next
       ((bh/agenda-sort-test 'bh/is-due-deadline a b))

                                        ; late deadlines next
       ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

                                        ; scheduled items for today next
       ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

                                        ; late scheduled items next
       ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

                                        ; pending deadlines last
       ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

                                        ; finally default to unsorted
       (t (setq result nil)))
      result))

  (defmacro bh/agenda-sort-test (fn a b)
    "Test for agenda sort"
    `(cond
                                        ; if both match leave them unsorted
      ((and (apply ,fn (list ,a))
            (apply ,fn (list ,b)))
       (setq result nil))
                                        ; if a matches put a first
      ((apply ,fn (list ,a))
       (setq result -1))
                                        ; otherwise if b matches put b first
      ((apply ,fn (list ,b))
       (setq result 1))
                                        ; if none match leave them unsorted
      (t nil)))

  (defmacro bh/agenda-sort-test-num (fn compfn a b)
    `(cond
      ((apply ,fn (list ,a))
       (setq num-a (string-to-number (match-string 1 ,a)))
       (if (apply ,fn (list ,b))
           (progn
             (setq num-b (string-to-number (match-string 1 ,b)))
             (setq result (if (apply ,compfn (list num-a num-b))
                              -1
                            1)))
         (setq result -1)))
      ((apply ,fn (list ,b))
       (setq result 1))
      (t nil)))

  (defun bh/is-not-scheduled-or-deadline (date-str)
    (and (not (bh/is-deadline date-str))
         (not (bh/is-scheduled date-str))))

  (defun bh/is-due-deadline (date-str)
    (string-match "Deadline:" date-str))

  (defun bh/is-late-deadline (date-str)
    (string-match "\\([0-9]*\\) d\. ago:" date-str))

  (defun bh/is-pending-deadline (date-str)
    (string-match "In \\([^-]*\\)d\.:" date-str))

  (defun bh/is-deadline (date-str)
    (or (bh/is-due-deadline date-str)
        (bh/is-late-deadline date-str)
        (bh/is-pending-deadline date-str)))

  (defun bh/is-scheduled (date-str)
    (or (bh/is-scheduled-today date-str)
        (bh/is-scheduled-late date-str)))

  (defun bh/is-scheduled-today (date-str)
    (string-match "Scheduled:" date-str))

  (defun bh/is-scheduled-late (date-str)
    (string-match "Sched\.\\(.*\\)x:" date-str))

  (defun bh/show-org-agenda ()
    (interactive)
    (if org-agenda-sticky
        (switch-to-buffer "*Org Agenda( )*")
      (switch-to-buffer "*Org Agenda*"))
    (delete-other-windows))

  (defun bh/prepare-meeting-notes ()
    "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
    (interactive)
    (let (prefix)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (untabify (point-min) (point-max))
          (goto-char (point-min))
          (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
            (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
          (goto-char (point-min))
          (kill-ring-save (point-min) (point-max))))))

  :init
  (require 'org-id)
  )


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
       :ensure t)
     (require 'ox-beamer)
     (require 'ox-html)
     (require 'ox-latex)
     (require 'ox-ascii)
     (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
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
    (setq org-reveal-root "file:///Users/v236177/Dropbox/dev/reveal-js-local"))
;;(setq org-reveal-mathjax t)

;;Org-export to LaTeX
(eval-after-load "ox-latex"
  '(progn
     (message "Now loading org-latex export settings")
     ;; page break after toc
     (setq org-latex-toc-command "\\tableofcontents \\clearpage")
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
