;;; org-cfg.el --- small extra functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; parts taken from https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org

;;; Code:

(use-package bbdb
  :ensure t
  :bind (("<f9> p" . bh/phone-call)
         ))

;; (use-package bbdb-com
;;   :ensure t)

(use-package org
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

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

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
        org-cycle-separator-lines 2
        org-deadline-warning-days 30

        org-ellipsis "…" ;;; replace the "..." with "…" for collapsed org-mode content
        org-export-with-smart-quotes t
        org-enforce-todo-dependencies t
        org-hide-emphasis-markers t
        org-html-table-default-attributes '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none")
        org-id-method 'uuidgen
        org-image-actual-width nil ;;; See https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
        org-insert-heading-respect-content nil
        org-log-done t
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
        org-reverse-note-order nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'other-window
        org-show-following-heading t
        org-show-hierarchy-above t
        org-show-siblings '((default))
        org-startup-align-all-tables t ;;; Can be set per file basis with: #+STARTUP: noalign (or align). Same as doing C-c C-c in a table.
        org-startup-indented t
        org-startup-truncated t
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
        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archived Tasks"
        org-alphabetical-lists t
        )

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
  (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
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

  :init
  (require 'org-id)
  )

;; Add languages
;;(use-package ob-ipython
;;  :ensure t)

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
(setq org-babel-default-header-args '((:eval . "never-export")))

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
     (use-package helm-org
       :ensure t
       :config
       (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
       (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))))

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
