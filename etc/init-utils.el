(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;; exit behaviour
;; variable bnb/kill-emacs-hooks for functions that need to run before emacs is killed.
(defvar bnb/kill-emacs-hooks)
(add-hook 'bnb/kill-emacs-hooks
          (lambda () (if (functionp 'server-edit)(server-edit))))

(defvar bnb/really-kill-emacs nil)
(defun bnb/kill-emacs ()
  "Actually kill emacs."
  (interactive)
  (setq bnb/really-kill-emacs t)
  (kill-emacs))

(defadvice kill-emacs (around bnb/pardon-emacs activate)
  "Only kill emacs if a prefix is set"
  (run-hooks 'bnb/kill-emacs-hooks)
  (if bnb/really-kill-emacs
      ad-do-it
    (iconify-frame)))

;; better window splitting functions
(defun bnb/vplit-last-buffer ()
  "When splitting the frame, load the last visited buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun bnb/hsplit-last-buffer ()
  "When splitting the frame, load the last visited buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(bind-keys
 ("C-x 2" . bnb/vplit-last-buffer)
 ("C-x 3" . bnb/hsplit-last-buffer))

;; auto-indent when pasting
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; ansi term
(defadvice term-sentinel (around bnb/advise-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-zsh)
  (interactive (list "/usr/local/bin/zsh")))
(ad-activate 'ansi-term)

(defun bnb/term-mode-hook ()
  "Setup `term-mode`."
  (goto-address-mode)
  (setq-local term-buffer-maximum-size 10000))

(add-hook 'term-mode-hook 'bnb/term-mode-hook)

(defalias 'zsh 'ansi-term)

(setq ansi-term-color-vector
      [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white]
      ansi-color-faces-vector
      [default bold shadow italic underline bold bold-italic bold])

(provide 'init-utils)
