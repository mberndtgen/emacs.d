;;; 01-core.el --- Paths, helpers, environment -*- lexical-binding: t; -*-

;;; Code:

(make-directory (locate-user-emacs-file "local") :no-error)

(let ((default-directory (expand-file-name user-emacs-directory)))
  (normal-top-level-add-to-load-path '("lisp" "etc" "elpa/emacs-reveal")))

;; Add every lisp/*.el directory to load-path (legacy layout).
(let* ((path (expand-file-name "lisp" user-emacs-directory))
       (local-pkgs (and (file-accessible-directory-p path)
                        (mapcar #'file-name-directory
                                (directory-files-recursively path "\\.el$")))))
  (when local-pkgs
    (mapc (apply-partially #'add-to-list 'load-path) local-pkgs)))

(setenv "PATH"
        (concat "/usr/bin" path-separator
                "~/.ghcup/bin" path-separator
                "~/.go/bin" path-separator
                (getenv "PATH")))

(when (eq system-type 'darwin)
  (setq exec-path '("/usr/bin" "~/.ghcup/bin" "~/go/bin")))

(setq ns-pop-up-frames nil
      user-full-name "Manfred Berndtgen"
      debug-on-error nil
      byte-compile-warnings '(cl-functions)
      network-security-level 'high
      read-process-output-max (* 1024 1024))

(setq-default tab-width 2
              indent-tabs-mode nil)

(setenv "PAGER" "cat")

(require 'extras)

(defun expose (function &rest args)
  "Return interactive version of FUNCTION, exposing it to the user."
  (lambda ()
    (interactive)
    (apply function args)))

(defun my/load-config-module (name)
  "Load NAME.el from `user-emacs-directory'/lisp/."
  (load (expand-file-name (concat name ".el") (expand-file-name "lisp" user-emacs-directory))
        nil 'nomessage))

(defun my/reload-config-module (module)
  "Reload a config module (e.g. \"10-org\").  Use after editing lisp/MODULE.el."
  (interactive
   (list (completing-read "Reload module: "
                          (mapcar (lambda (f) (file-name-base f))
                                  (directory-files (expand-file-name "lisp" user-emacs-directory)
                                                   nil "^[0-9].*\\.el$")))))
  (let ((file (expand-file-name (concat module ".el")
                                (expand-file-name "lisp" user-emacs-directory))))
    (load file nil 'nomessage 'nomessage)
    (message "Reloaded %s" file)))

(defun find-user-init-file ()
  "Edit `user-init-file' in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(advice-add 'display-startup-echo-area-message :override #'ignore)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq buffer-file-coding-system 'utf-8
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le-dos))

(provide '01-core)
