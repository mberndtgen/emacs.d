;;; 41-lisp-elisp.el -*- lexical-binding: t; -*-

;;; Code:

(bind-key
 "C-c e"
 (defhydra hydra-elisp-cmds (:color blue)
   ("b" eval-buffer "eval buffer")
   ("e" toggle-debug-on-error "debug-on-error")
   ("f" emacs-lisp-byte-compile-and-load "byte-compile-and-load")
   ("r" eval-region "eval-region")
   ("q" nil))
 emacs-lisp-mode-map)

(bind-key
 "C-h e"
 (defhydra hydra-elisp-help (:color blue)
   ("e" view-echo-area-messages "view-echod-area-messages")
   ("f" find-function "find-function")
   ("k" find-function-on-key "find-function-on-key")
   ("l" find-library "find-library")
   ("v" find-variable "find-variable")
   ("V" apropos-value "apropos-value")
   ("i" info-display-manual "info-display-manual")
   ("q" nil))
 emacs-lisp-mode-map)
(use-package font-lock-profiler
  :commands (font-lock-profiler-start font-lock-profiler-buffer font-lock-profiler-region))
(provide '41-lisp-elisp)
