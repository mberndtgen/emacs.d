;;; 04-keys.el -*- lexical-binding: t; -*-

;;; Code:

(if (eq system-type 'darwin)
    (setq mac-function-modifier 'hyper
          mac-right-option-modifier 'super
          mac-right-command-modifier 'meta
          mac-right-control-modifier 'ctrl
          mac-pass-command-to-system nil
          mac-command-modifier 'meta    ; make opt key do Super
          mac-control-modifier 'ctrl    ; make Control key do Control
          mac-right-option-modifier 'super
          ns-function-modifier 'hyper))
(if (eq system-type 'gnu/linux)
    nil)
(if (eq system-type 'windows-nt)
    nil)

;;
;; keybindings
;;

;; shift <cursor> now just select text, super <cursor> moves between windows
(windmove-default-keybindings 'super)

;; Set mark with H-SPC (Fn+Space on this Mac), then move cursor to extend region.
(transient-mark-mode 1)
(global-set-key (kbd "H-SPC") #'set-mark-command)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "H-SPC") #'set-mark-command)
  (define-key org-agenda-mode-map (kbd "H-SPC") #'set-mark-command))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-j") #'join-line)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-c I") #'find-user-init-file)

;; Home Keys Linux/Windows style
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo) ; Mac-style redo
;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.ino$" . arduino-mode))

(defun bnb/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(bind-keys ("C-+" . text-scale-increase)
           ("C--" . text-scale-decrease)
           ("C-x C-k" . bnb/kill-this-buffer)
           ("M-k" . fixup-whitespace)
           ("C-c TAB" . align-regexp)
           ("H-C-s" . switch-to-scratch-buffer))

(bind-key "M-/" 'hippie-expand)

;; shortcut for editing init.el - now crux
(bind-key "<f4>" (lambda ()
                   (interactive)
                   (find-file "~/.emacs.d/init.el")))
(provide '04-keys)
