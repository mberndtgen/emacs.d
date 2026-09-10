;;; 06-editing.el -*- lexical-binding: t; -*-

;;; Code:

(add-hook 'prog-mode-hook
          (lambda() (add-hook 'completion-at-point-functions
                         nil 'local)))
(use-package diminish)

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package delight
  :commands delight)

(use-package 0x0)

(use-package zoom
  :defer t)

(use-package view
  :delight " 👁"
  :init (setq view-read-only t)
  :bind (:map view-mode-map
              ("n" . next-line    )
              ("p" . previous-line)
              ("j" . next-line    )
              ("k" . previous-line)
              ("l" . forward-char)
              ("h" . bnb/view/h)
              ("q" . bnb/view/q))
  :config
  (defun bnb/view/h ()
    "Setup a function to go backwards a character"
    (interactive)
    (forward-char -1))
  (defun bnb/view/q ()
    "Setup a function to quit `view-mode`"
    (interactive)
    (view-mode -1)))
(use-package move-text
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down))
  :config (move-text-default-bindings))
(defvar reb-re-syntax)
(setq reb-re-syntax 'string)
(use-package undo-tree
  :delight "¬"
  :config
  (global-undo-tree-mode)
  (defhydra hydra-undo-tree (:color yellow :hint nil)
    "
    _p_: undo _n_: redo _s_: save _l_: load  "
    ("p" undo-tree-undo)
    ("n" undo-tree-redo)
    ("s" undo-tree-save-history)
    ("l" undo-tree-load-history)
    ("u" undo-tree-visualize "visualize" :color blue)
    ("q" nil "quit" :color blue))
    (setq undo-tree-history-directory-alist 
        '(("." . "~/.emacs.d/undo-tree-history/")))
  (global-set-key (kbd "H-,") 'hydra-undo-tree/body))

(use-package filladapt
  :delight " ▦"
  :defer t
  :commands filladapt-mode
  :init (setq-default filladapt-mode t)
  :hook ((text-mode . filladapt-mode)
         (org-mode . turn-off-filladapt-mode)
         (prog-mode . turn-off-filladapt-mode)))

(use-package buffer-move
  :defer t
  :bind (("<M-S-up>" . buf-move-up)
         ("<M-S-down>" . buf-move-down)
         ("<M-S-left>" . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))
(use-package browse-kill-ring
  :ensure t
  :config
  (global-set-key "\C-cy" 'browse-kill-ring))

;; show vertical lines to guide indentation
;; see https://github.com/zk-phi/indent-guide
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))
(use-package time
  :custom
  (display-time-default-load-average nil)
  (display-time-use-mail-icon t)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))
(use-package winner
  :config
  (winner-mode))
(use-package yascroll
  :config
  (global-yascroll-bar-mode 1))
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; For view-only buffers rendering content, it is useful to have them auto-revert in case of changes.
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'image-mode 'auto-revert-mode)

;; auto-revert buffer
;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(provide '06-editing)
