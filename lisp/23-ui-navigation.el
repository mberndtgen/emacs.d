;;; 23-ui-navigation.el --- Which-key, hydra, ace-*, neotree -*- lexical-binding: t; -*-

;;; Code:

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(defmacro toggle-setting-string (setting)
  `(if (and (boundp ',setting) ,setting) '[x] '[_]))

(bind-key
 "C-x t"
 (defhydra hydra-toggle (:color amaranth)
   "
    _c_ column-number : %(toggle-setting-string column-number-mode)  _b_ orgtbl-mode    : %(toggle-setting-string orgtbl-mode)
    _e_ debug-on-error: %(toggle-setting-string debug-on-error)  _s_ orgstruct-mode : %(toggle-setting-string orgstruct-mode)
    _u_ debug-on-quit : %(toggle-setting-string debug-on-quit)  _h_ diff-hl-mode   : %(toggle-setting-string diff-hl-mode)
    _f_ auto-fill     : %(toggle-setting-string auto-fill-function)  _B_ battery-mode   : %(toggle-setting-string display-battery-mode)
    _t_ truncate-lines: %(toggle-setting-string truncate-lines)  _l_ highlight-line : %(toggle-setting-string hl-line-mode)
    _r_ read-only     : %(toggle-setting-string buffer-read-only)  _n_ line-numbers   : %(toggle-setting-string display-line-numbers-mode)
    _w_ whitespace    : %(toggle-setting-string whitespace-mode)
    "
   ("c" column-number-mode nil)
   ("e" toggle-debug-on-error nil)
   ("u" toggle-debug-on-quit nil)
   ("f" auto-fill-mode nil)
   ("t" toggle-truncate-lines nil)
   ("r" dired-toggle-read-only nil)
   ("w" whitespace-mode nil)
   ("b" orgtbl-mode nil)
   ("s" orgstruct-mode nil)
   ("B" display-battery-mode nil)
   ("h" diff-hl-mode nil)
   ("l" hl-line-mode nil)
   ("n" display-line-numbers-mode nil)
   ("q" nil)))

(use-package pretty-mode
  :hook (org-mode . prettify-symbols-mode)
  :config
  (global-pretty-mode t)
  (pretty-activate-groups '(:sub-and-superscripts :greek :arithmetic-nary)))

(use-package beacon
  :hook (after-init . beacon-mode)
  :custom
  (beacon-push-mark 35)
  (beacon-color "#666600"))

(use-package goto-line-preview)

(with-eval-after-load "goto-line-preview"
  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package highlight-parentheses
  :hook (after-init . highlight-parentheses-mode))

(use-package find-file-in-project
  :bind (("H-x f" . find-file-in-project)
         ("H-x ." . find-file-in-project-at-point)))

(use-package window-purpose
  :ensure t
  :init (purpose-mode))

(use-package ace-flyspell
  :commands (ace-flyspell-setup)
  :bind ("H-f" . hydra-fly/body)
  :init
  (add-hook 'flyspell-mode-hook 'ace-flyspell-setup)
  (defhydra hydra-fly (:color pink)
    ("n" flyspell-goto-next-error "Next error")
    ("c" ispell-word "Correct word")
    ("j" ace-flyspell-jump-word "Jump word")
    ("." ace-flyspell-dwim "dwim")
    ("q" nil "Quit")))

(use-package ace-isearch
  :bind (:map isearch-mode-map ("C-'" . ace-isearch-jump-during-isearch))
  :delight ace-isearch-mode
  :config
  (global-ace-isearch-mode t)
  (setq ace-isearch-input-length 8))

(defvar org-mode-map)

(use-package ace-link
  :bind (:map org-mode-map ("M-o" . ace-link-org))
  :config (ace-link-setup-default))

(use-package ace-window
  :bind ("<f9> a" . ace-window)
  :config
  (setq aw-keys '(?j ?k ?l ?\; ?n ?m)
        aw-leading-char-style 'path
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
          (?c aw-swap-window   "Ace - Swap window")
          (?n aw-flip-window   "Ace - Flip window")
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?b balance-windows)))
  (defhydra hydra-window-size (:color amaranth)
    "Window size"
    ("h" shrink-window-horizontally "shrink horizontal")
    ("j" shrink-window "shrink vertical")
    ("k" enlarge-window "enlarge vertical")
    ("l" enlarge-window-horizontally "enlarge horizontal")
    ("q" nil "quit"))
  (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
  (defhydra hydra-window-frame (:color red)
    "Frame"
    ("f" make-frame "new frame")
    ("x" delete-frame "delete frame")
    ("q" nil "quit"))
  (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t)
  (defhydra hydra-window-scroll (:color amaranth)
    "Scroll other window"
    ("n" scroll-other-window "scroll")
    ("p" scroll-other-window-down "scroll down")
    ("q" nil "quit"))
  (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
  (set-face-attribute 'aw-leading-char-face nil :height 2.0))

(use-package neotree
  :commands (neotree)
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(provide '23-ui-navigation)
