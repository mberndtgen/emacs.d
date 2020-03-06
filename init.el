;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Use this at the top of your .emacs file for local overrides:
;;     (let ((init "~/.emacs.d/init.elc"))
;;       (if (file-exists-p init)
;;           (load-file init)
;;         (load-file (substring init 0 -1))))

;;; Code:

(make-directory (locate-user-emacs-file "local") :no-error)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/etc")

;; Set up package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cljr-helm . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
(setq package-archive-priorities '(("gnu" . 5)
                                   ("marmalade" . 4)
                                   ("org" . 3)
                                   ("melpa" . 2)
                                   ("melpa-stable" . 1)))

(package-initialize)


(defvar init.el-errors '()
  "A list of errors that occured during initialization. Each error is of the form (LINE ERROR &rest ARGS).")

(defvar init.el-line 0
  "Approximation to the currently executed line in this file.")


(setq package-enable-at-startup nil
      ;; work around package.el bug in Emacs 25
      package--init-file-ensured t)

;; prevent opening a new frame when loading a file
(setq ns-pop-up-frames nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  ;;(require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))

(when (eq system-type 'darwin)
  (when (not (package-installed-p 'dash-at-point))
    (package-install 'dash-at-point))
  ;; dash-at-point
  (autoload 'dash-at-point "dash-at-point" "Search the word at point with Dash." t nil)
  (global-set-key "\C-cd" 'dash-at-point))

;; Load local "packages"
(require 'init-utils)
(require 'unannoy)
(require 'extras)

;; Define `expose' since it's used everywhere.
(defun expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda ()
    (interactive)
    (apply function args)))

;; Some global keybindings

(if (eq system-type 'darwin)
    (progn
      ;; set keys for Apple keyboard, for emacs in OS X
      (setq mac-command-modifier 'meta) ; make cmd key do Meta
      (setq mac-option-modifier 'super) ; make opt key do Super
      (setq mac-control-modifier 'control) ; make Control key do Control
      (setq ns-function-modifier 'hyper)))  ; make Fn key do Hyper
(if (eq system-type 'gnu/linux)
    nil)
(if (eq system-type 'windows-nt)
    nil)

(global-set-key (kbd "C-j") #'join-line)
(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "C-x C-k") #'compile)
(global-set-key (kbd "<f5>") (expose #'revert-buffer nil t))
(global-set-key (kbd "C-=") #'calc)
(global-set-key (kbd "C-x k") #'kill-this-buffer)

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

(setq network-security-level 'high)

;; Frames and fonts

(add-to-list 'default-frame-alist
             '(font . "Fira Code-12"))

(defun my-set-frame-fullscreen (&optional frame)
  (set-frame-parameter frame 'fullscreen 'fullheight))

(defun insert-backs ()
  "insert back-slash"
  (interactive)
  (insert "\\"))

;;; convenience settings

(global-linum-mode 1)
(column-number-mode 1)
(setq-default comment-column 70) ; Set the default comment column to 70
;;; S - shift key
;;; M - Cmd key
;;; C - Ctrl key
;;; s - Option key
;;(global-set-key (kbd "C-c s") 'slime-selector)
(global-set-key (kbd "H-#") 'insert-backs)
(global-set-key (kbd "H-ü") "|")
(global-set-key (kbd "H-2") "@")
(global-set-key (kbd "H-ö") "[")
(global-set-key (kbd "H-ä") "]")
(global-set-key (kbd "H-p") "{")
(global-set-key (kbd "H-+") "}")
(global-set-key (kbd "H-<") "~")

(custom-set-variables
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes (quote ("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(fci-rule-color "#2a2a2a")
 '(scroll-preserve-screen-position t)
 '(which-key-mode t))

(electric-indent-mode +1) ;; indent after entering RET
(electric-pair-mode +1) ;; automatically add a closing paren
(desktop-save-mode 1) ;; save sessions
;;; display file path in frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; make eww default browser
(setq browse-url-browser-function 'eww-browse-url)


(defun find-user-init-file () ;; instant access to init file
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

(advice-add 'display-startup-echo-area-message
            :override #'ignore)

;; starting and terminating
;; confirm termination unless running in daemon mode
(if (daemonp)
    nil
  (setq confirm-kill-emacs 'yes-or-no-p))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun signal-restart-server ()
  "Handler for SIGUSR1 signal, to (re)start an emacs server.

Can be tested from within emacs with:
  (signal-process (emacs-pid) 'sigusr1)

or from the command line with:
$ kill -USR1 <emacs-pid>
$ emacsclient -c
"
  (interactive)
  (server-force-delete)
  (server-start))

(define-key special-event-map [sigusr1] 'signal-restart-server)


;;; Individual package configurations

;; emacs::pde
;; (add-to-list 'load-path "~/Documents/src/git/elisp/emacs-pde/lisp/")
;; (load "pde-load")

;; see https://github.com/bbatsov/crux
(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  (progn
    ;; use flycheck, not flymake
    ;; (setq lsp-prefer-flymake nil)
    ;; (setq lsp-print-performance nil)
    ;;(setq lsp-log-io nil)
    ))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config (progn
            ;; disable inline documentation
            ;; (setq lsp-ui-sideline-enable nil)
            ;; disable showing docs on hover at the top of the window
            ;; (setq lsp-ui-doc-enable nil)
	        (setq lsp-ui-imenu-enable t)
	        (setq lsp-ui-imenu-kind-position 'top))
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package use-package-hydra
  :ensure t)

;; go hydra
(use-package hydra
  :ensure t
  :config
  (require 'hydra)
  (require 'dap-mode)
  (require 'dap-ui)
  ;;:commands (ace-flyspell-setup)
  :bind
  ;;("M-s" . hydra-go/body)
  :init
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'hydra-go/body)))
  :hydra (hydra-go (:color pink :hint nil :foreign-keys run)
  "
   _n_: Next       _c_: Continue _g_: goroutines      _i_: break log
   _s_: Step in    _o_: Step out _k_: break condition _h_: break hit condition
   _Q_: Disconnect _q_: quit     _l_: locals
   "
	     ("n" dap-next)
	     ("c" dap-continue)
	     ("s" dap-step-in)
	     ("o" dap-step-out)
	     ("g" dap-ui-sessions)
	     ("l" dap-ui-locals)
	     ("e" dap-eval-thing-at-point)
	     ("h" dap-breakpoint-hit-condition)
	     ("k" dap-breakpoint-condition)
	     ("i" dap-breakpoint-log-message)
	     ("q" nil "quit" :color blue)
	     ("Q" dap-disconnect :color red)))

;; DAP
(use-package dap-mode
  :diminish
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-stopped . (lambda (&_rest) (call-interactively #'dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
         (go-mode . (lambda() (require 'dap-go))))
  :config
  ;;(dap-mode t)
  (setq dap-print-io t)
  (setq dap-go-delve-path (expand-file-name "dlv" (expand-file-name "bin" (getenv "GOPATH"))))
  ;; (require 'dap-hydra)
  ;;(require 'dap-go)
  ;;(dap-go-setup)
  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode 1)))

;;(add-hook 'dap-stopped-hook
;;          (lambda (arg) (call-interactively #'dap-hydra)))

(use-package lsp-treemacs
  :config
  ;;(lsp-metals-treeview-enable t) ; scala metals support
  ;;(setq lsp-metals-treeview-show-when-views-received t)
  )

(use-package company
  :ensure t
  :init
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-dabbrev-ignore-case nil
          company-dabbrev-code-ignore-case nil
          company-dabbrev-downcase nil
          company-idle-delay 0
          company-minimum-prefix-length 2
          company-begin-commands '(self-insert-command)
          company-transformers '(company-sort-by-occurrence)
          company-tooltip-align-annotations t)))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  ;; :custom
  ;; (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  ;; (company-lsp-async t)
  ;; (company-lsp-enable-snippet t)
  ;; (company-lsp-enable-recompletion t)
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package undo-tree
  :defer t
  :init (setf global-undo-tree-mode 1))

(use-package buffer-move
  :defer t)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))  ; set rainbow-delimiters mode for most programming modes

;; (use-package powerline
;;   :ensure t
;;   :init (progn
;;           (powerline-default-theme)
;;           (setq powerline-height 23))
;;   :config (progn
;;             (require 'powerline)
;;             (add-hook 'desktop-after-read-hook 'powerline-reset)
;;             (defface modes-ml-face '((t (:background "#002b36" :inherit mode-line)))
;;               "Powerline face for modes section of the mode-line"
;;               :group 'powerline)
;;             (defface file-ml-face '((t (:background "#586e75" :inherit mode-line)))
;;               "Powerline face for file and branch section of the mode-line"
;;               :group 'powerline)
;;             (defface line-ml-face '((t (:background "#93a1a1" :inherit mode-line)))
;;               "Powerline face for line number section of the mode-line"
;;               :group 'powerline)
;;             (defface pos-ml-face '((t (:background "#586e75" :inherit mode-line)))
;;               "Powerline face for file position section of the mode-line"
;;               :group 'powerline)
;;             (defface ml-fill-face '((t (:background "#93a1a1" :inherit mode-line)))
;;               "Powerline face used to fill the unused portion of the mode-line"
;;               :group 'powerline)))


(use-package highlight-symbol
  :ensure t
  :bind (("<C-f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("<S-f3>" . highlight-symbol-prev)
         ("<M-f3>" . highlight-symbol-query-replace)))

(use-package smooth-scrolling
  :ensure t)

(use-package window-number
  :ensure t
  :init
  (progn
    (autoload 'window-number-mode "window-number"
      "A global minor mode that enables selection of windows according to
 numbers with the C-x C-j prefix.  Another mode,
 `window-number-meta-mode' enables the use of the M- prefix."
      t)
    (autoload 'window-number-meta-mode "window-number"
      "A global minor mode that enables use of the M- prefix to select
 windows, use `window-number-mode' to display the window numbers in
 the mode-line."
      t)
    (window-number-mode 1)
    (window-number-meta-mode 1)))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (define-key projectile-mode-map (kbd "s-d") 'projectile-find-dir)
  (define-key projectile-mode-map (kbd "s-e") 'er/expand-region)
  (define-key projectile-mode-map (kbd "s-f") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-g") 'projectile-grep)
  (define-key projectile-mode-map (kbd "s-j") 'prelude-top-join-line)
  (define-key projectile-mode-map (kbd "s-k") 'prelude-kill-whole-line)
  (define-key projectile-mode-map (kbd "s-l") 'goto-line)
  (define-key projectile-mode-map (kbd "s-m") 'magit-status)
  (define-key projectile-mode-map (kbd "s-o") 'prelude-open-line-above)
  (define-key projectile-mode-map (kbd "s-w") 'delete-frame)
  (define-key projectile-mode-map (kbd "s-x") 'exchange-point-and-mark)
  (projectile-mode +1))

(use-package centaur-tabs
  :demand
  ;;:hook
  ;;(dired-mode . centaur-tabs-local-mode)
  ;;(dashboard-mode . centaur-tabs-local-mode)
  ;;(term-mode . centaur-tabs-local-mode)
  ;;(calendar-mode . centaur-tabs-local-mode)
  ;;(org-agenda-mode . centaur-tabs-local-mode)
  ;;(helpful-mode . centaur-tabs-local-mode)
  :config
  (setq centaur-tabs-style "box"
        centaur-tabs-height 32
        centaur-tabs-set-icons nil
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'over
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*")
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "arial" 120)
  (centaur-tabs-mode t)
  :bind
  ;;("C-<prior>" . centaur-tabs-backward)
  ;;("C-<next>" . centaur-tabs-forward)
  ;;("C-c t s" . centaur-tabs-counsel-switch-group)
  ;; ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ;; ("C-c t g" . centaur-tabs-group-buffer-groups)
  )  

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    ;; From https://gist.github.com/antifuchs/9238468
    ;;(define-key helm-map (kbd "<left>") 'helm-previous-source)
    ;;(define-key helm-map (kbd "<right>") 'helm-next-source)
    ;;helm-ff-lynx-style-map t
    ;;helm-imenu-lynx-style-map t

    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't)
          helm-candidate-number-limit 100
          helm-input-idle-delay 0.01    ; this actually updates things reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-autoresize-mode t
          helm-M-x-fuzzy-match t
          helm-ff-skip-boring-files t)
    (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
    (helm-mode 1)
    (when (executable-find "ack-grep")
      (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
            helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages))
  :bind (("C-c h" . helm-mini)
	 ("C-h a" . helm-apropos)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x b" . helm-buffers-list)
	 ("M-y" . helm-show-kill-ring)
	 ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x c o" . helm-occur)
	 ("C-x c s" . helm-swoop)
	 ("C-x c y" . helm-yas-complete)
	 ("C-x c Y" . helm-yas-create-snippet-on-region)
	 ("C-x c b" . my/helm-do-grep-book-notes)
	 ("C-x c SPC" . helm-all-mark-rings)))


(use-package counsel)

(use-package swiper
  :bind*
  (("C-s" . swiper))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
    (ivy-set-actions
     'counsel-find-file
     '(("d" (lambda (x) (delete-file (expand-file-name x)))
        "delete")))
    (ivy-set-actions
     'ivy-switch-buffer
     '(("k"
        (Lambda (x)
                (kill-buffer x)
                (ivy--reset-state ivy-last))
        "kill")
       ("j"
        ivy--switch-buffer-other-window-action
        "other window")))))

;; (use-package expand-region
;;   :bind (("C-=" . er/expand-region)))

(use-package buffer-move
  :ensure t
  :bind (("<M-S-up>" . buf-move-up)
         ("<M-S-down>" . buf-move-down)
         ("<M-S-left>" . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

;;; Save point position between sessions
(use-package saveplace
  :ensure t
  :init (setq-default save-place t)
  :config (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package flycheck
  :diminish
  :defer 2
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3))


(use-package auto-complete-config
  :ensure auto-complete
  :bind ("M-<tab>" . my--auto-complete)
  :init
  (defun my--auto-complete ()
    (interactive)
    (unless (boundp 'auto-complete-mode)
      (global-auto-complete-mode 1))
    (auto-complete)))

;; (require 'slime-cfg) ; --deprecated
(require 'sly-cfg)
(require 'clojure-cfg)
(require 'haskell-cfg)
(require 'python-cfg)
;;(require 'perl6-cfg)
(require 'golang-cfg)
;;(require 'javascript-cfg)
(require 'other-languages)
(require 'org-cfg)
(require 'misc)


;;; which-key for help with key bindings - https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; multiple cursors - https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(use-package recentf
  :ensure t
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 25)
    (global-set-key (kbd "C-x C-r") 'recentf-open-files)))

(use-package dabbrev
  :defer t
  :init (setf abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$" "\\.markdown$" "vimperator-.+\\.tmp$")
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (remove-hook 'fill-nobreak-predicate
                           'markdown-inside-link-p t)))
  (setf sentence-end-double-space nil
        markdown-indent-on-enter nil
        markdown-command
        "pandoc -f markdown -t html5 -s --self-contained --smart"))

(use-package impatient-mode
  :defer t
  :config
  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))
  (push (cons 'markdown-mode #'imp-markdown-filter)
        imp-default-user-filters))

(use-package dired
  :defer t
  :config
  (progn
    (add-hook 'dired-mode-hook #'toggle-truncate-lines)
    (setf dired-listing-switches "-alhG"
          dired-guess-shell-alist-user
          '(("\\.pdf\\'" "evince")
            ("\\(\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")
            ("\\(\\.png\\|\\.jpe?g\\)\\'" "qiv")
            ("\\.gif\\'" "animate"))
          dired-dwim-target t
          dired-recursive-copies 'top
          dired-listing-switches "-ahl"
          dired-auto-revert-buffer t
          dired-allow-to-change-permissions 'advanced)))

;; (use-package dired+
;;   :ensure t
;;   :config
;;   (global-dired-hide-details-mode 1))

(use-package dired-narrow
  :ensure t
  :config
  (define-key dired-mode-map (kbd "C-x /") 'dired-narrow))

(use-package notmuch
  :ensure t
  :bind ("C-x m" . notmuch)
  :config
  (progn
    (require 'email-setup)
    (require 'notmuch-address)
    (define-key notmuch-common-keymap "q" (expose #'kill-buffer))
    (define-key notmuch-message-mode-map (kbd "C-x C-s") nil)
    (define-key notmuch-message-mode-map (kbd "C-c C-s") nil) ; super annoying
    (setf notmuch-command "notmuch-remote"
          message-send-mail-function 'smtpmail-send-it
          message-kill-buffer-on-exit t
          smtpmail-smtp-server "localhost"
          smtpmail-smtp-service 2525
          notmuch-address-command "addrlookup-remote"
          notmuch-fcc-dirs nil
          notmuch-search-oldest-first nil
          notmuch-archive-tags '("-inbox" "-unread" "+archive")
          hashcash-path (executable-find "hashcash"))
    (custom-set-faces
     '(notmuch-search-subject ((t :foreground "#afa")))
     '(notmuch-search-date    ((t :foreground "#aaf")))
     '(notmuch-search-count   ((t :foreground "#777"))))
    (setq notmuch-hello-sections
          '(notmuch-hello-insert-header
            notmuch-hello-insert-saved-searches
            notmuch-hello-insert-search))))

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (require 'feed-setup)
  (setf bookmark-default-file (locate-user-emacs-file "local/bookmarks")))

(use-package youtube-dl
  :bind ("C-x y" . youtube-dl-list))

(use-package time
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format t)
    (display-time-mode t)))

(use-package comint
  :defer t
  :config
  (progn
    (define-key comint-mode-map (kbd "<down>") #'comint-next-input)
    (define-key comint-mode-map (kbd "<up>") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-n") #'comint-next-input)
    (define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-r") #'comint-history-isearch-backward)
    (setf comint-prompt-read-only t
          comint-history-isearch t)))

(use-package tramp
  :defer t
  :config
  (setf tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

(use-package whitespace-cleanup-mode
  :ensure t
  :init
  (progn
    (setq-default indent-tabs-mode nil)
    (global-whitespace-cleanup-mode)))

(use-package diff-mode
  :defer t
  :config (add-hook 'diff-mode-hook #'read-only-mode))

;; (use-package leuven-theme
;;   :ensure t
;;   :init
;;   (progn
;;     (load-theme 'leuven t)
;;     (global-hl-line-mode 1)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ghub)

;; remember to run 'M-x all-the-icons-install-fonts'
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-project-detection 'project
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-unicode-fallback nil
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-number-limit 99
        doom-modeline-vcs-max-length 12
        doom-modeline-persp-name t
        doom-modeline-display-default-persp-name nil
        doom-modeline-lsp t
        doom-modeline-github t
        doom-modeline-github-interval (* 30 60)
        doom-modeline-modal-icon t
        doom-modeline-mu4e nil
        doom-modeline-gnus t
        doom-modeline-gnus-timer 2
        doom-modeline-irc t
        doom-modeline-irc-stylize 'identity
        doom-modeline-env-version t
        doom-modeline-env-go-executable "go"
        doom-modeline-env-perl-executable "perl"
        doom-modeline-env-load-string "Check-check"
        doom-modeline-before-update-env-hook nil
        doom-modeline-after-update-env-hook nil)
  (global-hl-line-mode 1))

;; jump anywhere in buffer, see https://github.com/abo-abo/avy
(use-package avy
  :bind* (("C-'" . avy-goto-char)
          ("C-," . avy-goto-char-2)))

(use-package simple
  :defer t
  :config
  (progn
    ;; disable so I don't use it by accident
    (define-key visual-line-mode-map (kbd "M-q") (expose (lambda ())))
    (add-hook 'tabulated-list-mode-hook #'hl-line-mode)))

(use-package uniquify
  :config
  (setf uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package winner
  :config
  (progn
    (winner-mode 1)
    (windmove-default-keybindings)))

(use-package calc
  :defer t
  :config (setf calc-display-trail nil))

(use-package eshell
  :bind ([f1] . eshell-as)
  :init
  (setf eshell-directory-name (locate-user-emacs-file "local/eshell"))
  :config
  (add-hook 'eshell-mode-hook ; Bad, eshell, bad!
            (lambda ()
              (define-key eshell-mode-map (kbd "<f1>") #'quit-window))))

;; magit support, see https://magit.vc/
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init (setf magit-last-seen-setup-instructions "2.1.0")
  :config
  (setf vc-display-status nil
        magit-push-always-verify nil)
  (remove-hook 'git-commit-finish-query-functions
               'git-commit-check-style-conventions)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package gitconfig-mode
  :ensure t
  :defer t
  :config (add-hook 'gitconfig-mode-hook
                    (lambda ()
                      (setf indent-tabs-mode nil
                            tab-width 4))))

(use-package octave
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
  (setf octave-block-offset 4))

(use-package simple-httpd
  :ensure t
  :defer t
  :functions httpd-send-header
  :config
  (progn
    (defservlet uptime "text/plain" ()
      (princ (emacs-uptime)))
    (defun httpd-here ()
      (interactive)
      (setf httpd-root default-directory))
    (defadvice httpd-start (after httpd-query-on-exit-flag activate)
      (let ((httpd-process (get-process "httpd")))
        (when httpd-process
          (set-process-query-on-exit-flag httpd-process nil))))))

(use-package ps-print
  :defer t
  :config (setf ps-print-header nil))

(use-package ielm
  :defer t
  :config
  (progn
    (define-key ielm-map (kbd "C-c C-z") #'quit-window)
    (defadvice ielm-eval-input (after ielm-paredit activate)
      "Begin each ielm prompt with a paredit pair."
      (paredit-open-round))))

(use-package paredit
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'sly-editing-mode #'paredit-mode)
    ;;(add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode))
  :config (define-key paredit-mode-map (kbd "C-j") #'join-line))

(use-package paren
  :config (show-paren-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))
  :config
  (progn
    (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")
    (setf rainbow-delimiters-max-face-count 1)
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground 'unspecified
                        :inherit 'error)
    (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")))

(use-package icomplete
  :init (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<C-tab>" . minibuffer-force-complete)))

(use-package etags
  :defer t
  :config
  (defun etags-build (directory)
    (interactive "DDirectory: ")
    (let* ((results ())
           (head (list directory))
           (tail head))
      (while head
        (dolist (file (directory-files (car head) t nil t))
          (cond ((and (not (string-match "\\.$" file))
                      (not (string-match "\\.\\.$" file))
                      (file-directory-p file))
                 (let ((new-tail (list file)))
                   (setf (cdr tail) new-tail
                         tail new-tail)))
                (or (string-match "\\.[ch]$" file) (string-match "\\.go$" file)
                 (push file results))))
        (pop head))
      (let ((default-directory directory))
        (apply #'call-process "etags" nil nil nil results)))))

(use-package javadoc-lookup
  :ensure t
  :defer t
  :bind ("C-h j" . javadoc-lookup)
  :config
  (ignore-errors
    (setf javadoc-lookup-cache-dir (locate-user-emacs-file "local/javadoc"))))

(use-package gnuplot-mode
  :ensure t
  :defer t)

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :config
  (setf graphviz-dot-indent-width 2
        graphviz-dot-auto-indent-on-semi nil))

(use-package uuid-simple
  :demand t
  :bind ("C-x !" . uuid-insert)
  :config (random (make-uuid)))

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 1))

;; (use-package compile-bind
;;   :demand t
;;   :bind (("C-h g" . compile-bind-set-command)
;;          ("C-h G" . compile-bind-set-root-file))
;;   :config
;;   (progn
;;     (setf compilation-always-kill t
;;           compilation-scroll-output 'first-error
;;           compile-bind-command (format "make -kj%d " (numcores)))
;;     (when (executable-find "nmake.exe")
;;       (compile-bind-set-command "nmake -nologo "))
;;     (compile-bind* (current-global-map)
;;                    ("C-x c" ""
;;                     "C-x t" 'test
;;                     "C-x C" 'clean))))


;; Compile configuration
;;(byte-recompile-directory "~/.emacs.d/lisp/" 0)
;;(byte-recompile-directory "~/.emacs.d/etc/" 0)
;;(byte-recompile-file "~/.emacs.d/init.el" nil 0)


(provide 'init)
;;; init.el ends here
