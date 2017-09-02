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
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cljr-helm . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)


(defvar init.el-errors '()
  "A list of errors that occured during initialization. Each error is of the form (LINE ERROR &rest ARGS).")

(defvar init.el-line 0
  "Approximation to the currently executed line in this file.")


;; (defmacro with-buckled-seatbelts (&rest body)
;;   (let ((err (make-symbol "err")))
;;     `(condition-case-unless-debug ,err
;;          ,(macroexp-progn body)
;;        (error
;;         (push (cons init.el-line ,err)
;;               init.el-errors)))))

(setq package-enable-at-startup nil
      ;; work around package.el bug in Emacs 25
      package--init-file-ensured t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

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


;; Frames and fonts

(defvar my-preferred-fonts
  '("-*-AlixFB-normal-normal-normal-*-*-13-*-*-m-0-iso10646-1"
    "InputMonoCondensed Medium-11"
    "Droid Sans Mono-10"
    "Inconsolata-12"))

(defun my-set-preferred-font (&optional frame)
  "Set the first available font from `my-preferred-fonts'."
  (catch 'done
    (with-selected-frame (or frame (selected-frame))
      (dolist (font my-preferred-fonts)
        (when (ignore-errors (x-list-fonts font))
          (set-frame-font font)
          (throw 'done nil)))))
  (add-text-properties (point-min) (point-max) '(line-spacing 0.15 line-height 1.2)))

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

(defun find-user-init-file () ;; instant access to init file
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

(global-set-key  [f8] 'speedbar-get-focus) ;; bind speedbar to f8

(advice-add 'display-startup-echo-area-message
            :override #'ignore)

;; (defun summarize-initialization ()
;;   ;; (kill-buffer "*Messages*") ;; previous messages are spam
;;   (let ((errors (length init.el-errors)))
;;     (if (= 0 errors)
;;         (message "Initialization successful - happy hacking.")
;;       (message "There have been %d errors during init:\n%s"
;;                (length init.el-errors)
;;                (mapconcat (lambda (init.el-error)
;;                             (pcase-let ((`(,line ,err ,rest) init.el-error))
;;                               (format "Lines %d+: %s %s" line err rest)))
;;                           init.el-errors
;;                           "\n")))))

;; (add-hook 'emacs-startup-hook
;;           (lambda () (progn (run-at-time 0.1 nil #'summarize-initialization)
;;                        (my-set-preferred-font))))


;;; Individual package configurations

(use-package company
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company)
  (setf company-idle-delay 0.02)
  (setf company-minimum-prefix-length 1))

(use-package undo-tree
  :defer t
  :init (setf global-undo-tree-mode 1))

(use-package buffer-move
  :defer t)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))  ; set rainbow-delimiters mode for most programming modes

(use-package powerline
  :ensure t
  :init (progn
          (powerline-default-theme)
          (setq powerline-height 23))
  :config (progn
            (require 'powerline)
            (add-hook 'desktop-after-read-hook 'powerline-reset)
            (defface modes-ml-face '((t (:background "#002b36" :inherit mode-line)))
              "Powerline face for modes section of the mode-line"
              :group 'powerline)
            (defface file-ml-face '((t (:background "#586e75" :inherit mode-line)))
              "Powerline face for file and branch section of the mode-line"
              :group 'powerline)
            (defface line-ml-face '((t (:background "#93a1a1" :inherit mode-line)))
              "Powerline face for line number section of the mode-line"
              :group 'powerline)
            (defface pos-ml-face '((t (:background "#586e75" :inherit mode-line)))
              "Powerline face for file position section of the mode-line"
              :group 'powerline)
            (defface ml-fill-face '((t (:background "#93a1a1" :inherit mode-line)))
              "Powerline face used to fill the unused portion of the mode-line"
              :group 'powerline)))

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

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
    (helm-mode))
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


;;; auto-complete - https://github.com/auto-complete/auto-complete
(use-package auto-complete
  :ensure t
  :init
  (require 'auto-complete-config)
  :config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict"))

(require 'slime-cfg)
(require 'clojure-cfg)
(require 'haskell-cfg)
(require 'python-cfg)
;;(require 'golang-cfg)
(require 'other-languages)
(require 'misc)


;;; which-key for help with key bindings - https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; multiple cursors - https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-x c") 'mc/edit-lines)
  (global-set-key (kbd "C-x x") 'mc/mark-all-like-this)
  (global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-n") 'mc/mark-next-like-this))

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
          wdired-allow-to-change-permissions 'advanced)))

(use-package dired+
  :ensure t
  :config
  (global-dired-hide-details-mode 1))

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

(use-package afternoon-theme
  :ensure t
  :init
  (progn
    (load-theme 'afternoon t)
    (global-hl-line-mode 1)))

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

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init (setf magit-last-seen-setup-instructions "2.1.0")
  :config
  (setf vc-display-status nil
        magit-push-always-verify nil)
  (remove-hook 'git-commit-finish-query-functions
               'git-commit-check-style-conventions))

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
    (add-hook 'lisp-mode-hook #'paredit-mode)
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
                ((string-match "\\.[ch]$" file)
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

(use-package browse-url
  :defer t
  :init (setf url-cache-directory (locate-user-emacs-file "local/url"))
  :config
  (when (executable-find "firefox")
    (setf browse-url-browser-function #'browse-url-firefox
          browse-url-generic-program "xombrero"
          browse-url-generic-args '("-n"))))

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


(provide 'init) ; make (require 'init) happy
