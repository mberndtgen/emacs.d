;;; init.el --- mberndtgen config -*- eval: (read-only-mode 0) -*-

;;; Commentary:

;;; Code:

;; Make startup faster by reducing the frequency of garbage collection and then use a hook to measure Emacs startup time.
;; Also, turn on lexical-binding for the init file!

;; -*- lexical-binding: t; -*-

(setq debug-on-error t)

;; default is 800 kilobytes.  Measured in bytes.
;; This reduces the frequency of garbage collection during startup, making the initialization faster.
(setq gc-cons-threshold (* 1024 1024 32))

(add-hook 'emacs-startup-hook (lambda ()
                                ;; resetting gc-cons-threshold to ensure that Emacs does not consume excessive memory during normal operation.
                                (setq gc-cons-threshold (* 1024 1024 2) ;; 2 MB
                                      gc-cons-percentage 0.1) ;; Fine-tunes the garbage collection behavior for further optimization.
                                (message "Emacs is ready in %s with %d garbage collections."
                                         (format "%.2f seconds"
                                                 (float-time (time-subtract after-init-time before-init-time)))
                                         gcs-done)
                                ))

;; could be bad, will not let you save at all, until you correct the error
 (add-hook 'emacs-lisp-mode-hook
  (lambda ()
    ;; adds check-parens to local-write-file-hooks only for the current buffer
    ;;(nil t ensures it’s a buffer-local hook).
    (when (buffer-file-name) ;; only for files
      (add-hook 'write-file-functions 'check-parens))))

;; directories

(make-directory (locate-user-emacs-file "local") :no-error)
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("lisp" "etc" "elpa/emacs-reveal")))

;; PATH and exec-path
(setenv "PATH"
        (concat
         "/usr/bin" path-separator
         "~/.ghcup/bin" path-separator
         "~/.go/bin" path-separator
         (getenv "PATH")))

(when (eq system-type 'darwin)
  (setq exec-path
        '("/usr/bin"
          "~/.ghcup/bin"
          "~/go/bin"
          )))

;; Place to put local packages.
(let* ((path (expand-file-name "lisp" user-emacs-directory))
       (local-pkgs (mapcar 'file-name-directory (directory-files-recursively path "\\.el$"))))
  (if (file-accessible-directory-p path)
      (mapc (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))

;; Set up package manager
(require 'package)

;; package archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 3)
                                   ("melpa" . 2)
                                   ("melpa-stable" . 1)
                                   ))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; (setq package-enable-at-startup nil)

(setq byte-compile-warnings '(cl-functions))

(defvar init.el-errors '()
  "List of errors that occured during initialization.")

(defvar init.el-line 0
  "Approximation to the currently executed line in this file.")

;; prevent opening a new frame when loading a file
(setq ns-pop-up-frames nil)

;; personal information
(setq user-full-name "Manfred Berndtgen")

;; performance and statistics
;; output see *Messages* buffer
(defvar use-package-verbose)
(defvar use-package-compute-statistics)
(defvar use-package-minimum-reported-time)
(defvar use-package-always-ensure)

;; initialise use-packages on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Clean up unused repos with `straight-remove-unused-repos'

;;
;; basic UI configuration
;;
(setf backup-inhibited nil
      auto-save-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      echo-keystrokes 0.1
      delete-active-region nil
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold nil
      make-backup-files nil
      create-lockfiles nil
      ring-bell-function 'ignore
      auto-save-list-file-prefix "~/.emacs.d/auto-save/save-"
      backup-directory-alist `("." . ,(expand-file-name
                                       (concat user-emacs-directory "backups")))
      backup-inhibited nil
      set-fringe-mode 10)

;; set up visible bell
(setq visible-bell t)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(setq-default dired-allow-to-change-permissions t)

;; always pick latest version of the library to load
(setq load-prefer-newer t)

;; GUIs are for newbs
(dolist (mode'(menu-bar-mode tool-bar-mode tooltip-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Too distracting
(blink-cursor-mode -1)

;; overwrite text when highlighted
(delete-selection-mode t)
(global-display-line-numbers-mode t)

;; I never want to use this
(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))

;; I hate typing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Magit is the only front-end I care about
(setf vc-handled-backends nil
      ad-redefinition-action 'accept ; Don’t warn when advice is added for functions
      vc-follow-symlinks t)          ; Don’t warn for following symlinked files

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)
(setq-default truncate-lines t)

(column-number-mode t)
(global-auto-revert-mode t)
(setq-default comment-column 70) ; Set the default comment column to 70
(setq-default line-spacing 0.24)
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)
(setq-default frame-title-format '("%b - %f - %I")) ;; buffer name, full file name and size

;;; S - shift key
;;; M - Cmd key
;;; C - Ctrl key
;;; s - Option key

(electric-indent-mode +1) ;; indent after entering RET
(electric-pair-mode +1) ;; automatically add a closing paren

(setq desktop-restore-frames nil   ; don't restore frame layout
      desktop-restore-frameset nil ; don't restore detailed window sizes
      desktop-load-locked-desktop t) ; avoid prompts

(desktop-save-mode 1) ;; save sessions

;;
;; font configguration
;;
;; backup settings
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '((".*" . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Define `expose' since it's used everywhere.
(defun expose (function &rest args)
  "Return interactive version of FUNCTION, =exposing= it to user."
  (lambda ()
    (interactive)
    (apply function args)))

;; Some global keybindings
;;
(defvar mac-pass-command-to-system)

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-j") #'join-line)
(global-set-key (kbd "C-x C-k") #'compile)
(global-set-key (kbd "<f4>") (expose #'revert-buffer nil t))
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

(defvar network-security-level)
(setq network-security-level 'high)

;; Frames and fonts

(add-to-list 'default-frame-alist
             '(font . "Fira Code-16"))


(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat " " (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(defun find-user-init-file () ;; instant access to init file
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

(advice-add 'display-startup-echo-area-message
            :override #'ignore)

;; mode line style
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-attribute 'mode-line-inactive nil :box nil)
;;(set-face-attribute 'mode-line-highlight nil :box nil)

;; quick switch to scratch buffer
(defun switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))


;;
;; load packages
;;

(use-package s)
(use-package dash)
(use-package logview)

(use-package command-log-mode
  :straight t
  :commands command-log-mode
  :config (global-command-log-mode t))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
  (doom-themes-neotree-config)     ; Enable custom neotree theme (all-the-icons must be installed!)
  :init
  (load-theme 'doom-wilmersdorf t)
  ;; for treemacs users (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; NOTE: 1st time you load config on a new machine,
;; remember to run 'M-x all-the-icons-install-fonts' first!
(use-package all-the-icons)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 10)
  (doom-modeline-bar-width 6)
;;(doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

(use-package unicode-fonts
  :defer 10
  :init
  (unicode-fonts-setup))

;; If I ever use a font with a missing glyph, this will let Emacs check the Symbola font for the missing data.
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; Make the cursor the full width of the character at point.
(setq x-stretch-cursor t)

;; Auto-Saving Changed Files
;; (use-package super-save
;;   :defer 1
;;   :diminish super-save-mode
;;   :config
;;   (super-save-mode +1)
;;   (setq super-save-auto-save-when-idle t))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Persist history over Emacs restarts. Vertico sorts by history position.
                                        ; Individual history elements can be configured separately
;;(put 'minibuffer-history 'history-length 25)
;;(put 'evil-ex-history 'history-length 50)
;;(put 'kill-ring 'history-length 25))
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; Allow for profiling of font-locking.
(use-package font-lock-profiler
  :commands (font-lock-profiler-start font-lock-profiler-buffer font-lock-profiler-region))

;; optional if you want which-key integration
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package helm
  :diminish helm-mode
  :custom
  (helm-candke idate-number-limit 100)
  (helm-input-idle-delay 0.01)      ; this actually updates things reeeelatively quickly.
  (helm-ff-skip-boring-files t)
  :init
  ;; From https://gist.github.com/antifuchs/9238468
  (defvar helm-idle-delay)
  (defvar helm-yas-display-key-on-candidate)
  (defvar helm-quick-update)
  (defvar helm-M-x-requires-pattern)
  (defvar helm-M-x-fuzzy-match)
  (defvar helm-grep-default-command)
  (defvar helm-grep-default-recurse-command)

  (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't)
        helm-yas-display-key-on-candidate t
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-autoresize-mode t
        helm-M-x-fuzzy-match t)
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
  ;;(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
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
         ("C-x c SPC" . helm-all-mark-rings))
  :config
  (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
  (helm-mode))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-at-point helpful-function helpful-key helpful-command helpful-macro)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-h C" . helpful-command)
         ("C-h z" . helpful-macro)))

;; Stateful Keymaps with Hydra
(use-package hydra
  :defer t)

(defmacro toggle-setting-string (setting)
  `(if (and (boundp ',setting) ,setting) '[x] '[_]))

(bind-key
 "C-x t"
 (defhydra hydra-toggle (:color amaranth)
   "
    _c_ column-number : %(toggle-setting-string column-number-mode)  _b_ orgtbl-mode    : %(toggle-setting-string orgtbl-mode)  _x_/_X_ trans          : %(identity bnb/transparency)
    _e_ debug-on-error: %(toggle-setting-string debug-on-error)  _s_ orgstruct-mode : %(toggle-setting-string orgstruct-mode)  _m_   hide mode-line : %(toggle-setting-string bnb/hide-mode-line-mode)
    _u_ debug-on-quit : %(toggle-setting-string debug-on-quit)  _h_ diff-hl-mode   : %(toggle-setting-string diff-hl-mode)
    _f_ auto-fill     : %(toggle-setting-string auto-fill-function)  _B_ battery-mode   : %(toggle-setting-string display-battery-mode)
    _t_ truncate-lines: %(toggle-setting-string truncate-lines)  _l_ highlight-line : %(toggle-setting-string hl-line-mode)
    _r_ read-only     : %(toggle-setting-string buffer-read-only)  _n_ line-numbers   : %(toggle-setting-string linum-mode)
    _w_ whitespace    : %(toggle-setting-string whitespace-mode)  _N_ relative lines : %(if (eq linum-format 'linum-relative) '[x] '[_])
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
   ("x" bnb/transparency-next nil)
   ("B" display-battery-mode nil)
   ("X" bnb/transparency-previous nil)
   ("h" diff-hl-mode nil)
   ("l" hl-line-mode nil)
   ("n" linum-mode nil)
   ("N" linum-relative-toggle nil)
   ("m" bnb/hide-mode-line-mode nil)
   ("q" nil)))

;; features for elisp mode
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

(add-hook 'prog-mode-hook
          (lambda() (add-hook 'completion-at-point-functions
                         nil 'local)))
;; org mode

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Menlo" :weight 'regular :height (cdr face)))

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode nil)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org
  :straight nil
  :commands (org-capture org-agenda)
  :hook ((org-mode . efs/org-mode-setup)
         (org-agenda-mode . (lambda () (hl-line-mode 1))))
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
         ;;("<f5>" . bh/org-todo)
         ;;("<S-f5>" . bh/widen)
         ("<C-f6>" . (lambda () (interactive) (bookmark-set "SAVED")))
         ("<f6>" . (lambda () (interactive) (bookmark-jump "SAVED")))
         ;;("<f7>" . bn/set-truncate-lines)
         ;;("<f7>" . bh/set-truncate-lines)
         ("<f8>" . org-cycle-agenda-files)
         ;;("<f9> <f9>" . bh/show-org-agenda)
         ("<f9> b" . bbdb)
         ("<f9> c" . calendar)
         ("<f9> f" . boxquote-insert-file)
         ("<f9> g" . gnus)
         ;;("<f9> h" . bh/hide-other)
         ;;("<f9> n" . bh/toggle-next-task-display)
         ;;("<f9> I" . bh/punch-in)
         ;;("<f9> O" . bh/punch-out)
         ;;("<f9> o" . bh/make-org-scratch)
         ("<f9> r" . boxquote-region)
         ;;("<f9> s" . bh/switch-to-scratch)
         ;;("<f9> t" . bh/insert-inactive-timestamp)
         ;;("<f9> T" . bh/toggle-insert-inactive-timestamp)
         ("<f9> v" . visible-mode)
         ("<f9> l" . org-toggle-link-display)
         ;;("<f9> SPC" . bh/clock-in-last-task)
         ("C-<f9>" . previous-buffer)
         ("M-<f9>" . org-toggle-inline-images)
         ("C-x n r" . narrow-to-region)
         ("C-<f10>" . next-buffer)
         ("<f11>" . org-clock-goto)
         ("C-<f11>" . org-clock-in)
         ;;("C-s-<f12>" . bh/save-then-publish)
         )
  :custom
  (org-directory "~/Dropbox/orgfiles")
  ;; Compact the block agenda view
  (org-agenda-compact-blocks t)
  (org-agenda-files (mapcar (lambda (path) (concat org-directory path))
                            '("/work-2025.org")))
  (org-agenda-persistent-filter t)
  (org-ascii-links-to-notes nil)
  (org-ascii-headline-spacing (quote (1 . 1)))
  (org-capture-templates '(("t" "todo" entry (file org-default-notes-file)
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
                            "* MEETING on %? :MEETING:\n%U" :clock-in t :clock-resume t)
                           ("p" "Phone call" entry (file org-default-notes-file)
                            "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                           ("h" "Habit" entry (file org-default-notes-file)
                            "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
  ;; Do not prompt to resume an active clock
  (org-clone-delete-id t)
  (org-cycle-include-plain-lists t)
  ;; handling blank lines
  (org-cycle-separator-lines 0)
  ;; agenda visibility
  (org-default-notes-file "~/Dropbox/orgfiles/refile.org")
  ;; Separate drawers for clocking and logs
  (org-drawers '("PROPERTIES" "LOGBOOK"))
  ;; enable task blocking - prevents tasks from changing to DONE if any subtasks are still open
  (org-enforce-todo-dependencies t )
  (org-ellipsis " ▾")
  (org-export-with-smart-quotes t)
  (org-hide-emphasis-markers t)
  (org-html-table-default-attributes '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none"))
  ;; attachments
  (org-id-method 'uuidgen)
  ;; See https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
  (org-image-actual-width nil)
  ;; Use the current window for indirect buffer display
  (org-indirect-buffer-display 'current-window)
  (org-insert-heading-respect-content nil)
  ;; logging
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  ;; Targets complete directly with IDO
  (org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes (quote confirm))
  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (org-refile-use-outline-path t)
  ;; RET follows hyperlinks in org-mode:
  (org-return-follows-link t)
  ;; notes at the top
  (org-reverse-note-order nil)
  ;; selecting text with S-<cursor>
  (org-support-shift-select t)
  (org-replace-disputed-keys t)
  ;; searching and showing results
  (org-show-following-heading t)
  (org-show-hierarchy-above t)
  (org-show-siblings '((default)))
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window)
  ;; Can be set per file basis with: #+STARTUP: noalign (or align). Same as doing C-c C-c in a table.
  (org-startup-align-all-tables t)
  (org-startup-indented t)
  (org-startup-truncated t)
  ;; tags with fast selection keys
  (org-tag-alist '((:startgroup) ;; put mutually exclusive tasks here
                   ("@errand" . ?e)
                   ("@office" . ?o)
                   ("@home" . ?H)
                   ("@farm" . ?f)
                   (:endgroup)
                   ("AGENDA" . ?a)
                   ("PLANNING" . ?p)
                   ("WAITING" . ?w)
                   ("HOLD" . ?h)
                   ("PERSONAL" . ?P)
                   ("WORK" . ?W)
                   ("FARM" . ?F)
                   ("ORG" . ?O)
                   ("NORANG" . ?N)
                   ("crypt" . ?E)
                   ("NOTE" . ?n)
                   ("IDEA" . ?i)
                   ("CANCELLED" . ?c)
                   ("FLAGGED" . ??)))
  (org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                  ("WAITING" ("WAITING" . t))
                                  ("HOLD" ("WAITING") ("HOLD" . t))
                                  (done ("WAITING") ("HOLD"))
                                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                  ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  ;; exporting tables to csv
  (org-table-export-default-format "orgtbl-to-csv")
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "BACKLOG(b)" "PLAN(p)" "ACTIVE(a)" "REVIEW(v)" "MEETING(m)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(k@)" "COMPLETED(c)")))
  (org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                            ("NEXT" :foreground "blue" :weight bold)
                            ("DONE" :foreground "forest green" :weight bold)
                            ("WAITING" :foreground "orange" :weight bold)
                            ("HOLD" :foreground "magenta" :weight bold)
                            ("CANCELLED" :foreground "forest green" :weight bold)
                            ("MEETING" :foreground "forest green" :weight bold)
                            ("PHONE" :foreground "forest green" :weight bold)))
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-use-fast-todo-selection t)
  (org-use-speed-commands t)
  (org-time-stamp-rounding-minutes '(1 1)) ;bh/keep-clock-running nil
  (org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM") ; Set default column view headings: Task Effort Clock_Summary
  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                           ("STYLE_ALL" . "habit")))
  (org-fast-tag-selection-single-key 'expert) ; Allow setting single tags without the menu
  (org-archive-location "%s_archive::* Archived Tasks") ; archiving
  (org-babel-results-keyword "results") ; babel
  (org-babel-default-header-args '((:eval . "never-export")))
  (org-agenda-text-search-extra-files '(agenda-archives)) ; Include agenda archive files when searching for things
  (org-special-ctrl-a/e t) ; editing and special key Handling
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-hide-leading-stars nil) ; show leading stars
  ;; minimize emacs frames
  (org-link-frame-setup '((vm . vm-visit-folder)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)))
  (org-src-window-setup 'current-window) ; Use the current window for C-c ' source editing
  ;; modules for habit tracking
  (org-modules '(ol-bbdb
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
                 ol-w3m
                 org-tempo))
  (require-final-newline t)

  :config
  ;; Global
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq
   ;; Inline images in HTML instead of producting links to the image
   org-html-inline-images t
   ;; Do not use sub or superscripts - I currently don't need this functionality in my documents
   org-export-with-sub-superscripts nil
   ;; Use org.css from the norang website for export document stylesheets
   ;; org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />"
   org-html-head-include-default-style nil
   ;; remove xml header line for html exports
   org-html-xml-declaration '(("html" . "")
                              ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                              ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))

   ;; Do not generate internal css formatting for HTML exports
   org-export-htmlize-output-type 'css
   ;; Export with LaTeX fragments
   org-export-with-LaTeX-fragments t
   ;; Increase default number of headings to export
   org-export-headline-levels 6
   ;; allow #+BIND: vars to be set on export w/o confirmation
   org-export-allow-BIND t
   ;; narrowing
   org-show-entry-below '((default))
   org-agenda-include-diary nil
   org-agenda-diary-file "~/Dropbox/orgfiles/diary.org"
   org-agenda-insert-diary-extract-time t
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
   org-agenda-start-with-log-mode t
   ;; Enable display of the time grid so we can see the marker for the current time
   ;; org-agenda-time-grid '((daily today remove-match)
   ;;                        #("----------------" 0 16 (org-heading t))
   ;;                        (0900 1100 1300 1500 1700))
   ;; Display tags farther right
   org-agenda-tags-column -102
   ;; Agenda sorting functions
   ;;org-agenda-cmp-user-defined 'bh/agenda-sort
   ;; Use sticky agenda's so they persist
   org-agenda-sticky t
   ;; position the habit graph on the agenda to the right of the default
   org-habit-graph-column 60
   ;; speed commands
   ;; org-use-speed-commands t
   ;; org-speed-commands-user '(("0" . ignore)
   ;;                           ("1" . ignore)
   ;;                           ("2" . ignore)
   ;;                           ("3" . ignore)
   ;;                           ("4" . ignore)
   ;;                           ("5" . ignore)
   ;;                           ("6" . ignore)
   ;;                           ("7" . ignore)
   ;;                           ("8" . ignore)
   ;;                           ("9" . ignore)

   ;;                           ("a" . ignore)
   ;;                           ("d" . ignore)
   ;;                           ;;("h" . bh/hide-other)
   ;;                           ("i" progn
   ;;                            (forward-char 1)
   ;;                            (call-interactively 'org-insert-heading-respect-content))
   ;;                           ("k" . org-kill-note-or-show-branches)
   ;;                           ("l" . ignore)
   ;;                           ("m" . ignore)
   ;;                           ;;("q" . bh/show-org-agenda)
   ;;                           ("r" . ignore)
   ;;                           ("s" . org-save-all-org-buffers)
   ;;                           ("w" . org-refile)
   ;;                           ("x" . ignore)
   ;;                           ("y" . ignore)
   ;;                           ("z" . org-add-note)

   ;;                           ("A" . ignore)
   ;;                           ("B" . ignore)
   ;;                           ("E" . ignore)
   ;;                           ;;("F" . bh/restrict-to-file-or-follow)
   ;;                           ("G" . ignore)
   ;;                           ("H" . ignore)
   ;;                           ("J" . org-clock-goto)
   ;;                           ("K" . ignore)
   ;;                           ("L" . ignore)
   ;;                           ("M" . ignore)
   ;;                           ;;("N" . bh/narrow-to-org-subtree)
   ;;                           ;;("P" . bh/narrow-to-org-project)
   ;;                           ("Q" . ignore)
   ;;                           ("R" . ignore)
   ;;                           ("S" . ignore)
   ;;                           ;;("T" . bh/org-todo)
   ;;                           ;;("U" . bh/narrow-up-one-org-level)
   ;;                           ("V" . ignore)
   ;;                           ;;("W" . bh/widen)
   ;;                           ("X" . ignore)
   ;;                           ("Y" . ignore)
   ;;                           ("Z" . ignore))
   org-remove-highlights-with-change t
   org-read-date-prefer-future 'time
   ;; automatically change list bullets
   org-list-demote-modify-bullet '(("+" . "-")
                                   ("*" . "-")
                                   ("1." . "-")
                                   ("1)" . "-")
                                   ("A)" . "-")
                                   ("B)" . "-")
                                   ("a)" . "-")
                                   ("b)" . "-")
                                   ("A." . "-")
                                   ("B." . "-")
                                   ("a." . "-")
                                   ("b." . "-"))

   ;; remove indentation on agenda tags view
   org-tags-match-list-sublevels t
;;   org-table-use-standard-references 'from
   ;; Overwrite the current window with the agenda
   org-agenda-window-setup 'current-window
   org-clone-delete-id t
   org-startup-folded t
   ;; preserve source block indentation
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 0
   org-catch-invisible-edits 'error
   ;; keep utf9 as default coding system
   org-export-coding-system 'utf-8
   default-process-coding-system '(utf-8-unix . utf-8-unix)
   org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
   org-use-sub-superscripts t)
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  (efs/org-font-setup)
  :init
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  ;;(run-at-time "00:59" 3600 'org-save-all-org-buffers)
  )

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (require 'ob-js)
;;  (require 'org-re-reveal-ref)
;;  (require 'oer-reveal-publish)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (js . t)
     (lisp . t)
     (perl . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; insert structure template blocks
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("pl" . "src perl"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(with-eval-after-load 'org
  ;; manual see https://github.com/yjwen/org-reveal
  ;; reveal.js home: https://github.com/hakimel/reveal.js/
  (use-package helm-org
    :config
    (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))
  )

(if (eq system-type 'gnu/linux)
    (setq org-reveal-root "file:///home/mberndtgen/Documents/src/emacs/reveal.js/"))
(if (eq system-type 'darwin)
    (setq org-reveal-root "file:///Users/v236177/Dropbox/orgfiles/reveal.js/"))
;;(setq org-reveal-mathjax t)

;;Org-export to LaTeX
(with-eval-after-load 'ox-latex
  (message "Now loading org-latex export settings")
  ;; page break after toc
  (setq org-latex-toc-command "\\tableofcontents \\clearpage"
        org-latex-listings t)
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
                 ("\\usepackage{tikzposter}")
                 ("\\usepackage[colorlinks=true,urlcolor=SteelBlue4,linkcolor=Firebrick4]{hyperref}")
                 ("\\usepackage[hyperref,x11names]{xcolor}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-packages-alist '())
  (add-to-list 'org-latex-packages-alist '("" "color" t))
  (add-to-list 'org-latex-packages-alist '("" "tabularx" t))
  (add-to-list 'org-latex-packages-alist '("" "longtable" t))
  (add-to-list 'org-latex-packages-alist '("" "array" t))
  (add-to-list 'org-latex-packages-alist '("" "tabu" t))
  (add-to-list 'org-latex-packages-alist '("" "fontenc" t))
  (add-to-list 'org-latex-packages-alist '("" "multirow" t)))


(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-word (- arg))))

;; Enable vertico
;; see https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t) ; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-scroll-margin 0) ; Different scroll margin
  (vertico-count 20) ; Show more candidates
  (vertico-resize t) ; Grow and shrink the Vertico minibuffer
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; tab widths
(setq-default tab-width 2) ; Default to an indentation size of 2 spaces
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs for indentation

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  :init
  (setq completion-category-defaults nil))


(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

;;Consult provides a lot of useful completion commands similar to Ivy’s Counsel.
(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

;; Switching Directories with consult-dir
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function nil))

;; Thanks Karthik!
(with-eval-after-load 'eshell-mode
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                   :narrow ?e
                                                   :category file
                                                   :face consult-file
                                                   :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs))))))))


;; Enable rich annotations using the Marginalia package
;; see https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :after vertico
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; add github stars in package listing, autoremove packs, install packs parallel
(use-package paradox
  :delight " ፨"
  :commands (paradox-list-packages))

;; pretty mode
(use-package pretty-mode
  :hook (org-mode . prettify-symbols-mode)
  :config
  (global-pretty-mode t)
  (pretty-activate-groups '(:sub-and-superscripts :greek :arithmetic-nary)))

;; color treatment
(use-package rainbow-mode
  :commands (rainbow-mode))

;; relaxed handling of mode line
(use-package delight
  :commands delight)

;; beacon: highlight cursor
;; https://github.com/Malabarba/beacon
(use-package beacon
  :hook (after-init . beacon-mode)
  :custom
  (beacon-push-mark 35)
  (beacon-color "#666600"))

;; goto-line-preview
;; https://github.com/jcs-elpa/goto-line-preview
(use-package goto-line-preview)

(with-eval-after-load "goto-line-preview"
  (global-set-key [remap goto-line] 'goto-line-preview))

;; highlight-parentheses
;; https://github.com/tsdh/highlight-parentheses.el
(use-package highlight-parentheses
  :hook (after-init . highlight-parentheses-mode))

;; checks (on saving) whether the file you edit contains a shebang, and if yes, makes it executable.
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

;; kill current buffer (instead of asking which one)
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

;; no duplicates in minibuffer history
(defvar savehist-additional-variables)
(defvar savehist-file)
(setq history-delete-duplicates t)
(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

;; try to expand text before point in an intelligent way
(bind-key "M-/" 'hippie-expand)

;; shortcut for editing init.el - now crux
(bind-key "<f4>" (lambda ()
                   (interactive)
                   (find-file "~/.emacs.d/init.el")))

;; support reading large blobs of data for LSP’s sake.
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; find-file-in-project
;; https://github.com/technomancy/find-file-in-project
(use-package find-file-in-project
  :bind
  (("H-x f" . find-file-in-project)
   ("H-x ." . find-file-in-project-at-point)))

;; save bookmarks
;; C-x r m   set a bookmark
;; C-x r b   jump to a bookmark
;; C-x r l   list bookmarks
;; M-x bookmark-delete delete bookmark by name
;; auto-save bookmarks:
(defvar bookmark-save-flag)
(setq bookmark-save-flag t)

;; writegood
(use-package writegood-mode
  :bind (("C-c g" . writegood-mode)
         ("C-c C-g g" . writegood-grade-level)
         ("C-c C-g e" . writegood-reading-ease)))

;; spell checking
(defvar ispell-extra-args)
(defvar ispell-program-name)

(cond
 ((executable-find "aspell")
  (setq ispell-program-name (executable-find "aspell")
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 (t (setq ispell-program-name nil)
    (message "No aspell found!")))

(bind-key "H-$" 'ispell-word)

;; proselint
(with-eval-after-load "flycheck-mode"
  (flycheck-define-checker proselint
    "A linter for prose"
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
        (id (one-or-more (not (any " "))))
        (message (one-or-more not-newline)
           (zero-or-more "\n" (any " ") (one-or-more not-newline)))
        line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

;; development
(show-paren-mode t)
(add-hook 'cperl-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)

;; read-only mode
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

;; default file encoding
(set-charset-priority 'unicode)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq buffer-file-coding-system 'utf-8
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE
(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le-dos))


;; ediff single frame
(defvar ediff-window-setup-function)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; window-purpose, see https://github.com/bmag/emacs-purpose
(use-package window-purpose
  :ensure t
  :init
  (purpose-mode))


(use-package move-text
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down))
  :config (move-text-default-bindings))



;; (defvar lsp-headerline-breadcrumb-segments)
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "M-L") ; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . efs/lsp-mode-setup)
         (lsp-mode . company-mode)
         (haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred)
         (lsp-managed-mode . lsp-modeline-diagnostics-mode)
         ;; if you want which-key integration
         (lsp-mode . (lambda () (lsp-enable-which-key-integration t)))
         )
  :config
  (lsp-enable-which-key-integration t)
  (add-hook 'hack-local-variables-hook (lambda () (when lsp-mode (lsp))))
  :custom
  (lsp-progress-via-spinner nil) ;; spinner seems to cause problems
  (lsp-restart 'ignore)
  (lsp-keep-workspace-alive nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-lens-enable t)
  (lsp-enable-snippet nil)
  ;; :global/:workspace/:file
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-file-watch-threshold 2000)
  (lsp-completion-provider :capf))

;;(use-package lsp-lens :delight)

;; bit of AI
(setq epg-gpg-program "gpg")
(setenv "GPG_AGENT_INFO" nil)
(setq epg-pinentry-mode 'loopback) ; see https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html

(setq auth-sources '("~/.emacs.d/secrets/authinfo.gpg"))

;;(load-library "~/.emacs.d/secrets/your-secrets.el.gpg")

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; provides fancier overlays.
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-doc-enable t)
  ;; You might want this:
  ;; (lsp-ui-doc-show-with-cursor nil)
  ;; Also this because isearch gets broken otherwise
  ;; (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-imenu-window-width 20)
  ;;   (lsp-ui-imenu-enable t)
  ;;   (lsp-ui-imenu-kind-position 'top)
  ;;   (lsp-ui-sideline-show-diagnostics t)
  ;;   (lsp-ui-sideline-show-hover t)
  ;;   (lsp-ui-sideline-show-code-actions t)
  ;;   (lsp-ui-sideline-update-mode 'line)
  ;;   (lsp-ui-sideline-delay 0.5)
  ;;   (lsp-ui-sideline-enable t)
  ;;   (lsp-ui-imenu-enable t)
  ;;   (lsp-ui-flycheck-enable t)
  ;;   (lsp-ui-doc-enable nil)
  ;;   (lsp-ui-doc-delay '2)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-reference))


;; xref
(use-package xref
  :pin gnu
  :bind (("s-r" . #'xref-find-references)
         ("s-[" . #'xref-go-back)
         ("C-<down-mouse-2>" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))

;; eldoc
(use-package eldoc
  :pin gnu
  :diminish
  :bind ("s-d" . #'eldoc)
  :custom (eldoc-echo-area-prefer-doc-buffer t))

;; eglot
;; https://github.com/joaotavora/eglot
(use-package eglot
  :bind
  ("H-p ." . eglot-help-at-point)
  :hook
  (go-mode . eglot-ensure)
  (haskell-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c a r" . #'eglot-rename)
              ("C-<down-mouse-1>" . #'xref-find-definitions)
              ("C-S-<down-mouse-1>" . #'xref-find-references)
              ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown t))

;;(use-package consult-eglot
;;  :bind (:map eglot-mode-map ("s-t" . #'consult-eglot-symbols)))

;; ace-flyspell (https://github.com/cute-jumper/ace-flyspell)
(use-package ace-flyspell
  :commands (ace-flyspell-setup)
  :bind ("H-s" . hydra-fly/body)
  :init
  (add-hook 'flyspell-mode-hook 'ace-flyspell-setup)
  (defhydra hydra-fly (:color pink)
    ("n" flyspell-goto-next-error "Next error")
    ("c" ispell-word "Correct word")
    ("j" ace-flyspell-jump-word "Jump word")
    ("." ace-flyspell-dwim "dwim")
    ("q" nil "Quit")))

;;  The C-' key let's me jump to the isearch match easily with the ace-jump methods.
(use-package ace-isearch
  :bind (:map isearch-mode-map
              ("C-'" . ace-isearch-jump-during-isearch))
  :delight ace-isearch-mode
  :config
  (global-ace-isearch-mode t)
  (setq ace-isearch-input-length 8))

;; In modes with links, use o to jump to links. Map M-o to do the same in org-mode.
(defvar org-mode-map)

(use-package ace-link
  :bind (:map org-mode-map
              ("M-o" . ace-link-org))
  :config (ace-link-setup-default))

;; provide numbers for quick window access
(use-package ace-window
  :bind (("H-a"    . ace-window)
         ("<f9> a" . ace-window))
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


;; Sharing Files with 0x0
(use-package 0x0)

;; regexp builder
(defvar reb-re-syntax)
(setq reb-re-syntax 'string)

;; DAP
;; (use-package dap-mode
;;   :bind
;;   (:map dap-mode-map
;;         ("C-c b b" . dap-breakpoint-toggle)
;;         ("C-c b r" . dap-debug-restart)
;;         ("C-c b l" . dap-debug-last)
;;         ("C-c b d" . dap-debug))
;;   :init
;;   (require 'dap-go)
;;   ;; NB: dap-go-setup appears to be broken, so you have to download the extension from GH, rename its file extension
;;   ;; unzip it, and copy it into the config so that the following path lines up
;;   ;;(setq dap-go-debug-program '("node" "/Users/patrickt/.config/emacs/.extension/vscode/golang.go/extension/dist/debugAdapter.js"))
;;   (defun pt/turn-on-debugger ()
;;     (interactive)
;;     (dap-mode)
;;     (dap-auto-configure-mode)
;;     (dap-ui-mode)
;;     (dap-ui-controls-mode))
;;   )

;; (use-package dap-mode
;;   :defer t
;;   :ensure t
;;   :functions dap-hydra/nil
;;   :bind (:map lsp-mode-map
;;               ("<f5>" . dap-debug)
;;               ("C-<f5>" . dap-hydra))
;;   :hook ((after-init . dap-mode)
;;          (dap-mode . dap-ui-mode)
;;          (dap-session-created . (lambda (&_rest) (dap-hydra)))
;;          (dap-stopped . (lambda (&_rest) (call-interactively #'dap-hydra)))
;;          (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
;;          (go-mode . (lambda ()
;;                       (require 'dap-go)
;;                       (dap-go-setup)
;;                       (defvar dap-go-delve-path)
;;                       (setq dap-go-delve-path (concat (getenv "HOME") "/go/bin/dlv"))))
;;          (js2-mode . (lambda ()
;;                        (require 'dap-node)
;;                        (dap-node-setup))))
;;   :init
;;   (setq dap-auto-configure-features '(sessions locals controls tooltip)
;;         dap-print-io t)
;;   (require 'dap-hydra)
;;   (require 'dap-chrome)
;;   (dap-chrome-setup)
;;   (use-package dap-ui
;;     :ensure nil
;;     :config
;;     (dap-ui-mode 1)))

(use-package company
  :defer 0.1
  :bind (("C-M-i" . company-complete)
         :map company-mode-map ("<backtab>" . company-ysnippet))
  ;; :hook (after-init . global-company-mode)
  :config
  ;; (setq-default
  ;;  company-minimum-prefix-length 0
  ;;  ;; get only preview
  ;;  company-frontends '(company-preview-frontend)
  ;;  ;; also get a drop down
  ;;  company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend))
  :init
  (setq global-company-mode nil
        company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        ;; company-global-modes '(not erc-mode message-mode help-mode
        ;;                            gud-mode eshell-mode shell-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)))


;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode 1))

;; 
(use-package company-box
  :hook (company-mode . company-box-mode))


(defun my-derived-lang-name ()
  "Return a derived language name for the current buffer."
  (let ((name (if (listp mode-name)
                  (car mode-name)
                mode-name)))
    (replace-regexp-in-string "\\(/.*\\|-ts-mode\\|-mode\\)$" "" (substring-no-properties name))))

(setq company-quickhelp-use-propertized-text t) ;; Optional

(with-eval-after-load 'company
  (advice-add 'company-quickhelp--doc :around
              (lambda (orig-fun &rest args)
                (let ((mode-name (my-derived-lang-name)))
                  (apply orig-fun args))))
  (add-hook 'company-mode-hook
            (lambda ()
              (setq-local mode-name (my-derived-lang-name))))
  (add-to-list 'company-backends 'company-capf))


;; Optional - provides snippet support.
(use-package yasnippet
  :defer 15 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config (yas-global-mode)
  :custom (yas-prompt-functions '(yas-completing-prompt)))

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
  :defer t)

;; folding, see https://github.com/zenozeng/yafolding.el
(use-package yafolding
  :ensure t
  :hook (prog-mode . yafolding-mode)
  :bind (("C-S-<return>" . yafolding-hide-parent-element)
         ("C-M-<return>" . yafolding-toggle-all)
         ("C-<return>" . yafolding-toggle-element))
  :commands yafolding-mode)

(use-package highlight-symbol
  :bind (("<C-f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("<S-f3>" . highlight-symbol-prev)
         ("<M-f3>" . highlight-symbol-query-replace)))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; The diminish package hides pesky minor modes from the modelines.
(use-package diminish)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'helm))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (setq projectile-project-search-path '("~/.emacs.d/" "~/Documents/src"))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t))

(use-package ripgrep)
(use-package rg)

(use-package helm-projectile
  :after (helm projectile)
  :config (helm-projectile-on))

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
                                        ;(string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "TAGS*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(use-package centaur-tabs
  :demand t
  :hook ((dired-mode . centaur-tabs-local-mode)
         ;;(dashboard-mode . centaur-tabs-local-mode)
         (term-mode . centaur-tabs-local-mode)
         ;;(calendar-mode . centaur-tabs-local-mode)
         (org-agenda-mode . centaur-tabs-local-mode)
         ;;(helpful-mode . centaur-tabs-local-mode)
         )
  :custom
  (centaur-tabs-style "box")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-plain-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  :config
  (centaur-tabs-hide-tab "TAGS*")
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "fira code" 120)
  (centaur-tabs-mode t)
  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward)
         ;;("C-c t s" . centaur-tabs-counsel-switch-group)
         ;;("C-c t p" . centaur-tabs-group-by-projectile-project)
         ;;("C-c t g" . centaur-tabs-group-buffer-groups)
         ))

(use-package zoom
  :defer t)

(use-package swiper
  :diminish
  :bind (("C-s" . swiper)
         ("C-c S" . swiper)
         ("C-x C-r" . counsel-recentf)
         ("s-E" . counsel-colors-emacs)
         ("s-W" . counsel-colors-web)))

;; expand region: https://github.com/magnars/expand-region.el.
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package buffer-move
  :bind (("<M-S-up>" . buf-move-up)
         ("<M-S-down>" . buf-move-down)
         ("<M-S-left>" . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

;;; Save point position between sessions
(use-package saveplace
  :custom
  (save-place-file (expand-file-name ".places" user-emacs-directory))
  :init (setq-default save-place t))

(use-package flycheck
  :defer 10
  :init
  (setq flycheck-global-modes '(not org-mode)) ;; prevents unmatched brackets error when saving
  (global-flycheck-mode 1) 
  :custom
  (flycheck-display-errors-delay .3)
  :config
  (bind-key "H-!"
            (defhydra hydra-toggle (:color amaranth)
              "
  _c_ Check buffer      _x_ Explain error
  _n_ Next error        _h_ Show error
  _p_ Previous error
  _l_ Show all errors   _s_ Select syntax checker
  _C_ Clear errors      _?_ Describe syntax checker
  "
              ("c" flycheck-buffer)
              ("n" flycheck-next-error)
              ("p" flycheck-previous-error)
              ("l" flycheck-list-errors)
              ("C" flycheck-clear-errors)
              ("x" flycheck-explain-error-at-point)
              ("h" flycheck-display-error-at-point)
              ("s" flycheck-select-checker)
              ("?" flycheck-describe-checker)
              ("q" nil))))

;; smart line formatter, https://github.com/ahungry/prog-fill
(use-package prog-fill
  :commands (prog-fill)
  :config
  (defvar prog-fill-break-method-immediate-p)
  (setq
   prog-fill-floating-close-paren-p nil
   prog-fill-break-method-immediate-p t))


;; Load local "packages"
;;(require 'clojure-cfg)
;;(require 'python-cfg)
;;(require 'golang-cfg)
(require 'javascript-cfg)

(use-package sql
  :init
  (setf sql-product 'sqlite))


;; depends on slime and other modes
;; see https://github.com/kaz-yos/eval-in-repl
(use-package eval-in-repl
  :hook ((cider-repl-mode . company-mode)
         (cider-mode . company-mode)
         (lisp-mode . (lambda ()
                        (local-set-key (kbd "<C-return>") 'eir-eval-in-sly))))
  :config
  ;; ielm support (for emacs lisp)
  (require 'eval-in-repl-ielm)
  ;; for .el files
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; for *scratch*
  (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; for M-x info
  (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)

  ;;(require 'eval-in-repl-hy)
  ;;(define-key hy-mode-map (kbd "<C-return>") 'eir-eval-in-hy)
  )


;; for interactively building regular expressions
(use-package re-builder
  :config
  (setf reb-re-syntax 'read))


;; visually display kill ring
;; see https://github.com/browse-kill-ring/browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :config
  (global-set-key "\C-cy" 'browse-kill-ring))

;; show vertical lines to guide indentation
;; see https://github.com/zk-phi/indent-guide
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

;; activate FiraCode font
(when (window-system)
  (set-frame-font "Fira Code"))

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))

  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))
;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions
         (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
;;(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

;; disable Fira Code for helm
(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

;; diable ligatures in helm
(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

;; On some systems, == will appear incorrectly as a blank space in certain modes
;; unless you add the following lines:

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . ?λ)
          ("->" . ?→)
          ("=>" . ?⇒)
          ("!=" . ?≠)
          ("===" . ?≡)
          ("!==" . ?≢)
          (">=" . ?⩾)
          ("<=" . ?⩽)
          ("<|" . ?⊲)
          ("|>" . ?⊳)
          (">->" . ?↣)
          ("~>" . ?↝)
          (">=>" . ?⟾)
          )))

;; prettify symbols for various modes
(add-hook 'haskell-mode-hook 'my-add-pretty-lambda)
(add-hook 'shen-mode-hook 'my-add-pretty-lambda)
(add-hook 'tex-mode-hook 'my-add-pretty-lambda)
(add-hook 'org-mode-hook 'my-add-pretty-lambda)
(global-prettify-symbols-mode 1) ; display “lambda” as “λ”

;; make tab complete without losing ability to manually indent
;;(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; all-the-icons
;; see https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :defer 10
  :ensure t)

;; nice icons for dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :commands (all-the-icons-dired-mode))

;; neotree (file sidebar for navigation)
;; see https://github.com/jaypei/emacs-neotree
(use-package neotree
  :commands (neotree)
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))


;; ensure that Emacs has access to the PATH associated with the current environment.
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; go

(use-package go-mode
  :defer t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package go-snippets :defer t)

(defun fix-messed-up-gofmt-path ()
  (interactive)
  (setq gofmt-command (string-trim (shell-command-to-string "which gofmt"))))

(use-package gotest)


(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

(defun rvl/display-fill-column ()
  (setq display-fill-column-indicator 1)
  (display-fill-column-indicator-mode))

(defun rvl/font-lock-keywords ()
  "Configure font-lock-mode to highlight TODO/fixme tags"
  (font-lock-add-keywords
   nil
   '(("\\<\\(fixme\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t))))

(use-package haskell-mode
  :delight "λ"
  :after haskell-font-lock

  :config
  ;; Flycheck is usually slow for Haskell stuff - only run on save.
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  :init
  (defun rvl/enable-subword-mode ()
    "Navigate within identifier names"
    (subword-mode +1))

  (defun rvl/stylish-on-save ()
    (setq haskell-stylish-on-save t))

  :hook ((haskell-mode . rvl/display-fill-column)
         (haskell-mode . rvl/stylish-on-save)
         (haskell-mode . rvl/font-lock-keywords)
         (haskell-mode . direnv-update-environment)

         (haskell-mode . rvl/enable-subword-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . imenu-add-menubar-index))

  :bind (:map haskell-mode-map
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c c" . haskell-process-cabal)
              ("C-c v c" . haskell-cabal-visit-file)
              ("C-c i" . haskell-navigate-imports)

              ;; YMMV with haskell-interactive-mode - LSP is a better bet
              ("C-`" . haskell-interactive-bring)
              ("C-c C-l" . haskell-process-load-file)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-k" . haskell-interactive-mode-clear)

              ;; These are usually set by default, but just make sure:
              ("M-." . xref-find-definitions)
              ("M-," . xref-pop-marker-stack)
              ("M-," . xref-find-references)

              :map haskell-cabal-mode-map
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c c" . haskell-process-cabal))

  :custom
  (haskell-process-log t))

(use-package lsp-haskell
  :after (haskell-mode lsp-mode)
  :config
  ;; Comment/uncomment this line to see interactions between lsp client/server.
  ;; (setq lsp-log-io t)
  :custom
  ;;(lsp-haskell-process-args-hie '("-d" "-l" "/tmp/hie.log"))
  ;;(lsp-haskell-server-args ())
  (lsp-haskell-server-path "haskell-language-server"))


(use-package direnv
  :config
  ;; enable globally
  (direnv-mode)
  ;; exceptions
  ;; (add-to-list 'direnv-non-file-modes 'foobar-mode)
  ;; nix-shells make too much spam -- hide
  (setq direnv-always-show-summary nil)
  :hook
  ;; ensure direnv updates before flycheck and lsp
  ;; https://github.com/wbolster/emacs-direnv/issues/17
  (flycheck-before-syntax-check . direnv-update-environment)
  (lsp-before-open-hook . direnv-update-environment)
  :custom
  ;; quieten logging
  (warning-suppress-types '((direnv))))


(use-package lsp-mode
  :hook ((haskell-mode . lsp)))


;; perl 5

(use-package cperl-mode
  :ensure t
  :bind
  (:map cperl-mode-map
        ("C-c d" . helm-perldoc))
  :hook
  ((cperl-mode . flycheck-mode)
   (cperl-mode . auto-complete-mode)
   (cperl-mode . turn-on-eldoc-mode)
   (cperl-mode . perltidy-mode))
  :mode ("\.pl$'" . cperl-mode)
  :init
  (autoload 'perltidy "perltidy-mode" nil t)
  (autoload 'perltidy-mode "perltidy-mode" nil t)
  :config
  ;; cperl-mode
  (setq cperl-indent-level 4
        cperl-continued-statement-offset 4
        cperl-close-paren-offset -4
        cperl-label-offset -4
        cperl-comment-column 40
        cperl-highlight-variables-indiscriminately t
        cperl-indent-parens-as-block t
        cperl-tab-always-indent nil
        cperl-font-lock t
        cperl-set-style "PerlStyle")
  ;; auto-complete
  (make-variable-buffer-local 'ac-sources)
  (setq ac-sources '(ac-source-perl-completion))
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  ;; helm-perldoc
  (helm-mode 1)
  (indent-guide-mode 1)
  (helm-perldoc:setup))

;; sly configuration
(use-package sly
  :ensure t
  :mode "\\.lisp$"
  :hook ((sly-mode . rainbow-delimiters-mode)
         (sly-mode . enable-paredit-mode)
         (sly-mode . company-mode)
         (sly-mode . prettify-symbols-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-mode . company-mode)
         (lisp-mode . sly-symbol-completion-mode)
         ;; (sly-mode . (lambda () (unless (sly-connected-p)
         ;;                     (save-excursion (sly)))))
         )
  :bind (:map sly-prefix-map
              ("M-h" . sly-documentation-lookup))
  :config
  (setq inferior-lisp-program "sbcl"
        sly-net-coding-system 'utf-8-unix
        sly-complete-symbol-function 'sly-flex-completions)
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (add-to-list 'auto-mode-alist '("sly-mrepl" . sly-mrepl-mode))
  (eval-after-load "sly"  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup)))

(use-package sly-quicklisp
  :after sly
  :ensure t
  :config
  (require 'sly-quicklisp))


;; other language modes

(use-package css-mode
  :mode ("\\.css\\'" . css-mode)
  :config
  (custom-set-variables
   '(css-indent-offset 2)))


(use-package yaml-mode 
  :mode ("\\.ya?ml$\\'" . yaml-mode)
  :hook (yaml-mode . (lambda ()
                       (setq-local paragraph-separate ".*>-$\\|[   ]*$")
                       (setq-local paragraph-start paragraph-separate))))

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode)
  :config (setf terraform-indent-level 4))

;; multiple cursors - https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("H-m"   . hydra-mc/body)
         ("C-x m" . hydra-mc/body)
         ("s-<mouse-1>" . mc/add-cursor-on-click)
         ("C-x M" . compose-mail))
  :config
  (defhydra hydra-mc (:hint nil)
    "
  ^Up^            ^Down^        ^Miscellaneous^
   -----------------------------------------------------------
   [_p_]   Next    [_n_]   Next    [_l_] Edit lines  [_x_] Arrows
   [_P_]   Skip    [_N_]   Skip    [_a_] Mark all    [_g_] Regexp
   [_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
    ("l"   mc/edit-lines :exit t)
    ("a"   mc/mark-all-like-this-dwim :exit t)
    ("n"   mc/mark-next-like-this)
    ("N"   mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p"   mc/mark-previous-like-this)
    ("P"   mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("g"   mc/mark-all-in-region-regexp :exit t)
    ("r"   mc/mark-sgml-tag-pair :exit t)
    ("x"   mc/mark-more-like-this-extended)
    ("q"   nil))
  (add-hydra-mc-funcs))

(defun add-hydra-mc-funcs ()
  "Add my hydra-mc funcs to the proper whitelist."
  (let* ((hydra-mc-funcs
    (cl-remove-if-not
     'functionp
     (apply #'append hydra-mc/heads)))
   (mc-funcs-to-ignore (cl-intersection
            hydra-mc-funcs
            mc--default-cmds-to-run-once))
   (funcs-to-whitelist
    (cl-mapcar
     (lambda (x) (intern (concat "hydra-mc/" (symbol-name x))))
     mc-funcs-to-ignore)))
    (let (value)
      (dolist (element funcs-to-whitelist nil)
  (add-to-list 'mc/cmds-to-run-once element)))))

;; remember recently opened files.
(use-package recentf
  :custom
  (recentf-max-menu-items 25)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1))

(use-package dabbrev
  :defer t
  :bind (("C-/" . #'dabbrev-completion))
  :custom
  (abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  (dabbrev-case-replace nil)
  :config (setf dabbrev-case-fold-search nil))

(use-package markdown-mode
  :straight t
  :mode ("\\.md$" "\\.markdown$" "vimperator-.+\\.tmp$")
  :commands (markdown-mode gfm-mode)
  :custom
  (sentence-end-double-space nil)
  (markdown-indent-on-enter nil)
  (markdown-command "pandoc -f markdown -t html5 -s --self-contained --smart")
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (remove-hook 'fill-nobreak-predicate
                           'markdown-inside-link-p t)))
  :init (setq markdown-command "multimarkdown"))

;; for html
(use-package impatient-mode
  :straight t
  :mode "\\.html\\'"
  :config
  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))
  (push (cons 'markdown-mode #'imp-markdown-filter)
        imp-default-user-filters)
  (add-to-list 'imp-default-user-filters '(mhtml-mode . nil)))

;; vterm

(use-package vterm
  :config
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :hook (vterm-mode . turn-off-chrome))


;; dired
(use-package all-the-icons-dired)

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
  :commands (dired dired-jump)
  :hook
  ((dired-load . (lambda ()
                   (interactive)
                   (dired-collapse)))
   (dired-mode . (lambda ()
                   (interactive)
                   (dired-omit-mode 1)
                   (all-the-icons-dired-mode 1)
                   (hl-line-mode 1))))
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-use-ls-dired t)
  (dired-listing-switches "-agho --group-directories-first")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-dwim-target t)
  (dired-recursive-copies 'top)
  (dired-auto-revert-buffer t)
  :config
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)
  (setq dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        delete-by-moving-to-trash t
        insert-directory-program "gls" ; on mac: 'brew install coreutils" first!
        )
  (setf dired-guess-shell-alist-user
        '(("\\.pdf\\'" "evince")
          ("\\(\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")
          ("\\(\\.png\\|\\.jpe?g\\)\\'" "qiv")
          ("\\.gif\\'" "animate"))
        dired-allow-to-change-permissions 'advanced)
  (autoload 'dired-omit-mode "dired-x"))

(use-package dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))


(use-package dired-ranger
  :after dired
  :defer t)

(use-package dired-collapse
  :after dired
  :defer t)


(use-package time
  :custom
  (display-time-default-load-average nil)
  (display-time-use-mail-icon t)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))


(use-package tramp
  :defer t
  :config
  (setf tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

(use-package whitespace-cleanup-mode
  :init
  (setq-default indent-tabs-mode nil)
  (global-whitespace-cleanup-mode))

(use-package diff-mode
  :defer t
  :hook (diff-mode . read-only-mode))

(use-package ghub
  :defer t)

;; jump anywhere in buffer, see https://github.com/abo-abo/avy
;; highlight text an all shown buffers
(use-package avy
  :bind (("H-SPC" . avy-goto-char-timer)
         ("H-w"   . avy-goto-word-1)
         ("H-c"   . avy-goto-char-2)
         ("H-l"   . avy-goto-line)
         ("H-d"   . avy-goto-word-0)
         ("<f9> SPC" . avy-goto-char-timer)
         ("C-c g" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)
         ("M-g c" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-0)))


;; zap to char using avy
(use-package avy-zap
  :bind
  (("M-z" . avy-zap-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim)))


;; Dim everything except for the thing-at-point.
(use-package focus
  :bind (("C-c f" . focus-mode)
         ("C-c F" . focus-read-only-mode)))

(use-package linum-relative
  :defer 10)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(use-package pdf-tools
  :defer t
  :hook (pdf-view-mode . (lambda ()
                           (nlinum-mode 0)))
  :config
  (custom-set-variables '(pdf-tools-handle-upgrades nil))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-info-epdfinfo-program "/usr/local/bin/pdfinfo")
  (pdf-loader-install)
  (pdf-tools-install))

;; Window History with winner-mode
(use-package winner
  :config
  (winner-mode))

;; Set Margins for Modes
(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dw/org-mode-visual-fill))

;; Control Buffer Placement
;; (setq display-buffer-base-action
;;       '(display-buffer-reuse-mode-window
;;         display-buffer-reuse-window
;;         display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)


;; Taming Popups with Popper.el

(defun dw/popper-window-height (window)
  (let (buffer-mode (with-current-buffer (window-buffer window)
                      major-mode))
    (pcase buffer-mode
      ('exwm-mode 40)
      (_ 15))))

(use-package popper
  :straight (popper :host github
                    :repo "karthink/popper"
                    :build (:not autoloads))
  :bind (("C-M-'" . popper-toggle-latest)
         ("M-'" . popper-cycle)
         ("C-M-\"" . popper-toggle-type))
  :custom
  (popper-window-height 12)
  ;; (popper-window-height
  ;; (lambda (window)
  ;;   (let ((buffer-mode (with-current-buffer (window-buffer window)
  ;;                        major-mode)))
  ;;     (message "BUFFER MODE: %s" buffer-mode)
  ;;     (pcase buffer-mode
  ;;       ('exwm-mode 40)
  ;;       ('helpful-mode 20)
  ;;       ('eshell-mode (progn (message "eshell!") 10))
  ;;       (_ 15)))))
  (popper-reference-buffers
   '("^\\*eshell\\*"
     "^vterm"
     help-mode
     helpful-mode
     compilation-mode))
  :init
  (require 'popper) ;; Needed because I disabled autoloads
  (popper-mode 1))


;; what keybindings were active in the current buffer for the current mode?
(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h C-d" . discover-my-mode)))

(use-package calc
  :defer t
  :config (setf calc-display-trail nil))

(use-package eshell
  :bind ("<f1>" . eshell-as)
  :hook (eshell-mode . (lambda ()
                         (define-key eshell-mode-map (kbd "<f1>") #'quit-window)))
  :init
  (setf eshell-directory-name (locate-user-emacs-file "local/eshell")))

;; no need for a pager when running eshell
(setenv "PAGER" "cat")

(defun eshell/emacs (&rest args)
  "Open a file in Emacs the natural way."
  (if (null args)
      ;; If emacs is called by itself, then just go to emacs directly
      (bury-buffer)
    ;; If opening multiple files with a directory name, e.g.
    ;; > emacs bar/bar.txt foo.txt
    ;; then the names must be expanded to complete file paths.
    ;; Otherwise, find-file will look in the current directory which
    ;; would fail for 'foo.txt' in the example above.
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun eshell/emo (&rest args)
  (mapc
   (lambda (f)
     (save-selected-window
       (find-file-other-window f)))
   (mapcar #'expand-file-name (eshell-flatten-list (reverse args)))))


(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; magit support, see https://magit.vc/
(use-package magit
  :commands magit-status
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit
  :commands (forge-pull))

;; smerge
(add-hook
 'smerge-mode-hook
 (lambda ()
   (bind-key
    "C-c ^ h"
    (defhydra hydra-smerge (:color amaranth)
      ("a" smerge-keep-all "Keep all")
      ("b" smerge-keep-base "Keep base")
      ("m" smerge-keep-mine "Keep mine")
      ("o" smerge-keep-other "Keep other")
      ("n" smerge-next "Next conflict")
      ("p" smerge-previous "Previous conflict")
      ("r" smerge-resolve "Keep mine")
      ("q" nil "quit"))
    smerge-mode-map)))

;; kill inactive buffers after 3 days
(use-package midnight
  :defer 10)

(use-package octave
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
  (setf octave-block-offset 4))

(use-package ielm
  :defer t
  :config
  (define-key ielm-map (kbd "C-c C-z") #'quit-window)
  (defadvice ielm-eval-input (after ielm-paredit activate)
    "Begin each ielm prompt with a paredit pair."
    (paredit-open-round)))

(use-package paredit
  :defer t
  :delight " 🍐"
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode-hook . paredit-mode)
         (scheme-mode-hook . paredit-mode)
         (ielm-mode-hook . paredit-mode))
  :config (define-key paredit-mode-map (kbd "C-j") #'join-line))

(use-package paxedit
  :delight " ꁀ"
  :commands (paxedit-mode)
  :bind (("M-<right>" . paxedit-transpose-forward)
         ("M-<left>"  . paxedit-transpose-backward)
         ("M-<up>"    . paxedit-backward-up)
         ("M-<down>"  . paxedit-backward-end)
         ("M-b"       . paxedit-previous-symbol)
         ("M-f"       . paxedit-next-symbol)
         ("C-%"       . paxedit-copy)
         ("C-&"       . paxedit-kill)
         ("C-*"       . paxedit-delete)
         ("C-^"       . paxedit-sexp-raise)
         ("M-u"       . paxedit-symbol-change-case)
         ("C-@"       . paxedit-symbol-copy)
         ("C-#"       . paxedit-symbol-kill)))

(use-package paren
  :config (show-paren-mode))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (ielm-mode . rainbow-delimiters-mode))
  :custom
  (rainbow-delimiters-max-face-count 1)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4"))

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

(use-package gnuplot-mode
  :defer t)

(use-package graphviz-dot-mode
  :defer t
  :config
  (setf graphviz-dot-indent-width 2
        graphviz-dot-auto-indent-on-semi nil))

(use-package yascroll
  :config
  (global-yascroll-bar-mode 1))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000)) ; 32 MB
(setq gc-cons-percentage 0.6)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   '("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(fci-rule-color "#2a2a2a")
 '(scroll-preserve-screen-position 'always)
 '(which-key-mode t))

(provide 'init)
;;; init.el ends here
