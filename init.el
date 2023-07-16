;;; init.el --- mberndtgen config -*- eval: (read-only-mode 0) -*-

;;; Commentary:

;;; Code:

(make-directory (locate-user-emacs-file "local") :no-error)
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("lisp" "etc" "elpa/emacs-reveal")))

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

(setq use-package-verbose t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0)


(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; initialise use-packages on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; keep config folders clean, see https://github.com/emacscollective/no-littering
;; For moving everything out of ~/.emacs.d reliably, set 'user-emacs-directory' before
;; loading no-littering!

(use-package no-littering)

;; no-littering doesn't set this by default so we must place aut-save files in the
;; same path as it uses for sessions
(setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(require 'use-package)
(setq use-package-always-ensure t)

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
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32)
      ring-bell-function 'ignore
      auto-save-default nil
      auto-save-list-file-prefix "~/.emacs.d/auto-save/save-"
      backup-directory-alist (quote (("." . "~/.emacs.d/saves")))
      backup-inhibited nil
      set-fringe-mode 10)

;; set up visible bell
(setq visible-bell t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

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
(delete-selection-mode +1)

;; I never want to use this
(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))

;; I hate typing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always use the one true encoding
(prefer-coding-system 'utf-8-unix)

;; Insert key is stupid
(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)

;; Magit is the only front-end I care about
(setf vc-handled-backends nil
      vc-follow-symlinks t)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)

(global-linum-mode 1)
(column-number-mode 1)
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
 '(package-selected-packages
   '(no-littering zoom yascroll yaml-mode xref-js2 writegood-mode whitespace-cleanup-mode which-key visual-regexp-steroids visual-fill-column use-package-hydra use-package-ensure-system-package unicode-fonts undo-tree typescript-mode transwin terraform-mode tern-auto-complete sunrise-commander smooth-scrolling smart-tabs-mode sly-quicklisp ripgrep rg reveal-in-osx-finder rainbow-mode rainbow-delimiters quelpa-use-package pug-mode prog-fill pretty-mode pov-mode pfuture pdf-tools paxedit paradox ox-reveal origami org-ref org-bullets org-ai npm notmuch neotree nasm-mode move-text lsp-ui linum-relative kurecolor kubernetes json-mode js2-refactor javadoc-lookup indent-guide impatient-mode highlight-symbol highlight-parentheses helpful helm-projectile helm-org helm-lsp graphviz-dot-mode goto-line-preview go-guru gnuplot-mode general forge font-lock-profiler focus flymake-eslint flymake-diagnostic-at-point flycheck find-file-in-project filladapt expand-region exec-path-from-shell evil-nerd-commenter eval-in-repl eslintd-fix elfeed eglot doom-themes doom-modeline discover-my-major dired-rainbow dired-narrow dired-filter dired-collapse diminish delight dap-mode crux counsel-projectile company-tabnine company-quickhelp company-box command-log-mode clojure-mode-extra-font-locking chatgpt-shell cfrs centaur-tabs buffer-move browse-kill-ring beacon avy-zap auctex arduino-mode anakondo all-the-icons-dired aggressive-indent ace-link ace-isearch ace-flyspell ac-js2 ac-cider))
 '(scroll-preserve-screen-position 'always)
 '(which-key-mode t))

(electric-indent-mode +1) ;; indent after entering RET
(electric-pair-mode +1) ;; automatically add a closing paren
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
  "Return interactive version of FUNCTION, 'exposing' it to user."
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
          mac-option-modifier 'super
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

;; I hate hitting this by accident
(global-set-key (kbd "C-<up>") #'previous-line)
(global-set-key (kbd "C-<down>") #'next-line)

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


;; (use-package ring
;;   :bind (("H-f" . bnb/font-next)
;;          ("H-F" . bnb/font-prev))
;;   :config
;;   (setq bnb/fontlist '("Fira Code-13" "Source Code Pro-13")
;;         bnb/font-ring
;;         (ring-convert-sequence-to-ring bnb/fontlist)
;;         bnb/font
;;         (ring-ref bnb/font-ring 0))
;;   (defun bnb/font-apply (font)
;;     "Change the default font to FONT."
;;     (set-frame-font (setq bnb/font font))
;;     (message "Set default font to %s" bnb/font))
;;   (defun bnb/font-next ()
;;     "Cycle the default font to the next in the ring."
;;     (interactive)
;;     (bnb/font-apply (ring-next bnb/font-ring bnb/font)))
;;   (defun bnb/font-prev ()
;;     "Cycle the default font to the previous in the ring."
;;     (interactive)
;;     (bnb/font-apply (ring-prev bnb/font-ring bnb/font)))
;;   (set-frame-font bnb/font))

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

;; (defun add-fira-code-symbol-keywords ()
;;   (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(defun find-user-init-file () ;; instant access to init file
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

(advice-add 'display-startup-echo-area-message
            :override #'ignore)

;; mode line style
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line-highlight nil :box nil)

;;
;; load packages
;;

(use-package command-log-mode
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
  (load-theme 'doom-one t)
  ;; for treemacs users (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; NOTE: 1st time you load config on a new machine,
;; remember to run 'M-x all-the-icons-install-fonts' first!
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 10))

(use-package unicode-fonts
  :defer 10
  :init
  (unicode-fonts-setup))

;; If I ever use a font with a missing glyph, this will let Emacs check the Symbola font for the missing data.
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; Make the cursor the full width of the character at point.
(setq x-stretch-cursor t)

;; Allow for profiling of font-locking.
;; (use-package font-lock-profiler
;;   :commands (font-lock-profiler-start font-lock-profiler-buffer font-lock-profiler-region))

;; optional if you want which-key integration
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

(use-package helm
  :diminish helm-mode
  :custom
  (helm-candidate-number-limit 100)
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
  (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
  (helm-mode 1)
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
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

;; hydra
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

(require 'org-cfg)

(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
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
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package corfu
  :bind (:map corfu-map
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("C-f" . corfu-insert))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-category-defaults nil))


(use-package consult-dir
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
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; see https://github.com/oantolin/embark
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; https://github.com/waymondo/use-package-ensure-system-package
(use-package use-package-ensure-system-package)

;; Quelpa grabs and builds packages from source (e.h. github)
;; (use-package quelpa)
;; (use-package quelpa-use-package)

;; Handle the `use-package-always-ensure' setting
;; (quelpa-use-package-activate-advice)

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

;; (With-eval-after-load "beacon"
;;                       (beacon-mode 1))

;; goto-line-preview
;; https://github.com/jcs-elpa/goto-line-preview
(use-package goto-line-preview)

(with-eval-after-load "goto-line-preview"
  (global-set-key [remap goto-line] 'goto-line-preview))

;; highlight-parentheses
;; https://github.com/tsdh/highlight-parentheses.el
(use-package highlight-parentheses
  :hook (after-init . highlight-parentheses-mode))

;; ensure proper lisping
(add-hook 'after-save-hook  'check-parens nil t)

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
           ("C-c TAB" align-regexp))

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

;; see https://github.com/bbatsov/crux
(use-package crux
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


(use-package move-text
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down))
  :config (move-text-default-bindings))

(defvar lsp-headerline-breadcrumb-segments)
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") ; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :config
  (lsp-enable-which-key-integration t))

;; optionally if you want to use debugger
(use-package dap-mode
  :after lsp)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


;; bit of AI
(setq epg-gpg-program "gpg")
(setenv "GPG_AGENT_INFO" nil)
(setq epg-pinentry-mode 'loopback) ; see https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html

(setq auth-sources '("~/.emacs.d/secrets/authinfo.gpg"))

;;(load-library "~/.emacs.d/secrets/your-secrets.el.gpg")

(use-package org-ai
  :after org
  :hook (org-mode . org-ai-mode) ; enable org-ai in org-mode
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-3.5-turbo")
  (org-ai-install-yasnippets) ; if you are using yasnippet and want `ai` snippets
)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; provides fancier overlays.
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  (lsp-ui-imenu-window-width 20)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom-and-right)
  (lsp-ui-doc-delay '2)
  (lsp-ui-peek-enable t)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-reference))


;; if you are helm user
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list helm-lsp-global-workspace-symbol helm-lsp-code-actions helm-lsp-switch-project)
  :config
  (lsp-treemacs-sync-mode 1)
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; eglot
;; https://github.com/joaotavora/eglot
(use-package eglot
  :bind
  ("H-p ." . eglot-help-at-point)
  :hook
  (go-mode . eglot-ensure))

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

;; zap to char using avy
(use-package avy-zap
  :bind
  (("M-z" . avy-zap-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim)))

;; regexp builder
(defvar reb-re-syntax)
(setq reb-re-syntax 'string)

;; DAP
;; (use-package dap-mode
;;   :custom
;;   (lsp-enable-dap-auto-configure nil)
;;   :config
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (require 'dap-node)
;;   (dap-node-setup))

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
  :hook (lsp-mode . company-mode)
  :bind
  ((:map company-active-map ("<tab>" . company-complete-selection))
   (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common)))
  :custom
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  (company-begin-commands '(self-insert-command))
  (company-transformers '(company-sort-by-occurrence))
  (company-tooltip-align-annotations t)
  (company-show-numbers t))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode 1))

(use-package company-tabnine
  :after company
  :custom
  (company-idle-delay 0) ;; Trigger completion immediately.
  (company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).
  :init
  (add-to-list 'company-backends #'company-tabnine))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Optional - provides snippet support.
(use-package yasnippet
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

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
     (string-prefix-p "*lsp" name)
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
  :init (global-flycheck-mode)
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
;;(require 'init-utils)
;;(require 'unannoy)
;; (require 'slime-cfg) ; --deprecated
;;(require 'sly-cfg)
;;(require 'clojure-cfg)
;;(require 'haskell-cfg)
;;(require 'python-cfg)
;;(require 'perl5-cfg)
;;(require 'golang-cfg)
;;(require 'javascript-cfg)
;;(require 'other-languages)
;;
;;(require 'misc)


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
  :custom
  (abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))

(use-package markdown-mode
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
  :mode "\\.html\\'"
  :config
  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
  (markdown out))))
  (push (cons 'markdown-mode #'imp-markdown-filter)
  imp-default-user-filters)
  (add-to-list 'imp-default-user-filters '(mhtml-mode . nil)))


(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-listing-switches "-alhG")
  (dired-dwim-target t)
  (dired-recursive-copies 'top)
  (dired-listing-switches "-ahl")
  (dired-auto-revert-buffer t)
  :config
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)
  (setf dired-guess-shell-alist-user
        '(("\\.pdf\\'" "evince")
          ("\\(\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")
          ("\\(\\.png\\|\\.jpe?g\\)\\'" "qiv")
          ("\\.gif\\'" "animate"))
        dired-allow-to-change-permissions 'advanced))

;; (use-package dired+
;;   :ensure t
;;   :config
;;   (global-dired-hide-details-mode 1))

(use-package dired-narrow
  :after dired
  :config
  (define-key dired-mode-map (kbd "C-x /") 'dired-narrow))


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

(use-package winner
  :init (winner-mode))

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

(use-package tex-site
  :defer 10
  :ensure auctex
  :hook ((LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . auto-fill-mode)
         (LaTeX-mode . orgtbl-mode))
  :config
  (setq TeX-auto-untabify t
        TeX-auto-save t
        TeX-save-query nil
        TeX-parse-self t
        TeX-output-view-style
        (if (eq system-type 'windows-nt)
            (quote
             (("^pdf$" "." "SumatraPDF.exe -reuse-instance %o")
              ("^html?$" "." "start %o")))
          (quote
           (("^pdf$" "." "evince -f %o")
            ("^html?$" "." "start %o"))))
        TeX-command-extra-options "-shell-escape"
        TeX-PDF-mode 1
        TeX-engine 'xetex)
  (setq-default TeX-master nil)
  (setq org-latex-listings t)
  (with-eval-after-load 'org
    (add-to-list 'org-latex-packages-alist '("" "tikzposter" t))
    (add-to-list 'org-latex-packages-alist '("" "tikz-cd" t))
    (add-to-list 'org-latex-packages-alist '("" "minted" t)))
  (setq org-latex-create-formula-image-program 'imagemagick)
  (eval-after-load "preview"
    '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

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
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
