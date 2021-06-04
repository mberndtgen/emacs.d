;;; init.el --- mberndtgen config -*- eval: (read-only-mode 0) -*-

;;; Commentary:

;;; Code:

(make-directory (locate-user-emacs-file "local") :no-error)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/etc")

;; Set up package manager
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; package archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cljr-helm . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
(setq package-archive-priorities '(("gnu" . 4)
                                   ("org" . 3)
                                   ("melpa" . 2)
                                   ("melpa-stable" . 1)))

(defvar init.el-errors '()
  "A list of errors that occured during initialization. Each error is of the form (LINE ERROR &rest ARGS).")

(defvar init.el-line 0
  "Approximation to the currently executed line in this file.")

;; prevent opening a new frame when loading a file
(setq ns-pop-up-frames nil)

;; personal information
(setq user-full-name "Manfred Berndtgen")

;; performance and statistics
;; output see *Messages* buffer
(setq use-package-verbose t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure nil)
(require 'use-package)

;; https://github.com/waymondo/use-package-ensure-system-package
(use-package use-package-ensure-system-package
  :ensure t)

;; Quelpa grabs and builds packages from source (e.h. github)
(use-package quelpa)
(use-package quelpa-use-package :ensure t)

;; Handle the `use-package-always-ensure' setting
(quelpa-use-package-activate-advice)

;; always pick latest version of the library to load
(setq load-prefer-newer t)

;; add github stars in package listing, autoremove packs, install packs parallel
(use-package paradox
  :ensure t
  :delight " ፨"
  :commands (paradox-list-packages))

;; (use-package use-package-ensure-system-package
;;   :ensure t)

;; (require 'use-package)
;; (require 'diminish)

;; binding keys
(use-package bind-key
  :bind ("C-h B" . describe-personal-keybindings))

;; path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))

;; backup settings
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '((".*" . "~/.saves"))
      ;; auto-save-file-name-transforms `((".*" "~/.saves" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; dash at point
;; (when (eq system-type 'darwin)
;;   (when (not (package-installed-p 'dash-at-point))
;;     (package-install 'dash-at-point))
;;   ;; dash-at-point
;;   (autoload 'dash-at-point "dash-at-point" "Search the word at point with Dash." t nil)
;;   (global-set-key "\C-cd" 'dash-at-point))

;; Define `expose' since it's used everywhere.
(defun expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda ()
    (interactive)
    (apply function args)))

;; Some global keybindings
;;
(if (eq system-type 'darwin)
    (setq mac-function-modifier 'hyper
          mac-right-option-modifier 'super
          mac-right-command-modifier 'super
          mac-right-control-modifier 'ctrl
          mac-pass-command-to-system nil
          mac-command-modifier 'meta ; make opt key do Super
          mac-control-modifier 'ctrl ; make Control key do Control
          mac-option-modifier 'none
          ns-function-modifier 'hyper))
(if (eq system-type 'gnu/linux)
    nil)
(if (eq system-type 'windows-nt)
    nil)

(global-set-key (kbd "C-j") #'join-line)
;;(global-set-key (kbd "M-g") #'goto-line)
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

(setq network-security-level 'high)

;; Frames and fonts

;; (add-to-list 'default-frame-alist
;;              '(font . "Fira Code-16"))

;; (defun my-set-frame-fullscreen (&optional frame)
;;   (set-frame-parameter frame 'fullscreen 'fullheight))

(use-package ring
  :bind ("H-f" . bnb/font-next)
  ("H-F" . bnb/font-prev)
  :config
  (setq bnb/fontlist '("Fira Code-13" "Source Code Pro-13")
        bnb/font-ring
        (ring-convert-sequence-to-ring bnb/fontlist)
        bnb/font
        (ring-ref bnb/font-ring 0))
  (defun bnb/font-apply (font)
    "Change the default font to FONT."
    (set-frame-font (setq bnb/font font))
    (message "Set default font to %s" bnb/font))
  (defun bnb/font-next ()
    "Cycle the default font to the next in the ring."
    (interactive)
    (bnb/font-apply (ring-next bnb/font-ring bnb/font)))
  (defun bnb/font-prev ()
    "Cycle the default font to the previous in the ring."
    (interactive)
    (bnb/font-apply (ring-prev bnb/font-ring bnb/font)))
  (set-frame-font bnb/font))

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

(use-package unicode-fonts
  :ensure t
  :defer 10
  :init
  (unicode-fonts-setup))

;; dynamic font sizes
(defun bnb/change-frame-font-size (fn)
  "Change the frame font size according to function FN."
  (let* ((font-name (frame-parameter nil 'font))
         (decomposed-font-name (x-decompose-font-name font-name))
         (font-size (string-to-number (aref decomposed-font-name 5))))
    (aset decomposed-font-name 5 (int-to-string (funcall fn font-size)))
    (set-frame-font (x-compose-font-name decomposed-font-name))))

(defun bnb/frame-text-scale-increase ()
  "Increase the frame font size by 1."
  (interactive)
  (bnb/change-frame-font-size '1+))

(defun bnb/frame-text-scale-decrease ()
  "Decrease the frame font size by 1."
  (interactive)
  (bnb/change-frame-font-size '1-))

(bind-keys
 ("C-+" . text-scale-increase)
 ("C--" . text-scale-decrease)
 ("s--" . bnb/frame-text-scale-decrease)
 ("s-+" . bnb/frame-text-scale-increase)
 ("s-=" . bnb/frame-text-scale-increase))

;; If I ever use a font with a missing glyph, this will let Emacs check the Symbola font for the missing data.
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; Make the cursor the full width of the character at point.
(setq x-stretch-cursor t)

;; Allow for profiling of font-locking.
(use-package font-lock-profiler
  :commands (font-lock-profiler-start
             font-lock-profiler-buffer
             font-lock-profiler-region))


;;; convenience settings

(global-linum-mode 1)
(column-number-mode 1)
(setq-default comment-column 70) ; Set the default comment column to 70
(setq-default line-spacing 0.2)
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)
(setq-default frame-title-format '("%b - %f - %I")) ;; buffer name, full file name and size

;;; S - shift key
;;; M - Cmd key
;;; C - Ctrl key
;;; s - Option key
;;(global-set-key (kbd "C-c s") 'slime-selector)
;; (global-set-key (kbd "H-ü") "|")
;; (global-set-key (kbd "H-2") "@")
;; (global-set-key (kbd "H-ö") "[")
;; (global-set-key (kbd "H-ä") "]")
;; (global-set-key (kbd "H-p") "{")
;; (global-set-key (kbd "H-+") "}")
;; (global-set-key (kbd "H-<") "~")

(custom-set-variables
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes (quote ("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(fci-rule-color "#2a2a2a")
 '(scroll-preserve-screen-position 'always)
 '(which-key-mode t))

(electric-indent-mode +1) ;; indent after entering RET
(electric-pair-mode +1) ;; automatically add a closing paren
(desktop-save-mode 1) ;; save sessions

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

;; run emacs as server
(when (and (or (eq system-type 'windows-nt) (eq system-type 'darwin))
           (not (and (boundp 'server-clients) server-clients))
           (not (daemonp)))
  (server-start))

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

;; mode line style
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line-highlight nil :box nil)

;; pretty mode
(use-package pretty-mode
  :disabled
  :config
  (global-pretty-mode t)
  (pretty-activate-groups
   '(:sub-and-superscripts :greek :arithmetic-nary)))

(use-package prog-mode ; Contains pretty-symbols-mode
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode t)
  (add-hook
   'python-mode-hook
   (lambda ()
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(;; Syntax
             ("def" .      ?ℱ)
             ("not" .      ?❗)
             ("in" .       ?∈)
             ("not in" .   ?∉)
             ("return" .   ?⟼)
             ("yield" .    ?⟻)
             ("for" .      ?∀)
             ;; Base Types
             ("int" .      ?ℤ)
             ("float" .    ?ℝ)
             ("str" .      ?𝕊)
             ("True" .     ?𝕋)
             ("False" .    ?𝔽)
             ;; Mypy
             ("Dict" .     ?𝔇)
             ("List" .     ?ℒ)
             ("Tuple" .    ?⨂)
             ("Set" .      ?Ω)
             ("Iterable" . ?𝔊)
             ("Any" .      ?❔)
             ("Union" .    ?∪))))))

;; color treatment

(use-package rainbow-mode
  :commands (rainbow-mode)
  :ensure t)

(use-package kurecolor
  :bind (("H-k" . kurecolor-increase-hue-by-step)
         ("H-j" . kurecolor-decrease-hue-by-step)
         ("s-k" . kurecolor-increase-saturation-by-step)
         ("s-j" . kurecolor-decrease-saturation-by-step)
         ("s-l" . kurecolor-increase-brightness-by-step)
         ("s-h" . kurecolor-decrease-brightness-by-step))
  :ensure t)

;; hydra
(use-package hydra
  :ensure t)

;; relaxed handling of mode line
(use-package delight
  :ensure t
  :commands delight)

;; toggle map: show toggleable settings until pressing 'q'

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

;;handle emptiness
(use-package whitespace
  :ensure nil
  :config
  (setq whitespace-line-column nil)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :delight whitespace-mode)

;; beacon: highlight cursor
;; https://github.com/Malabarba/beacon
(use-package beacon
  :ensure t)

(with-eval-after-load "beacon"
  (beacon-mode 1))

;; goto-line-preview
;; https://github.com/jcs-elpa/goto-line-preview
(use-package goto-line-preview
  :ensure t)

(with-eval-after-load "goto-line-preview"
  (global-set-key [remap goto-line] 'goto-line-preview))

;; highlight-parentheses
;; https://github.com/tsdh/highlight-parentheses.el
(use-package highlight-parentheses
  :ensure t
  :hook (after-init . highlight-parentheses-mode))

;; ensure proper lisping
(add-hook 'after-save-hook  'check-parens nil t)

;; checks (on saving) whether the file you edit contains a shebang, and if yes, makes it executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; For view-only buffers rendering content, it is useful to have them auto-revert in case of changes.
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'image-mode 'auto-revert-mode)

(bind-key "M-k" 'fixup-whitespace)

;; auto-revert buffer
;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; scroll screen without moving the cursor
(defun bnb/scroll-up-1 ()
  "Scroll up by one line."
  (interactive)
  (cua-scroll-up 1))

(defun bnb/scroll-down-1 ()
  "Scroll down by one line."
  (interactive)
  (cua-scroll-down 1))

(bind-keys
 ("M-n" . bnb/scroll-up-1)
 ("M-p" . bnb/scroll-down-1))

;; align text by regexp
(bind-key "C-c TAB" 'align-regexp)

;; kill current buffer (instead of asking which one)
(defun bnb/kill-this-buffer ()
  "Kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(bind-keys
 ("C-x C-k" . bnb/kill-this-buffer))

;; no duplicates in minibuffer history
(setq history-delete-duplicates t)
(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

;; abbrev
(bind-key "C-x C-i" 'bnb/ispell-word-then-abbrev)

(defun bnb/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction
    made. With prefix P, create local abbrev. Otherwise, it will be
    global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p global-abbrev-table local-abbrev-table)
        bef aft))))

(use-package abbrev
  :delight " ⚆"
  :config
  (setq save-abbrevs t)
  (setq-default abbrev-mode t))

;; try to expand text before point in an intelligent way
(bind-key "M-/" 'hippie-expand)

;; shortcut for editing init.el
(bind-key "<f4>" (lambda ()
                   (interactive)
                   (find-file "~/.emacs.d/init.el")))

;; find-file-in-project
;; https://github.com/technomancy/find-file-in-project
(use-package find-file-in-project
  :ensure t
  :bind
  (("H-x f" . find-file-in-project)
   ("H-x ." . find-file-in-project-at-point)))

;; save bookmarks
;; C-x r m   set a bookmark
;; C-x r b   jump to a bookmark
;; C-x r l   list bookmarks
;; M-x bookmark-delete delete bookmark by name
;; auto-save bookmarks:
(setq bookmark-save-flag t)

;; writegood
(use-package writegood-mode
  :ensure t
  :bind
  ("C-c g"     . writegood-mode)
  ("C-c C-g g" . writegood-grade-level)
  ("C-c C-g e" . writegood-reading-ease))

;; spell checking
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

;; vimrc generic mode
(define-generic-mode 'vimrc-generic-mode
  '()
  '()
  '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
     (0 font-lock-warning-face))
    ("\\(^\\|[\t ]\\)\\(\".*\\)$"
     (2 font-lock-comment-face))
    ("\"\\([^\n\r\"\\]\\|\\.\\)*\""
     (0 font-lock-string-face)))
  '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
  '((lambda ()
      (modify-syntax-entry ?\" ".")))
  "Generic mode for Vim configuration files.")

;; ediff single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package move-text
  :ensure t
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down))
  :config (move-text-default-bindings))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; (dolist (m '(clojure-mode
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  ;; add paths to your local installation of project mgmt tools, like lein
  ;;              clojurec-mode
  ;;              clojurescript-mode
  ;;              clojurex-mode))
  ;;   (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;; (setq lsp-enable-indentation nil
  ;;       lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))
  )

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
  :config
  ;; disable inline documentation
  ;; (setq lsp-ui-sideline-enable nil)
  ;; disable showing docs on hover at the top of the window
  (setq lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top
        lsp-ui-imenu-window-width 20
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-delay 0.5
        lsp-ui-sideline-enable t
        lsp-ui-imenu-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-doc-enable nil
        lsp-ui-doc-position 'bottom-and-right
        lsp-ui-doc-delay '2
        lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-reference)
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

;; if you are helm user
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  )
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list helm-lsp-global-workspace-symbol helm-lsp-code-actions helm-lsp-switch-project)
  :config
  (lsp-treemacs-sync-mode 1)
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package use-package-hydra
  :ensure t)

;; eglot
;; https://github.com/joaotavora/eglot
(use-package eglot
  :ensure t
  :bind
  (("H-p ." . eglot-help-at-point))
  :hook
  ((go-mode . eglot-ensure)))

;; ace-flyspell (https://github.com/cute-jumper/ace-flyspell)
(use-package ace-flyspell
  :ensure t
  :commands (ace-flyspell-setup)
  :bind
  ("H-s" . hydra-fly/body)
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
  :ensure t
  :bind (:map isearch-mode-map
              ("C-'" . ace-isearch-jump-during-isearch))
  :delight ace-isearch-mode
  :config
  (global-ace-isearch-mode t)
  (setq ace-isearch-input-length 8))

;; In modes with links, use o to jump to links. Map M-o to do the same in org-mode.
(use-package ace-link
  :ensure t
  :bind (:map org-mode-map
              ("M-o" . ace-link-org))
  :config (ace-link-setup-default))

;; provide numbers for quick window access
(use-package ace-window
  :ensure t
  :bind
  ("H-a"    . ace-window)
  ("<f9> a" . ace-window)
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

;; go hydra
(use-package hydra
  :ensure t
  :after dap-mode
  :config
  (require 'hydra)
  (require 'dap-mode)
  (require 'dap-ui)
  :commands (ace-flyspell-setup)
  :bind
  ("M-s" . hydra-go/body)
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

;; zap to char using avy
(use-package avy-zap
  :ensure t
  :bind ("M-z" . avy-zap-to-char-dwim)
  ("M-Z" . avy-zap-up-to-char-dwim))

;; The edit server talks to Chrome and uses emacs to edit any text areas.
(use-package edit-server
  :ensure t
  :defer 10
  :init
  (edit-server-start))

;; regexp builder
(setq reb-re-syntax 'string)

;; DAP
(use-package dap-mode
  :defer t
  :ensure t
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("C-<f5>" . dap-hydra))
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-stopped . (lambda (&_rest) (call-interactively #'dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
         (go-mode . (lambda ()
                      (require 'dap-go)
                      (dap-go-setup)
                      (setq dap-go-delve-path (concat (getenv "HOME") "/go/bin/dlv"))))
         (js2-mode . (lambda ()
                       (require 'dap-node)
                       (dap-node-setup))))
  :init
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (setq dap-print-io t)
  (require 'dap-hydra)
  (require 'dap-chrome)
  (dap-chrome-setup)
  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode 1)))


(use-package lsp-treemacs
  :ensure t
  :config
  ;;(lsp-metals-treeview-enable t) ; scala metals support
  ;;(setq lsp-metals-treeview-show-when-views-received t)
  )

(use-package company
  :ensure t
  :init
  (progn
    (add-hook 'after-init-hook #'global-company-mode)
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-more-hook #'company-mode)
    (setq company-dabbrev-ignore-case nil
          company-dabbrev-code-ignore-case nil
          company-dabbrev-downcase nil
          company-idle-delay 0
          company-minimum-prefix-length 2
          company-begin-commands '(self-insert-command)
          company-transformers '(company-sort-by-occurrence)
          company-tooltip-align-annotations t)))

(use-package company-quickhelp
  :ensure t
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

(use-package company-tabnine
  :ensure t
  :config
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  :init
  (add-to-list 'company-backends #'company-tabnine))

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package undo-tree
  :ensure t
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
  :ensure t
  :commands filladapt-mode
  :init (setq-default filladapt-mode t)
  :hook ((text-mode . filladapt-mode)
         (org-mode . turn-off-filladapt-mode)
         (prog-mode . turn-off-filladapt-mode)))

(use-package buffer-move
  :defer t)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))  ; set rainbow-delimiters mode for most programming modes

(use-package highlight-symbol
  :ensure t
  :bind (("<C-f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("<S-f3>" . highlight-symbol-prev)
         ("<M-f3>" . highlight-symbol-query-replace)))

(use-package smooth-scrolling
  :ensure t)

(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(use-package projectile
  :ensure t
  :bind
  ("C-c p" . projectile-command-map)
  ("C-x w" . hydra-projectile-other-window/body)
  ("C-c C-p" . hydra-projectile/body)
  :config
  (use-package counsel-projectile
    :after (projectile)
    :ensure t
    :bind
    (:map projectile-command-map
          ("s s" . counsel-projectile-rg)
          ("p" . counsel-projectile-switch-project))
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
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "s-w") 'delete-frame)
    (define-key projectile-mode-map (kbd "s-x") 'exchange-point-and-mark))
  (when (eq system-type 'windows-nt)
    (setq projectile-indexing-method 'native))
  (setq projectile-enable-caching t
        projectile-require-project-root t
        projectile-mode-line '(:eval (format " 🛠[%s]" (projectile-project-name)))
        projectile-completion-system 'default)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-mode)
  (defhydra hydra-projectile-other-window (:color teal)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window        "file")
    ("g"  projectile-find-file-dwim-other-window   "file dwim")
    ("d"  projectile-find-dir-other-window         "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil                                      "cancel" :color blue))
  (defhydra hydra-projectile (:color teal :hint nil)
    "
 PROJECTILE: %(projectile-project-root)

 Find File            Search/Tags          Buffers                Cache
  ------------------------------------------------------------------------------------------
  _C-f_: file            _r_: ag                _i_: Ibuffer           _c_: cache clear
   _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
   _fd_: file curr dir   _o_: multi-occur     _C-k_: Kill all buffers  _X_: cleanup non-existing
    _r_: recent file                                               ^^^^_z_: cache current
    _d_: dir

  "
    ("r"   counsel-projectile-rg)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("C-f" projectile-find-file)
    ("ff"  projectile-find-file-dwim)
    ("fd"  projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("C-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("C-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("C-p" projectile-switch-project "switch project")
    ("p"   projectile-switch-project)
    ("s"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue)))


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
  :ensure t
  :demand
  :hook
  (dired-mode . centaur-tabs-local-mode)
  ;;(dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  ;;(calendar-mode . centaur-tabs-local-mode)
  ;;(org-agenda-mode . centaur-tabs-local-mode)
  ;;(helpful-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-hide-tab "TAGS*")
  (setq centaur-tabs-style "box"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-plain-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'over
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*")
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "fira code" 120)
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
    ;; Froma https://gist.github.com/antifuchs/9238468
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

;; (use-package ivy
;;   :ensure t
;;   :defer 0.1
;;   :delight " 🙘"
;;   :bind
;;   ("C-c C-r" . ivy-resume)
;;   ("C-x B" . ivy-switch-buffer-other-window)
;;   :custom
;;   (ivy-count-format "(%d/%d) ")
;;   (ivy-use-virtual-buffers t)
;;   (ivy-display-style 'fancy)
;;   (ivy-re-builders-alist
;;    '((t . ivy--regex-fuzzy)))
;;   :config
;;   (ivy-mode))

;; (use-package counsel
;;   :after ivy
;;   :delight " 𝖈"
;;   :bind
;;   ("s-M-x"   . counsel-M-x)
;;   ("H-y"     . counsel-yank-pop)
;;   ("C-x C-f" . counsel-find-file)
;;   ("C-c a"   . counsel-rg)
;;   ("H-h f"  . counsel-describe-function)
;;   ("H-h v"  . counsel-describe-variable)
;;   ("H-h l"  . counsel-find-library)
;;   ("H-h i"  . counsel-info-lookup-symbol)
;;   ("H-u"    . counsel-unicode-char)
;;   :custom
;;   (counsel-find-file-at-point t)
;;   (enable-recursive-minibuffers t)
;;   :config
;;   (counsel-mode))

;; (use-package smex
;;   :delight
;;   :ensure t
;;   :config (smex-initialize))

;; (use-package ivy-rich
;;   :ensure t
;;   :after ivy
;;   :delight
;;   :custom
;;   (ivy-virtual-abbreviate 'full)
;;   (ivy-rich-switch-buffer-align-virtual-buffer t))

;; (use-package ivy-hydra
;;   :ensure t
;;   :after ivy)

;; (use-package ivy-view
;;   :ensure nil
;;   :bind
;;   ("H-[" . ivy-push-view)
;;   ("H-]" . ivy-pop-view)
;;   ("H-b" . ivy-switch-buffer)
;;   :config
;;   (setq ivy-use-virtual-buffers t))

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :after ivy-mode)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))))

(use-package zoom
  :ensure t)

(use-package swiper
  :ensure t
  :diminish
  :bind
  ("C-s" . swiper)
  ("C-c S" . swiper)
  ("C-x C-r" . counsel-recentf)
  ("s-E" . counsel-colors-emacs)
  ("s-W" . counsel-colors-web)
  ;; :config
  ;; (progn
  ;;   ;; (ivy-mode 1)
  ;;   ;; (setq ivy-use-virtual-buffers t)
  ;;   ;; (define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
  ;;   (ivy-set-actions
  ;;    'counsel-find-file
  ;;    '(("d" (lambda (x) (delete-file (expand-file-name x)))
  ;;       "delete")))
  ;;   (ivy-set-actions
  ;;    'ivy-switch-buffer
  ;;    '(("k"
  ;;       (Lambda (x)
  ;;               (kill-buffer x)
  ;;               (ivy--reset-state ivy-last))
  ;;       "kill")
  ;;      ("j"
  ;;       ivy--switch-buffer-other-window-action
  ;;       "other window"))))
  )

;; expand region: https://github.com/magnars/expand-region.el.
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

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
  :ensure t
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

(use-package prog-fill
  :ensure t
  :commands (prog-fill)
  :config
  (setq
   prog-fill-floating-close-paren-p nil
   prog-fill-break-method-immediate-p t))

(use-package auto-complete-config
  :ensure auto-complete
  :bind ("M-<tab>" . my--auto-complete)
  :init
  (defun my--auto-complete ()
    (interactive)
    (unless (boundp 'auto-complete-mode)
      (global-auto-complete-mode 1))
    (auto-complete)))

;; Load local "packages"
(require 'init-utils)
(require 'unannoy)
;;(require 'extras)

;; (require 'slime-cfg) ; --deprecated
(require 'sly-cfg)
(require 'clojure-cfg)
(require 'haskell-cfg)
;;(require 'python-cfg)
(require 'perl5-cfg)
;;(require 'perl6-cfg)
(require 'golang-cfg)
(require 'javascript-cfg)
(require 'other-languages)
(require 'org-cfg)
(require 'misc)


;;; which-key for help with key bindings - https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :delight which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  (setq which-key-max-description-length 60))

;; multiple cursors - https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :bind
  ("H-m"   . hydra-mc/body)
  ("C-x m" . hydra-mc/body)
  ("s-<mouse-1>" . mc/add-cursor-on-click)
  ("C-x M" . compose-mail)
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
  "Add my hydra-mc funcs to the proper whitelist"
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
  :commands (markdown-mode gfm-mode)
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (remove-hook 'fill-nobreak-predicate
                           'markdown-inside-link-p t)))
  (setf sentence-end-double-space nil
        markdown-indent-on-enter nil
        markdown-command
        "pandoc -f markdown -t html5 -s --self-contained --smart")
  :init (setq markdown-command "multimarkdown"))

;; for html
(use-package impatient-mode
  :ensure t
  :defer t
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


(use-package doom-themes
  :ensure t
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

(use-package ghub
  :ensure t)

;; remember to run 'M-x all-the-icons-install-fonts'
(use-package doom-modeline
  :ensure t
  :defer t
  :delight
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
;; highlight text an all shown buffers
(use-package avy
  :ensure t
  :bind
  ("H-SPC" . avy-goto-char-timer)
  ("H-w"   . avy-goto-word-1)
  ("H-c"   . avy-goto-char-2)
  ("H-l"   . avy-goto-line)
  ("H-d"   . avy-goto-word-0)
  ("<f9> SPC" . avy-goto-char-timer)
  ("C-c g" . avy-goto-word-1)
  ("M-g l" . avy-goto-line)
  ("M-g c" . avy-goto-char-2)
  ("M-g w" . avy-goto-word-0))

(use-package simple
  :defer t
  :config
  (progn
    ;; disable so I don't use it by accident
    (define-key visual-line-mode-map (kbd "M-q") (expose (lambda ())))
    (add-hook 'tabulated-list-mode-hook #'hl-line-mode)))

;; When editing files with the same name, but different location, a unique identifier (based on path) is preferred over a number.
(use-package uniquify
  :defer 10
  :config
  (setf uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator ":"))

;; Dim everything except for the thing-at-point.
(use-package focus
  :ensure t
  :bind
  ("C-c f" . focus-mode)
  ("C-c F" . focus-read-only-mode))

(use-package linum-relative
  :defer 10
  :ensure t)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(use-package pdf-tools
  :defer t
  :ensure t
  :init (add-hook 'pdf-view-mode-hook
                  (lambda ()
                    (nlinum-mode 0)))
  :config
  (custom-set-variables '(pdf-tools-handle-upgrades nil))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-info-epdfinfo-program "/usr/local/bin/pdfinfo")
  (pdf-loader-install)
  (pdf-tools-install))

(use-package helpful
  :ensure t
  :bind
  ("C-c C-d" . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h F" . helpful-function)
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable)
  ("C-h C" . helpful-command)
  ("C-h z" . helpful-macro))

(use-package winner
  :config
  (progn
    (winner-mode 1)
    (windmove-default-keybindings)))

;; what keybindings were active in the current buffer for the current mode?
(use-package discover-my-major
  :ensure t
  :bind (("C-h C-m" . discover-my-major)
         ("C-h C-d" . discover-my-mode)))

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
;; no need for a pager when running eshell
(setenv "PAGER" "cat")

(defun eshell/emacs (&rest args)
  "Open a file in emacs the natural way"
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

;; some aliases for eshell
(with-eval-after-load "em-alias"
  '(progn
     (eshell/alias "em" "emacs")
     (eshell/alias "ll" "ls -Aloh")
     (eshell/alias "llc" "*ls -AlohG --color=always")))

(defun eshell/gst (&rest args)
  (magit-status-internal (pop args) nil)
  (eshell/echo))

(defun eshell/gd (&rest args)
  (magit-diff-unstaged)
  (eshell/echo))

(defun eshell/gds (&rest args)
  (magit-diff-staged)
  (eshell/echo))

(use-package tex-site
  :defer 10
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'orgtbl-mode)
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
  (add-to-list 'org-latex-packages-alist
               '("" "tikz" t))
  (add-to-list 'org-latex-packages-alist
               '("" "minted" t))
  (setq org-latex-create-formula-image-program 'imagemagick)
  (eval-after-load "preview"
    '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

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

(use-package forge
  :ensure t
  :commands (forge-pull))

(use-package magit-todos
  :ensure t
  :after magit
  :hook (magit-mode-hook . magit-todos-mode))

;; git timemachine
(use-package git-timemachine
  :disabled
  :ensure t
  :bind ("C-x C-g" . git-timemachine-toggle)
  :config
  (bind-keys
   :map git-timemachine-mode-map
   ("<M-up>" . git-timemachine-show-previous-revision)
   ("<M-down>" . git-timemachine-show-next-revision)
   ("<S-wheel-up>" . git-timemachine-show-previous-revision)
   ("<S-wheel-down>" . git-timemachine-show-next-revision)))

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
  :ensure t
  :defer 10)

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
  :delight " 🍐"
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'sly-editing-mode #'paredit-mode)
    ;;(add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode))
  :config (define-key paredit-mode-map (kbd "C-j") #'join-line))

(use-package paxedit
  :ensure t
  :delight " ꁀ"
  :commands (paxedit-mode)
  :bind
  ("M-<right>" . paxedit-transpose-forward)
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
  ("C-#"       . paxedit-symbol-kill))

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
