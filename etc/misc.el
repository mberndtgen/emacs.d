;;; misc.el --- summary:

;;; Commentary:

;;----------------------------------------------------------------------------
;; misc.el - enable miscellaneous packages
;;----------------------------------------------------------------------------

;;; Code:

(use-package help-mode
  :defer t
  :bind (:map help-mode-map
              ("f" . push-first-button)))

(use-package origami
  :ensure t
  :defer t
  :hook (prog-mode . origami-mode)
  :custom
  (origami-show-fold-header t)
  :commands origami-mode
  :custom-face
  (origami-fold-replacement-face ((t (:inherit magit-diff-context-highlight))))
  (origami-fold-fringe-face ((t (:inherit magit-diff-context-highlight))))
  :config
  (face-spec-reset-face 'origami-fold-header-face)
  (with-eval-after-load 'hydra
    (define-key origami-mode-map (kbd "C-x f")
      (defhydra hydra-folding (:color red :hint nil)
        "
_o_pen node    _n_ext fold       toggle _f_orward    _F_ill column: %`fill-column
_c_lose node   _p_revious fold   toggle _a_ll        e_x_it
"
        ("o" origami-open-node)
        ("c" origami-close-node)
        ("n" origami-next-fold)
        ("p" origami-previous-fold)
        ("f" origami-forward-toggle-node)
        ("a" origami-toggle-all-nodes)
        ("F" fill-column)
        ("x" nil :color blue)))))

(use-package gamegrid
  :defer t
  :init
  (setf gamegrid-user-score-file-directory (locate-user-emacs-file "games")))

(use-package apt-sources-mode
  :defer t
  :mode "sources.list$")

(use-package pov-mode
  :ensure t
  :defer t
  :init
  (autoload 'irfc-mode "irfc" nil t)
  (autoload 'irfc-visit "irfc" nil t)
  (setf irfc-directory (locate-user-emacs-file "local/rfc")
        irfc-assoc-mode t)
  (mkdir irfc-directory t))

(use-package ospl-mode
  :init
  (autoload 'ospl-mode "ospl-mode"))

(use-package sql
  :init
  (setf sql-product 'sqlite))

(use-package enriched
  :config
  (define-key enriched-mode-map "\C-m" nil))

;; depends on slime and other modes
(use-package eval-in-repl
  :ensure t
  :hook ((cider-repl-mode . company-mode)
         (cider-mode . company-mode))
  :config
  ;;(add-hook 'lisp-mode-hook '(lambda () (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))
  ;; ielm support (for emacs lisp)
  (require 'eval-in-repl-ielm)
  ;; for .el files
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; for *scratch*
  (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; for M-x info
  (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm))


;; for interactively building regular expressions
(use-package re-builder
  :ensure t
  :config
  (setf reb-re-syntax 'read))

;; enable aggressive-indenting for some modes
;; see https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (css-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)))

;; visually display kill ring
;; see https://github.com/browse-kill-ring/browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :config
  (global-set-key "\C-cy" 'browse-kill-ring))

;; show vertical lines to guide indentation
;; see https://github.com/zk-phi/indent-guide
(use-package indent-guide
  :ensure t
  :hook ((emacs-lisp-mode . indent-guide-mode)
         (css-mode . indent-guide-mode)
         (lisp-mode . indent-guide-mode)
         (go-mode . indent-guide-mode)))

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
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

;; disable Fira Code for helm
(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
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

;; diable ligatures in helm
(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

;; On some systems, == will appear incorrectly as a blank space in certain modes
;; unless you add the following lines:

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; prettify symbols for various modes
(add-hook 'haskell-mode-hook 'my-add-pretty-lambda)
(add-hook 'shen-mode-hook 'my-add-pretty-lambda)
(add-hook 'tex-mode-hook 'my-add-pretty-lambda)
(global-prettify-symbols-mode 1) ; display “lambda” as “λ”

;; make tab complete without losing ability to manually indent
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; all-the-icons
;; see https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :defer 10
  :ensure t)

;; nice icons for dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :commands (all-the-icons-dired-mode))

;; file management: dired
(use-package dired-hacks-utils
  :defer t
  :ensure t)

(use-package dired-filter
  :defer t
  :ensure t)

(use-package dired-rainbow
  :defer t
  :ensure t)

(use-package dired-narrow
  :defer t
  :ensure t)

(use-package dired-collapse
  :defer t
  :ensure t)

;; for moving files around
(use-package sunrise-commander
  :defer 10
  :quelpa
  (sunrise-commander :fetcher github :repo "escherdragon/sunrise-commander")
  :bind ("<f7>" . sunrise))

;; neotree (file sidebar for navigation)
(use-package neotree
  :ensure t
  :commands (neotree)
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; reveal current file in osx finder
(use-package reveal-in-osx-finder
  :ensure t
  :bind ("C-c z" . reveal-in-osx-finder))

(use-package smart-tabs-mode
  :defer 10
  :ensure t
  :init
  (setq-default indent-tabs-mode nil
                indent-tabs-modetab-width 2)
  (smart-tabs-insinuate 'c 'cperl 'c++))

;; k8s interface
;; https://github.com/chrisbarrett/kubernetes-el
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(provide 'misc)

;;; misc.el ends here
