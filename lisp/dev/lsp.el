;;; dev/lsp.el --- Archived LSP stack (opt-in) -*- lexical-binding: t; -*-
;; (defvar lsp-headerline-breadcrumb-segments)
;; (defun efs/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

;; (use-package lsp-mode
;;   :straight t
;;   :init
;;   (setq lsp-keymap-prefix "M-L") ; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   :commands (lsp lsp-deferred)
;;   :hook ((lsp-mode . efs/lsp-mode-setup)
;;          (lsp-mode . company-mode)
;;          (haskell-mode . lsp-deferred)
;;          (haskell-literate-mode . lsp-deferred)
;;          (lsp-managed-mode . lsp-modeline-diagnostics-mode)
;;          ;; if you want which-key integration
;;          (lsp-mode . (lambda () (lsp-enable-which-key-integration t)))
;;          )
;;   :config
;;   (lsp-enable-which-key-integration t)
;;   (add-hook 'hack-local-variables-hook (lambda () (when lsp-mode (lsp))))
;;   :custom
;;   (lsp-progress-via-spinner nil) ;; spinner seems to cause problems
;;   (lsp-restart 'ignore)
;;   (lsp-keep-workspace-alive nil)
;;   (lsp-headerline-breadcrumb-enable t)
;;   (lsp-headerline-breadcrumb-segments '(symbols))
;;   (lsp-lens-enable t)
;;   (lsp-enable-snippet nil)
;;   ;; :global/:workspace/:file
;;   (lsp-modeline-diagnostics-scope :workspace)
;;   (lsp-file-watch-threshold 2000)
;;   (lsp-completion-provider :capf))


;; ;; provides fancier overlays.
;; (use-package lsp-ui
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :straight t
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-peek-enable t)
;;   (lsp-ui-peek-show-directory t)
;;   (lsp-ui-doc-enable t)
;;   ;; You might want this:
;;   ;; (lsp-ui-doc-show-with-cursor nil)
;;   ;; Also this because isearch gets broken otherwise
;;   ;; (lsp-ui-doc-show-with-mouse nil)
;;   (lsp-ui-doc-position 'top)
;;   (lsp-ui-imenu-window-width 20)
;;   ;;   (lsp-ui-imenu-enable t)
;;   ;;   (lsp-ui-imenu-kind-position 'top)
;;   ;;   (lsp-ui-sideline-show-diagnostics t)
;;   ;;   (lsp-ui-sideline-show-hover t)
;;   ;;   (lsp-ui-sideline-show-code-actions t)
;;   ;;   (lsp-ui-sideline-update-mode 'line)
;;   ;;   (lsp-ui-sideline-delay 0.5)
;;   ;;   (lsp-ui-sideline-enable t)
;;   ;;   (lsp-ui-imenu-enable t)
;;   ;;   (lsp-ui-flycheck-enable t)
;;   ;;   (lsp-ui-doc-enable nil)
;;   ;;   (lsp-ui-doc-delay '2)
;;   :config
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-reference))


;; xref
;; (use-package xref
;;   :pin gnu
;;   :bind (("s-r" . #'xref-find-references)
;;          ("s-[" . #'xref-go-back)
;;          ("C-<down-mouse-2>" . #'xref-go-back)
;;          ("s-]" . #'xref-go-forward)))

;; ;; eldoc
;; (use-package eldoc
;;   :pin gnu
;;   :diminish
;;   :bind ("s-d" . #'eldoc)
;;   :custom (eldoc-echo-area-prefer-doc-buffer t))

;; ;; eglot
;; ;; https://github.com/joaotavora/eglot
;; (use-package eglot
;;   :bind
;;   ("H-p ." . eglot-help-at-point)
;;   :hook
;;   (go-mode . eglot-ensure)
;;   (haskell-mode . eglot-ensure)
;;   :bind (:map eglot-mode-map
;;               ("C-c a r" . #'eglot-rename)
;;               ("C-<down-mouse-1>" . #'xref-find-definitions)
;;               ("C-S-<down-mouse-1>" . #'xref-find-references)
;;               ("C-c C-c" . #'eglot-code-actions))
;;   :custom
;;   (eglot-autoshutdown t))

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

;; Uncomment blocks above, then add "lsp" to my/dev-modules in local.el.
