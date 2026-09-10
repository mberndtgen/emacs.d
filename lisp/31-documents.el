;;; 31-documents.el --- Markdown, PDF, diagrams -*- lexical-binding: t; -*-

;;; Code:

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
(use-package gnuplot-mode
  :defer t)

(use-package graphviz-dot-mode
  :defer t
  :config
  (setf graphviz-dot-indent-width 2
        graphviz-dot-auto-indent-on-semi nil))
(provide '31-documents)
