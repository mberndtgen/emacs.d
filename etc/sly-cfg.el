;;; sly-cfg.el - enable sly

;;----------------------------------------------------------------------------
;; sly settings
;;----------------------------------------------------------------------------

(use-package sly
  :ensure t
  :init
  (progn
    (require 'sly-autoloads)
    (setf inferior-lisp-program "/usr/local/bin/sbcl"))
  :config
  (progn
    (eval-after-load 'sly
      `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))
    (eval-after-load 'sly-mrepl
      `(define-key sly-mrepl-mode-map (kbd "C-c C-k")
         'sly-mrepl-clear-recent-output))

    (use-package sly-company
      :defer t
      :ensure t
      :init
      (add-hook 'sly-mode-hook #'sly-company-mode))

    (add-hook 'sly-mode-hook
              (lambda ()
                (unless (sly-connected-p)
                  (save-excursion (sly)))))
    ))

(provide 'sly-cfg)

;;; end of sly-cfg.el
