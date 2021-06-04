;;; javascript-cfg.el --- enable javascript mode(s)

;;; Commentary:
;;; mostly taken from https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-js.el

;;; Code:

;;----------------------------------------------------------------------------
;; javascript settings
;;----------------------------------------------------------------------------

(use-package js2-mode
  :ensure t
  :mode (("\\.js$'" . js2-mode)
     ("\\.jsx$" . js2-jsx-mode))
  :hook (
     (js2-mode . (lambda () (setq mode-name "js2")))
     (js2-mode . #'js2-imenu-extras-mode) ;;; better imenu
     (js2-mode . (lambda ()
               (flycheck-mode)
               (my-tide-setup-hook)
               (company-mode)))
     (js2-jsx-mode . (lambda ()
               (flycheck-mode)
               (my-tide-setup-hook)
               (company-mode)))
     )
  :custom
  (js2-include-node-externs t)
  (js2-global-externs '("customElements"))
  (js2-highlight-level 3) ;;; Try to highlight most ECMA built-ins
  (js2r-prefer-let-over-var t)
  (js2r-prefered-quote-type 2)
  (js-indent-align-list-continuation t)
  (global-auto-highlight-symbol-mode t)
  :ensure-system-package ((prettier . "npm i -g prettier")
              (eslint . "npm i -g eslint")
              (eslint_d . "npm i -g eslint_d")
              (tern . "npm i -g tern"))
  :config
  (progn
    (setq js-indent-level 2
      js2-basic-offset 2
      js-chain-indent t)
    (setq flycheck-javascript-eslint-executable "eslint_d") ;;; use eslint_d insetad of eslint for faster linting
    (setq js2-mode-show-parse-errors t) ;;; turn off all warnings in js2-mode
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-strict-missing-semi-warning nil)
    (advice-add #'js2-identifier-start-p
        :after-until
        (lambda (c) (eq c ?#)))))

(use-package ac-js2
  :ensure t
  :hook
  (js2-mode . 'ac-js2-mode))

(use-package xref-js2
  :ensure t
  :after js2-mode
  :hook
  (js2-mode . (lambda ()
        (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :config
  (progn
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill)))

(use-package tern
  :ensure t
  :if (locate-file "tern" exec-path)
  :after js2-mode
  :hook (js-mode . tern-mode))

(use-package tern-auto-complete
  :after tern)

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
;;(define-key js-mode-map (kbd "M-.") nil)
(setq-default js2-additional-externs
          '("$" "unsafeWindow" "localStorage" "jQuery"
        "setTimeout" "setInterval" "location" "skewer"
        "console" "phantom"))

(use-package js2-refactor
  :ensure t
  :defer 30
  :after js2-mode
  :hook
  (js2-mode . #'js2-refactor-mode)
  :bind
  (:map js2-mode-map
    ("C-k" . js2r-kill))
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

;; json-mode: Major mode for editing JSON files with emacs
;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :ensure t
  :defer 20
  :custom
  (json-reformat:indent-width 2)
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode)
         ("\\.json\\'" . json-mode))
  :hook
  (json-mode . #'prettier-js-mode)
  :bind (:package json-mode-map
                  :map json-mode-map
                  ("C-c <tab>" . json-mode-beautify))
  :config
  (setq js-indent-level 2))

(use-package pug-mode
  :ensure t
  :mode ("\\.pug$'" . pug-mode))

;; jest: test helpers for javascript
;; https://github.com/Emiller88/emacs-jest/
(use-package jest
  :ensure t
  :after (js2-mode)
  :hook (js2-mode . jest-minor-mode))

;; npm client
;; https://github.com/shaneikennedy/npm.el
(use-package npm
  :after jest
  :ensure t)

(use-package flymake-eslint
  :ensure t
  :defer 10
  :custom ;; add glasses-mode to bolden capitals in CamelCase here. Could also be done elsewhere.
  (glasses-face (quote bold))
  (glasses-original-separator "")
  (glasses-separate-capital-groups t)
  (glasses-separate-parentheses-p nil)
  (glasses-separator "")
  :config
  (add-hook 'js-mode-hook (lambda () (flymake-eslint-enable)(flymake-mode -1)(flycheck-mode 1)(glasses-mode 1)))
  (add-hook 'js2-mode-hook (lambda () (flymake-eslint-enable)(flymake-mode -1)(flycheck-mode 1)(glasses-mode 1)))
  (custom-set-variables
   '(help-at-pt-timer-delay 0.3)
   '(help-at-pt-display-when-idle '(flymake-overlay))))

(use-package flymake-diagnostic-at-point
  :ensure t
  :defer 20
  :config
  (flymake-diagnostic-at-point-mode t))

;; eslintd-fix: Emacs minor-mode to automatically fix javascript with eslint_d.
;; https://github.com/aaronjensen/eslintd-fix/tree/master
(use-package eslintd-fix
  :ensure t)

;; indium: javascript ide
;; https://github.com/NicolasPetton/indium
;; (use-package indium
;;   :ensure t
;;   :hook
;;   ((js-mode . indium-interaction-mode)))

(provide 'javascript-cfg)

;;; javascript-cfg.el ends here
