;;; javascript-cfg.el - enable javascript mode(s)
;;; mostly taken from https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-js.el

;;; Code:

;;----------------------------------------------------------------------------
;; javascript settings
;;----------------------------------------------------------------------------

(use-package js2-mode
  :mode (("\\.js$'" . js2-mode)
         ("\\.jsx$" . js2-jsx-mode))
  ;; :ensure-system-package ((prettier . "npm i -g prettier")
  ;;                         (eslint . "npm i -g eslint")
  ;;                         (eslint_d . "npm i -g eslint_d")
  ;;                         (tern . "npm i -g tern"))
  :config
  (progn
    (setq js-indent-level 2
          js2-basic-offset 2
          js-chain-indent t)
    ;; use eslint_d insetad of eslint for faster linting
    (setq flycheck-javascript-eslint-executable "eslint_d")

    ;; Try to highlight most ECMA built-ins
    (setq js2-highlight-level 3)

    ;; turn off all warnings in js2-mode
    (setq js2-mode-show-parse-errors t)
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-strict-missing-semi-warning nil)

    ;; use eslint_d insetad of eslint for faster linting
    (setq flycheck-javascript-eslint-executable "eslint_d")

    ;; Try to highlight most ECMA built-ins
    (setq js2-highlight-level 3)

    ;; turn off all warnings in js2-mode
    (setq js2-mode-show-parse-errors t)
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-strict-missing-semi-warning nil)

    (use-package ac-js2
      :ensure t
      :config
      (add-hook 'js2-mode-hook 'ac-js2-mode))

    (use-package xref-js2
      :ensure t
      :config
      (progn
        (js2r-add-keybindings-with-prefix "C-c C-r")
        (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
        (add-hook 'js2-mode-hook (lambda ()
                                   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))
    
    (use-package company-tern
      ;;; tern setup, see https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html
      :ensure t
      :config
      (progn
        ((add-to-list 'company-backends 'company-tern)
         (add-hook 'js2-mode-hook (lambda ()
                                    (tern-mode)
                                    (company-mode))))))

    (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode) ;;; better imenu
    (add-hook 'js2-mode-hook (lambda ()
			       (flycheck-mode)
			       (my-tide-setup-hook)
			       (company-mode)))
    (add-hook 'js2-jsx-mode (lambda ()
                              (flycheck-mode)
                              (my-tide-setup-hook)
                              (company-mode)))
    

    ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
    ;; unbind it.
    (define-key js-mode-map (kbd "M-.") nil)
    (setq-default js2-additional-externs
                  '("$" "unsafeWindow" "localStorage" "jQuery"
                    "setTimeout" "setInterval" "location" "skewer"
                    "console" "phantom"))))

    (use-package js2-refactor
      :after js2-mode
      :bind
      (:map js2-mode-map
            ("C-k" . js2r-kill)
            ("C-c h r" . js2-refactor-hydra/body))
      :ensure t
      :config
      (progn ((add-hook 'js2-mode-hook #'js2-refactor-mode)
              (js2r-add-keybindings-with-prefix "C-c C-r")
	      (defhydra js2-refactor-hydra (:color blue :hint nil)
    "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
    ("ee" js2r-expand-node-at-point)
    ("cc" js2r-contract-node-at-point)
    ("ef" js2r-extract-function)
    ("em" js2r-extract-method)
    ("tf" js2r-toggle-function-expression-and-declaration)
    ("ta" js2r-toggle-arrow-function-and-expression)
    ("ip" js2r-introduce-parameter)
    ("lp" js2r-localize-parameter)
    ("wi" js2r-wrap-buffer-in-iife)
    ("ig" js2r-inject-global-in-iife)
    ("ag" js2r-add-to-globals-annotation)
    ("ev" js2r-extract-var)
    ("iv" js2r-inline-var)
    ("rv" js2r-rename-var)
    ("vt" js2r-var-to-this)
    ("ao" js2r-arguments-to-object)
    ("ti" js2r-ternary-to-if)
    ("sv" js2r-split-var-declaration)
    ("ss" js2r-split-string)
    ("uw" js2r-unwrap)
    ("lt" js2r-log-this)
    ("dt" js2r-debug-this)
    ("sl" js2r-forward-slurp)
    ("ba" js2r-forward-barf)
    ("k" js2r-kill)
    ("q" nil)))))



;; prettier-emacs: minor-mode to prettify javascript files on save
;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :config
  (progn
    ((add-hook 'js2-mode #'prettier-js-mode)
     (add-hook 'js2-jsx-mode #'prettier-js-mode))))

;; json-snatcher: get the path of any JSON element easily
;; https://github.com/Sterlingg/json-snatcher
(use-package json-snatcher
  :config
  (progn
    ((add-hook 'json-mode #'js-mode-bindings)
     (defun js-mode-bindings ()
       "Sets a hotkey for using the json-snatcher plugin"
       (when (string-match  "\\.json$" (buffer-name))
	 (local-set-key (kbd "C-c C-g") 'jsons-print-path))))))

;; indium: javascript awesome development environment
;; https://github.com/NicolasPetton/indium
(use-package indium
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :config ((add-hook 'js2-mode #'indium-interaction-mode)))

;; mocha: emacs mode for running mocha tests
;; https://github.com/scottaj/mocha.el
(use-package mocha
  :after js2-mode
  :config
  (require 'typescript-mode)
  (dolist (m (list js2-mode-map typescript-mode-map))
    (bind-keys
     :map m
     ("C-c m P" . mocha-test-project)
     ("C-c m f" . mocha-test-file)
     ("C-c m p" . mocha-test-at-point))))

;; mocha-snippets: snippets for mocha test framework
;; https://github.com/cowboyd/mocha-snippets.el
(use-package mocha-snippets)

;; Adds the node_modules/.bin directory to the buffer exec_path. E.g. support project local eslint installations.
;; https://github.com/codesuki/add-node-modules-path/tree/master
(use-package add-node-modules-path
  :config
  (progn
    ((add-hook 'js2-mode #'add-node-modules-path)
     (add-hook 'js2-jsx-mode #'add-node-modules-path))))

;; json-mode: Major mode for editing JSON files with emacs
;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :config
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string? t)
  (setq js-indent-level 2))

;; eslintd-fix: Emacs minor-mode to automatically fix javascript with eslint_d.
;; https://github.com/aaronjensen/eslintd-fix/tree/master
(use-package eslintd-fix)

(provide 'javascript-cfg)

;;; javascript-cfg.el ends here
