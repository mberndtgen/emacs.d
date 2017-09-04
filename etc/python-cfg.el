;;; python-cfg.el - enable python

;;----------------------------------------------------------------------------
;; python settings
;;----------------------------------------------------------------------------
(use-package ein
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))

(use-package elpy
  :ensure t
  :init
  (defalias 'workon 'pyvenv-workon)
  (elpy-enable)
  :config
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    ;; For elpy
    (setq elpy-rpc-python-command "python3")
    ;; For interactive shell
    (setq python-shell-interpreter "python3")

    (setq elpy-use-cpython "/usr/local/bin/python3")
    (setq elpy-use-ipython "/usr/local/bin/ipython3")
    (elpy-use-ipython)))


(provide 'python-cfg)

;;; end of python-cfg.el
