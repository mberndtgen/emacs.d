;;; package --- Set up python mode

;;; Commentary:

;;; Code:

(use-package ein
  :ensure t)


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
                                        ;(setq elpy-use-ipython "/usr/local/bin/ipython3")
                                        ;(elpy-use-ipython)
    ))

(use-package anaconda-mode
  ;; see https://github.com/pythonic-emacs/anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)))

(provide 'python-cfg)

;;; python-cfg.el ends here
