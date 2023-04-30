;;; package --- Summary

;;; Commentary:

;;; Code:

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; auto-indent when pasting
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; ansi term
(defadvice term-sentinel (around bnb/advise-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-zsh)
  (interactive (list "/usr/local/bin/zsh")))
(ad-activate 'ansi-term)

(defun bnb/term-mode-hook ()
  "Setup `term-mode`."
  (goto-address-mode)
  (setq-local term-buffer-maximum-size 10000))

(add-hook 'term-mode-hook 'bnb/term-mode-hook)

(defalias 'zsh 'ansi-term)

(setq-default ansi-term-color-vector
              [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white]
              ansi-color-faces-vector
              [default bold shadow italic underline bold bold-italic bold])

(provide 'init-utils)

;;; init-utils.el ends here
