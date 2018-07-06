;;; scala-cfg.el - enable scala / sbt

;;----------------------------------------------------------------------------
;; scala settings
;;----------------------------------------------------------------------------

(use-package ensime
  :ensure t
  :commands ensime
  :init
  (setq-default ensime-startup-notification nil)
  (setq-default ensime-startup-snapshot-notification nil))

(use-package sbt-mode)

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'")

(provide 'scala-cfg)

;;; end of scala-cfg.el
