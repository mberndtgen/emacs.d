;;; 05-swiper.el --- Swiper search and Counsel extras -*- lexical-binding: t; -*-

;;; Swiper uses Ivy internally.  Do not enable `ivy-mode' or `counsel-mode'
;;; globally — Helm remains the default completion framework.

;;; Code:

(use-package ivy
  :ensure t
  :config
  (setq ivy-wrap-around t
        ivy-use-virtual-buffers t))

(use-package swiper
  :after ivy
  :ensure t
  :bind (("C-s" . swiper)
         ("C-c S" . swiper))
  :config
  (setq swiper-action-recenter t
        swiper-include-line-number-in-search t))

(use-package counsel
  :after ivy
  :ensure t
  :bind (("C-x C-r" . counsel-recentf)
         ("s-E" . counsel-colors-emacs)
         ("s-W" . counsel-colors-web))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

(provide '05-swiper)
