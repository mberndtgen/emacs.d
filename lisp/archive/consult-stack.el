;;; archive/consult-stack.el -*- lexical-binding: t; -*-
;; Enable vertico
;; see https://github.com/minad/vertico
;; (use-package vertico
;;   :ensure t
;;   :bind (:map vertico-map
;;               ("C-j" . vertico-next)
;;               ("C-k" . vertico-previous)
;;               ("C-f" . vertico-exit)
;;               :map minibuffer-local-map
;;               ("M-h" . dw/minibuffer-backward-kill))
;;   :custom
;;   (vertico-cycle t) ; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;;   (vertico-scroll-margin 0) ; Different scroll margin
;;   (vertico-count 20) ; Show more candidates
;;   (vertico-resize t) ; Grow and shrink the Vertico minibuffer
;;   :custom-face
;;   (vertico-current ((t (:background "#3a3f5a"))))
;;   :init
;;   (vertico-mode))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; tab widths
(setq-default tab-width 2) ; Default to an indentation size of 2 spaces
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs for indentation

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  :init
  (setq completion-category-defaults nil))


(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

;;Consult provides a lot of useful completion commands similar to Ivy’s Counsel.
(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

;; Switching Directories with consult-dir
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function nil))

;; Thanks Karthik!
(with-eval-after-load 'eshell-mode
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                   :narrow ?e
                                                   :category file
                                                   :face consult-file
                                                   :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs))))))))


(use-package marginalia
  :ensure t
  :after vertico
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
;; Swiper + Counsel extras live in lisp/05-swiper.el
