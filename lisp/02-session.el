;;; 02-session.el -*- lexical-binding: t; -*-

;;; Code:

(require 'desktop)

(setq desktop-load-locked-desktop t
      desktop-restore-eager 5)

(setq desktop-files-not-to-save
      (concat "\\(?:"
              "\\`/Users/v236177/Library/CloudStorage/"
              "\\|\\.jsonl\\'"
              "\\)"))
;; Save desktop state, but perform restoration ourselves.
(setq desktop-save t)

(defun my/desktop-read-safely ()
  "Restore the desktop without making startup fail."
  (condition-case err
      (desktop-read)
    (error
     (display-warning
      'desktop
      (format "Desktop restoration failed: %S" err)
      :error))))

(add-hook 'after-init-hook #'my/desktop-read-safely)

(use-package savehist
  :config
  (setq history-length 25
        history-delete-duplicates t
        savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-mode 1))

(use-package saveplace
  :custom
  (save-place-file (expand-file-name ".places" user-emacs-directory))
  :init (setq-default save-place t))

(use-package recentf
  :custom
  (recentf-max-menu-items 25)
  :config
  (recentf-mode 1))

(provide '02-session)
