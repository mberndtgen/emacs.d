;;; 04-german.el --- German umlauts via Karabiner -*- lexical-binding: t; -*-

;;; Karabiner fn+letter -> Hyper; lowercase omits shift, uppercase adds it.
;;; Emacs treats C-M-S-a and C-M-S-A as the same key, so lower/upper must differ:
;;;   fn+a       -> C-M-a   ->  ä
;;;   fn+Shift+a -> C-M-S-a ->  Ä
;;; Same pattern for o/O, u/U.  fn+s -> C-M-s -> ß.
;;;
;;; Debug: M-x my/describe-next-key RET

;;; Code:

(defun my/insert-german-char (char)
  "Insert German character CHAR."
  (interactive)
  (insert char))

(defun my/bind-german-char (char keys)
  "Bind KEYS to insert CHAR."
  (let ((command (lambda () (interactive) (my/insert-german-char char))))
    (dolist (key keys)
      (global-set-key (kbd key) command))))

;; Lowercase: C-M-* (fn+key).  Uppercase: C-M-S-* (fn+Shift+key).
(my/bind-german-char ?ä '("C-M-a"))
(my/bind-german-char ?Ä '("C-M-S-a"))
(my/bind-german-char ?ö '("C-M-o"))
(my/bind-german-char ?Ö '("C-M-S-o"))
(my/bind-german-char ?ü '("C-M-u"))
(my/bind-german-char ?Ü '("C-M-S-u"))
(my/bind-german-char ?ß '("C-M-s"))

(defun my/describe-next-key ()
  "Describe the next key Emacs receives (for Karabiner / modifier debugging)."
  (interactive)
  (message "Press the key to inspect (3s timeout)...")
  (let ((event (read-event "" nil 3)))
    (if event
        (message "Key: %s | Raw: %S | Modifiers: %s — add (kbd \"%s\") to 04-german.el if needed"
                 (key-description (vector event))
                 event
                 (event-modifiers event)
                 (key-description (vector event)))
      (message "No key received within 3 seconds."))))

(provide '04-german)
