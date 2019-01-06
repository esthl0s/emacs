;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general sexpr-mode procedures

(defun sexpr-get-region-content ()
  (buffer-substring (mark) (point)))

(defun sexpr-open-edit-buffer (&optional
                               initial-contents
                               mode-function
                               incoming-content-filter
                               outgoing-content-filter)
  (let ((parent-major-mode major-mode)
        (parent-window (selected-window))
        (edit-buffer (generate-new-buffer "sexpr-edit-buffer")))
    (setq sexpr-edit-buffer edit-buffer)
    (setq sexpr-outgoing-content-filter outgoing-content-filter)
    (select-window (split-window nil nil 'below) 'norecord)
    (switch-to-buffer (generate-new-buffer "sexpr-string-edit"))
    (if mode-function
        (funcall mode-function)
      (funcall parent-major-mode))
    (sexpr-edit-mode)
    (setq sexpr-parent-window parent-window)
    (if initial-contents
        (if incoming-content-filter
            (insert (funcall incoming-content-filter initial-contents))
          (insert initial-contents)))))

(defun sexpr-insert-edit-buffer-contents-and-kill ()
  (interactive)
  (let ((string-to-insert (buffer-string))
        (edit-window (selected-window)))
    (select-window sexpr-parent-window)
    (delete-window edit-window)
    (kill-buffer sexpr-edit-buffer)
    (kill-region (mark) (point))
    (insert (if sexpr-outgoing-content-filter
                (funcall sexpr-outgoing-content-filter string-to-insert)
              string-to-insert))))

(defvar sexpr-edit-mode-map
  (let ((k (make-sparse-keymap)))
    (define-key k (kbd "C-c C-c")
      'sexpr-insert-edit-buffer-contents-and-kill)
    k))

(define-minor-mode sexpr-edit-mode
  "Minor mode for sexpr edit buffers."
  :init-value nil
  :lighter " sexpr-edit"
  :keymap sexpr-edit-mode-map
  (make-local-variable 'sexpr-parent-window)
  (make-local-variable 'sexpr-outgoing-content-filter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string editing in a separate buffer

(defun sexpr-move-to-beginning-of-string ()
  (let ((done))
    (while (not done)
      (if (eq (preceding-char) ?\")
          (progn
            (backward-char)
            (if (eq (preceding-char) ?\\)
                (backward-char)
              (progn
                (setq done t)
                (forward-char))))
        (backward-char)))))

(defun sexpr-move-to-end-of-string ()
  (let ((done))
    (while (not done)
      (if (eq (char-after) ?\\)
          (forward-char 2)
        (if (eq (char-after) ?\")
            (setq done t)
          (forward-char))))))

(defun sexpr-select-string-at-point ()
  (sexpr-move-to-beginning-of-string)
  (set-mark-command nil)
  (sexpr-move-to-end-of-string))

(defun sexpr-get-string-at-point ()
  (sexpr-select-string-at-point)
  (sexpr-get-region-content))

(defun sexpr-unescape-quotes (s)
  (let ((pos 0)
        (out '()))
    (while (< pos (length s))
      (if (not (eq (aref s pos) ?\\))
          (setq out (cons (aref s pos) out)))
      (incf pos))
    (concat (nreverse out))))

(defun sexpr-escape-quotes (s)
  (let ((pos 0)
        (out '()))
    (while (< pos (length s))
      (if (eq (aref s pos) ?\")
          (setq out (cons ?\" (cons ?\\ out)))
        (setq out (cons (aref s pos) out)))
      (incf pos))
    (concat (nreverse out))))

(defun sexpr-edit-string-at-point ()
  (interactive)
  (sexpr-open-edit-buffer (sexpr-get-string-at-point)
                          (lambda () (text-mode))
                          #'sexpr-unescape-quotes
                          #'sexpr-escape-quotes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sub-sexpr editing in a separate buffer

(defun sexpr-get-sexpr-at-point ()
  (mark-sexp)
  (buffer-substring (mark) (point)))

(defun sexpr-edit-sexpr-at-point ()
  (interactive)
  (sexpr-open-edit-buffer (sexpr-get-sexpr-at-point)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sexpr mode declaration and map

(defvar sexpr-mode-map
  (let ((k (make-sparse-keymap)))
    (define-key k (kbd "C-c t") 'sexpr-edit-string-at-point)
    (define-key k (kbd "C-c s") 'sexpr-edit-sexpr-at-point)
    k))

(define-minor-mode sexpr-mode
  "Enhanced sexpr editing mode."
  :init-value nil
  :lighter " sexpr"
  :keymap sexpr-mode-map
  (make-local-variable 'sexpr-edit-buffer))

(provide 'sexpr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get your parens back on the right line!

(defun sexpr-fix-parens ()
  (interactive)
  (let ((whitespace (list ?\s ?\t ?\n ?\f)))
    (while (memq (char-after) whitespace)
      (forward-char))
    (push-mark)
    (while (memq (char-before) whitespace)
      (backward-char))
    (kill-region (point) (mark))
    (pop-mark)))
