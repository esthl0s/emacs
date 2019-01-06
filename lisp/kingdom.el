;; -*- lexical-binding: t; -*-

(defvar keys-bindings
  '((general ("M-i" . previous-line)
             ("M-j" . backward-char)
             ("M-k" . next-line)
             ("M-l" . forward-char)
             ("M-n" . beginning-of-buffer)
             ("M-N" . end-of-buffer)
             ("M-c" . kill-ring-save)
             ("M-v" . yank)
             ("M-h" . beginning-of-line)
             ("M-H" . end-of-line))
    (paredit ("C-M-i" . paredit-raise-sexp)
             ("C-M-j" . paredit-backward)
             ("C-M-k" . paredit-wrap-sexp)
             ("C-M-l" . paredit-forward)
             ("M-u" . paredit-backward-down)
             ("M-U" . paredit-backward-up)
             ("M-o" . paredit-forward-down)
             ("M-O" . paredit-forward-up))
    (undo-tree-mode ("M-z" . undo)
                    ("M-Z" . redo))))

(defun keys-extend-keymap (keymap request-list)
  (defun establish-binding (binding-pair)
    (define-key keymap (kbd (car binding-pair)) (cdr binding-pair)))

  (defun establish-bindings-if-requested (binding-set)
    (if (member (car binding-set) request-list)
        (mapcar (function (lambda (binding-pair)
                            (establish-binding keymap binding-pair)))
                (cdr binding-set))))

  (mapcar 'establish-bindings-if-requested
          kingdom-bindings))


(provide 'keys)
