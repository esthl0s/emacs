;; -*- lexical-binding: t; -*-



(make-local-variable 'keys-buffer-binding-sets)
(setq-default keys-buffer-binding-sets '())

(defvar keys-bindings
  '((general ("M-a" . execute-extended-command)
             ("M-A" . ansi-term)
             ("M-s" . (lambda () (interactive) (other-window -1)))
             ("M-S" . other-window)
             ("M-d" . delete-region)
             ("M-f" . delete-char)
             ("M-F" . occur)
             ("M-g" . kill-line)
             ("M-h" . beginning-of-line)
             ("M-;" . end-of-line)
             ("C-M-h" . beginning-of-buffer)
             ("C-M-;" . end-of-buffer)
             ("M-i" . previous-line)
             ("M-j" . backward-char)
             ("M-k" . next-line)
             ("M-l" . forward-char)
             ("M-n" . beginning-of-buffer)
             ("M-N" . end-of-buffer)
             ("M-x" . kill-region)
             ("M-c" . kill-ring-save)
             ("M-v" . yank))
    (ace-window ("M-w" . ace-window))
    (org ("C-c <" . org-promote-subtree)
         ("C-c >" . org-demote-subtree)
         ("C-c ," . org-set-priority)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c C" . org-columns)
         ("C-c e" . org-set-effort)
         ("C-c j" . outline-next-visible-heading)
         ("C-c k" . outline-previous-visible-heading)
         ("C-c l" . org-store-link)
         ("C-c i" . org-insert-link)
         ("C-c p" . org-paste-subtree)
         ("C-c s" . org-sort-entries)
         ("C-c y" . org-cut-subtree)
         ("C-c t" . org-todo)
         ("C-c h" . org-toggle-hold))
    (paredit ("(" . paredit-open-round)
             (")" . paredit-close-round)
             ("<backspace>" . paredit-backward-delete)
             ("<return>" . paredit-newline)
             (";" . paredit-semicolon)
             ("\\" . paredit-backslash)
             ("[" . paredit-open-square)
             ("]" . paredit-close-square)
             ("\"" . paredit-doublequote)
             ("C-M-i" . paredit-raise-sexp)
             ("C-M-j" . paredit-backward)
             ("C-M-k" . paredit-wrap-sexp)
             ("C-M-l" . paredit-forward)
             ("M-u" . paredit-backward-down)
             ("C-M-u" . paredit-backward-up)
             ("M-o" . paredit-forward-down)
             ("C-M-o" . paredit-forward-up)
             ("C-." . paredit-forward-slurp-sexp)
             ("C-," . paredit-backward-slurp-sexp)
             ("M-," . paredit-forward-barf-sexp)
             ("M-." . paredit-backward-barf-sexp)
             ("C-M-;" . paredit-comment-dwim)
             ("M-/" . paredit-split-sexp)
             ("C-/" . paredit-splice-sexp))
    (slime ("C-c h" . slime-documentation-lookup)
           ("C-c c" . slime-complete-symbol)
           ("C-c d" . slime-edit-definition)
           ("C-c D" . slime-pop-find-definition-stack)
           ("C-c m" . slime-macroexpand-1)
           ("C-c M" . slime-macroexpand-all)
           ("C-c M-d" . slime-disassemble-symbol)
           ("C-c M-g" . slime-interrupt)
           ("C-c C-M-R" . slime-restart-inferior-lisp)
           ("C-c p" . slime-repl-set-package)
           ("C-c i" . slime-inspector))
    (sexpr ("M-p" . sexpr-fix-parens)
           ("M-s" . sexpr-edit-string-at-point))
    (undo-tree-mode ("M-z" . undo)
                    ("M-Z" . redo))))

;; So, you want to MAKE SURE that your keybindings stick.
;; Well, emacs is not so cooperative here. emacs really wants
;; to do whatever its minor modes want. maybe you think,
;; "hey, there's this overriding-local-map thing that is
;; oddly not buffer-local. I'll just change it to be buffer
;; local and I'll be good." WRONG. Doing that will fuck your
;; shit up. Below is a better solution that works the vast
;; majority of the time. We simply stop any minor modes from
;; impacting the local key bindings. The downside is that you
;; have to bind everything yourself, but you knew that, right?
(defun kill-minor-mode-maps ()
  (interactive)
  (let ((km (make-sparse-keymap)))
    (setq minor-mode-overriding-map-alist
          (mapcar (lambda (x) (cons x km))
                  (mapcar (function car) minor-mode-map-alist)))))

(defun kill-minor-mode-maps-if-needed ()
  (if (not minor-mode-overriding-map-alist)
      (kill-minor-mode-maps)))

(defun keys-establish-binding (keymap binding-pair)
  (define-key keymap (kbd (car binding-pair)) (cdr binding-pair)))

(defun keys-establish-bindings-if-requested (request-list keymap binding-set)
  (if (member (car binding-set) request-list)
      (mapcar (function (lambda (binding-pair)
                          (keys-establish-binding keymap binding-pair)))
              (cdr binding-set))))

(defun keys-extend-keymap (keymap request-list)
  (mapcar (lambda (binding-set) (keys-establish-bindings-if-requested request-list keymap binding-set))
          keys-bindings))

(defun keys-establish-local-binding (binding-pair)
  (local-set-key (kbd (car binding-pair)) (cdr binding-pair)))

(defun keys-establish-local-bindings-if-requested (request-list binding-set)
  (if (member (car binding-set) request-list)
      (mapcar (function (lambda (binding-pair)
                          (keys-establish-local-binding binding-pair)))
              (cdr binding-set))))

(defun keys-extend-local-keymap (request-list)
  (kill-minor-mode-maps-if-needed)
  (mapcar (lambda (binding-set) (keys-establish-local-bindings-if-requested request-list binding-set))
          keys-bindings)
  (setq keys-buffer-binding-sets
        (remove-duplicates
         (append keys-buffer-binding-sets request-list))))

(defun keys-apply-local-bindings ()
  (interactive)
  (kill-minor-mode-maps-if-needed)
  (keys-extend-local-keymap keys-buffer-binding-sets)
  (message "Applied binding sets: ~a..." keys-buffer-binding-sets))

(provide 'keys)
