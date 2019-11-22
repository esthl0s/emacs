;; -*- lexical-binding: t; -*-

(require 'cl)

;; two variables
;; keys-bindings, which is global
;; keys-bidings-buffer, which is buffer local

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
			 ("M-v" . yank)
			 ("C-b" . switch-to-buffer))
	(ace-window ("M-w" . ace-window))
	(cider ("M-<return>" . cider-eval-last-sexp)
		   ("C-M-d" . cider-doc))
	(cider-repl ("M-<return>" . cider-repl-return))
	(dired ("M-i" . dired-previous-line)
		   ("M-k" . dired-next-line))
	(dired-subtree ("<tab>" . dired-subtree-cycle))
	(hideshow-lisp ("<tab>" . (lambda () (interactive)
								(hs-toggle-hiding)
								(backward-char))))
	(hydra ("C-c h" . hydra-hydra/body))
	(file ("C-x M-f" . find-file-root)
		  ("C-x C-r" . view-file))
	(org ("C-M-j" . org-promote-subtree)
		 ("C-M-l" . org-demote-subtree)
		 ("C-M-," . org-priority)
		 ("C-M-n" . org-narrow-to-subtree)
		 ("C-M-w" . widen)
		 ("C-M-e" . org-set-effort)
		 ("C-M-g" . org-global-cycle)
		 ("C-M-k" . org-metadown)
		 ("C-M-i" . org-metaup)
		 ("C-M-S-l" . org-store-link)
		 ("C-M-I" . org-insert-link)
		 ("C-M-v" . org-paste-subtree)
		 ("C-M-s" . org-sort-entries)
		 ("C-M-X" . org-cut-subtree)
		 ("C-M-t" . org-todo))
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
	(slime ("C-c i" . slime-hyperspec-lookup)
		   ("C-c c" . slime-complete-symbol)
		   ("C-c d" . slime-edit-definition)
		   ("C-c D" . slime-pop-find-definition-stack)
		   ("C-c m" . slime-macroexpand-1)
		   ("C-c M" . slime-macroexpand-all)
		   ("C-c M-d" . slime-disassemble-symbol)
		   ("C-c M-g" . slime-interrupt)
		   ("C-c C-M-R" . slime-restart-inferior-lisp)
		   ("C-c p" . slime-repl-set-package)
		   ("C-c I" . slime-inspect)
		   ("M-<return>" . slime-eval-last-expression))
	(slime-repl ("M-<return>" . slime-repl-return)
				("<tab>" . indent-for-tab-command)
				("M-I" . slime-repl-previous-input)
				("M-K" . slime-repl-next-input))
	(sexpr ("M-p" . sexpr-fix-parens)
		   ("M-s" . sexpr-edit-string-at-point))
	(undo-tree-mode ("M-z" . undo)
					("M-Z" . redo))
	(with-editor-mode ("C-c C-c" . with-editor-finish)
					  ("C-c C-k" . with-editor-cancel))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;; (defun keys|list-buffer-bindings ()
;;   (interactive))



(provide 'keys)
