(defkeys general
  "M-a" execute-extended-command
  "M-A" ansi-term
  "M-s" (lambda () (interactive) (other-window -1))
  "M-S" other-window
  "M-d" delete-region
  "M-f" delete-char
  "M-F" occur
  "M-g" kill-line
  "M-h" beginning-of-line
  "M-;" end-of-line
  "C-M-h" beginning-of-buffer
  "C-M-;" end-of-buffer
  "M-i" previous-line
  "M-j" backward-char
  "M-k" next-line
  "M-l" forward-char
  "M-n" scroll-down-command
  "M-m" scroll-up-command
  "M-N" beginning-of-buffer
  "M-M" end-of-buffer
  "M-x" kill-region
  "M-v" yank
  "M-c" kill-ring-save
  "C-b" switch-to-buffer)

(defkeys eshell
  "<return>" eshell-send-input
  "M-<return>" newline)

(defkeys ace-jump
  "M-SPC" ace-jump-mode)

(defkeys ace-window
  "M-w" ace-window)

(defkeys cider
  "M-<return>" cider-eval-last-sexp
  "C-M-d" cider-doc)

(defkeys cider-repl
  "M-<return>" cider-repl-return)

(defkeys coq
  "M-RET" coq-easy-indent)

(defkeys counsel
  "C-M-v" counsel-yank-pop)

(defkeys dired
  "M-i" dired-previous-line
  "M-k" dired-next-line)

(defkeys dired-subtree
  "<tab>" dired-subtree-cycle)

(defkeys hideshow-lisp
  "<tab>" (lambda nil (interactive) (hs-toggle-hiding) (backward-char)))

(defkeys hydra
  "C-c h" hydra-hydra/body)

(defkeys file
  "C-x M-f" find-file-root
  "C-x C-r" view-file)

(defkeys ivy
  "M-i" ivy-previous-line
  "M-j" backward-char
  "M-k" ivy-next-line
  "M-l" forward-char
  "M-u" ivy-scroll-down-command
  "M-m" ivy-scroll-up-command
  "M-U" ivy-beginning-of-buffer
  "M-M" ivy-end-of-buffer
  "M-v" yank)

(defkeys org
  "C-M-j" org-promote-subtree
  "C-M-l" org-demote-subtree
  "C-M-," org-priority
  "C-M-n" org-narrow-to-subtree
  "C-M-w" widen
  "C-M-e" org-set-effort
  "C-M-g" org-global-cycle
  "C-M-k" org-metadown
  "C-M-i" org-metaup
  "C-M-S-l" org-store-link
  "C-M-I" org-insert-link
  "C-M-v" org-paste-subtree
  "C-M-s" org-sort-entries
  "C-M-X" org-cut-subtree
  "C-M-t" org-todo)

(defkeys paredit
  "(" paredit-open-round
  ")" paredit-close-round
  "<backspace>" paredit-backward-delete
  "<return>" paredit-newline
  ";" paredit-semicolon
  "\\" paredit-backslash
  "[" paredit-open-square
  "]" paredit-close-square
  "\\" paredit-doublequote
  "C-M-i" paredit-raise-sexp
  "C-M-j" paredit-backward
  "C-M-k" paredit-wrap-sexp
  "C-M-l" paredit-forward
  "M-u" paredit-backward-down
  "M-U" paredit-backward-up
  "C-M-u" paredit-backward-up-all
  "M-o" paredit-forward-down
  "M-O" paredit-forward-up
  "C-M-o" paredit-forward-up-all
  "C-." paredit-forward-slurp-sexp
  "C-," paredit-backward-slurp-sexp
  "M-," paredit-forward-barf-sexp
  "M-." paredit-backward-barf-sexp
  "C-M-;" paredit-comment-dwim
  "M-/" paredit-split-sexp
  "C-/" paredit-splice-sexp)

(defkeys python-shell
  "<return>" newline
  "M-<return>" comint-send-input
  "C-M-i" comint-previous-input
  "C-M-k" comint-next-input)

(defkeys slime
  "C-c i" slime-hyperspec-lookup
  "C-c c" slime-complete-symbol
  "C-c d" slime-edit-definition
  "C-c D" slime-pop-find-definition-stack
  "C-c m" slime-macroexpand-1
  "C-c M" slime-macroexpand-all
  "C-c M-d" slime-disassemble-symbol
  "C-c M-g" slime-interrupt
  "C-c C-M-R" slime-restart-inferior-lisp
  "C-c p" slime-repl-set-package
  "C-c I" slime-inspect
  "M-<return>" slime-eval-last-expression)

(defkeys slime-repl
  "M-<return>" slime-repl-return
  "<tab>" indent-for-tab-command
  "M-I" slime-repl-previous-input
  "M-K" slime-repl-next-input)

(defkeys sexpr
  "M-p" sexpr-fix-parens
  "M-s" sexpr-edit-string-at-point)

(defkeys swiper
  "C-s" swiper)

(defkeys term
  "C-c C-t" term-toggle-mode)

(defkeys undo-tree-mode
  "M-z" undo
  "M-Z" redo)

(defkeys visual-regexp
  "C-M-s" vr/query-replace)

(defkeys with-editor-mode
  "C-c C-c" with-editor-finish
  "C-c C-k" with-editor-cancel)

(defkeys testkeys
  "M-b" (lambda () (interactive) (message "hello world")))
