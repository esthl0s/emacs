(define-minor-mode lisperior-mode
  "A mode for the superior language"
  :init-value nil
  :lighter " ^"
  (font-lock-mode 1)
  (setq indent-tabs-mode nil)
  (if (featurep 'smart-tabs-mode)
      (smart-tabs-mode -1))
  (show-paren-mojde 1)
  (whitespace-mode 1)
  (aggressive-indent-mode 1)
  (hl-line-mode -1)
  (paredit-mode 1))

(define-minor-mode lisperior-show-types-mode
  "Show lisp types"
  :init-value nil
  :lighter " lisperior-show-types"
  :keymap (let ((k (make-sparse-keymap)))
            (define-key k (kbd "C-c C-c")
              (lambda ()
                (interactive)
                (kill-buffer)
                (delete-window)))
            k))

(defun lisperior-show-types ()
  (interactive)
  (select-window (split-window nil nil 'below 'norecord))
  (switch-to-buffer (generate-new-buffer "Lisp types"))
  (lisperior-show-types-mode)
  (insert "  _____________________________________________
 |                                             |
 |          Sequence                           |
 |  ______   ________________________________  |
 | |      | |                                | |
 | | List | |             Array              | |
 | |      | |    ________       ________     | |
 | |______| |   |        |     |        |    | |
 |          |   | Vector |     | String |    | |
 |          |   |________|     |________|    | |
 |          |  ____________   _____________  | |
 |          | |            | |             | | |
 |          | | Char-table | | Bool-vector | | |
 |          | |____________| |_____________| | |
 |          |________________________________| |
 |_____________________________________________|"))

(provide 'lisperior)
