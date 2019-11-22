;; The init sexpr is an alist. Each key K is a list of features, and
;; each value V is a list of sexprs. When all features in K are
;; available, the sexprs in V are evaluated in order.
(defvar
  *init-sexpr*
  '(((emacs)
	 ;; look and feel
	 (menu-bar-mode -1)
	 (tool-bar-mode -1)
	 (display-battery-mode -1)
	 (setq inhibit-splash-screen t)
	 (prefer-coding-system 'us-ascii)
	 (global-font-lock-mode 1)
	 (setq initial-scratch-message "")
	 (set-scroll-bar-mode 'nil)
	 (setq-default indent-tabs-mode t)
	 (setq indent-line-function 'insert-tab)
	 (setq electric-indent-mode 1)
	 (setq delete-selection-mode 1)
	 (setq sentence-end-double-space nil)
	 (setq tab-stop-list '(0 3))
	 (setq fill-column 80)
	 (setq-default tab-width 4)
	 (setq backup-by-copying t)
	 (setq read-quoted-char-radix 10)
	 (global-set-key (kbd "<C-S-M-right>") 'shrink-window-horizontally)
	 (global-set-key (kbd "<C-S-M-left>") 'enlarge-window-horizontally)
	 (global-set-key (kbd "<C-S-M-down>") 'shrink-window)
	 (global-set-key (kbd "<C-S-M-up>") 'enlarge-window)
	 (global-set-key (kbd "C-'") 'comment-or-uncomment-region)
	 (setq mail-host-address "esthlos.com")
	 (setq browse-url-browser-function 'browse-url-firefox)
	 ;; load paths
	 (add-to-list 'load-path
				  (expand-file-name "lisp/"
									user-emacs-directory))
	 ;; load the theme
	 (setq custom-theme-directory
		   (expand-file-name "themes/"
							 user-emacs-directory))
	 ;; set the backup directory
	 (setq backup-directory-alist
		   `(("." . ,(expand-file-name "saves"
									   user-emacs-directory))))
	 (setq default-directory "~/")
	 ;; easy add to hook set
	 (defun do-for-hooks-in-list (hook-list function)
	   (dolist (hook hook-list)
		 (add-hook hook function)))
	 (defvar lisp-mode-hooks '(common-lisp-mode-hook
							   clojure-mode-hook
							   cider-repl-mode-hook
							   emacs-lisp-mode-hook
							   lisp-mode-hook
							   scheme-mode-hook
							   slime-repl-mode-hook))
	 (defvar programming-modes-list (append lisp-mode-hooks
											'(ada-mode
											  html-mode-hook
											  java-mode-hook
											  tex-mode-hook
											  shell-script-mode-hook
											  c-mode-hook
											  python-mode-hook
											  puppet-mode-hook
											  text-mode-hook
											  js-mode-hook
											  css-mode-hook))))
	((package)
	 (require 'package)
	 (package-initialize)
	 (add-to-list 'package-archives
				  (cons "melpa" "http://melpa.milkbox.net/packages/")
				  t)
	 ;; (add-to-list 'package-archives
	 ;;				  (cons "melpa" "http://melpa.org/packages/")
	 ;;				  t)
	 )
	((ace-window keys)
	 (keys-extend-keymap global-map '(ace-window)))
	((ada-mode)
	 (mapcar (lambda (x) (add-to-list 'auto-mode-alist (cons x 'ada-mode)))
			 (list "\\.gpr\\'" "\\.ads\\'" "\\.adb\\'")))
	((aggressive-indent)
	 (do-for-hooks-in-list lisp-mode-hooks
						   (lambda () (aggressive-indent-mode 1))))
	((aggressive-indent diminish)
	 (diminish 'aggressive-indent-mode))
	((ascii))
	((autorevert diminish)
	 (diminish 'auto-revert-mode))
	((calendar)
	 (setq calendar-latitude 41.2)
	 (setq calendar-longitude -73.7)
	 (setq calendar-location-name "Armonk, NY"))
	((calfw))
	((calfw-org))
	((cc-mode)
	 (do-for-hooks-in-list '(c-mode-hook
							 java-mode-hook)
						   (lambda ()
							 (setq indent-tabs-mode t)
							 (aggressive-indent-mode 1))))
	((cider)
	 ;; to remove errors, which fuck with aggressive indent
	 (defun cider-repl-emit-stderr (buffer string))
	 ;; to remove the terrible huge map in overtone
	 (setq cider-print-fn nil)
	 (add-hook 'cider-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(cider)))))
	((cider-repl)
	 (setq cider-repl-display-help-banner nil)
	 (add-hook 'cider-repl-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(cider-repl)))))
	((company)
	 (do-for-hooks-in-list programming-modes-list
						   (lambda () (company-mode)))
	 (add-hook 'company-mode-hook
			   (lambda ()
				 (setq company-idle-delay 0))))
	((company diminish)
	 (diminish 'company-mode))
	((proof)
	 (defun coq-easy-indent ()
	   (interactive)
	   (let* ((spaces-indented 0)
			  (starts-with-pipe
			   (save-excursion
				 (beginning-of-line)
				 (back-to-indentation)
				 (setq spaces-indented (current-column))
				 (eq ?\| (char-after (point))))))
		 (newline)
		 (if starts-with-pipe
			 (progn (dotimes (x spaces-indented)
					  (insert " "))
					(insert "| ")))))
	 (add-hook 'coq-mode-hook
			   (lambda ()
				 (local-set-key (kbd "M-RET") 'coq-easy-indent))))
	((proof keys)
	 (add-hook 'coq-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(general)))))
	((custom)
	 (setq custom-theme-directory
		   (expand-file-name "themes/"
							 user-emacs-directory))
	 (setq custom-file
		   (expand-file-name "custom.el"
							 user-emacs-directory))
	 (load custom-file))
	((define-word))
	((dired)
	 (add-hook 'dired-mode-hook
			   (lambda ()
				 (dired-hide-details-mode 1))))
	((dired keys)
	 (add-hook 'dired-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(dired)))))
	((dired hydra)
	 (add-hook 'dired-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(hydra)))))
	((dired-subtree keys)
	 (add-hook 'dired-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(dired-subtree)))))
	((dockerfile-mode)
	 (add-hook 'dockerfile-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(general)))))
	((elisp-mode)
	 (add-hook 'emacs-lisp-mode-hook
			   (lambda ()
				 (font-lock-mode 1)
				 (show-paren-mode 1))))
	((eldoc diminish)
	 (diminish 'eldoc-mode))
	((eshell)
	 (add-hook 'eshell-mode-hook
			   (lambda ()
				 (set-buffer-process-coding-system 'utf-8-unix
												   'utf-8-unix)))
	 (defun eshell-new ()
	   (interactive)
	   (eshell 'N)))
	((fill-column-indicator)
	 (setq fci-rule-use-dashes t)
	 (setq fci-rule-color "blue")
	 (do-for-hooks-in-list programming-modes-list
						   (lambda () (fci-mode))))
	((font-core)
	 (load-file (expand-file-name "config.local/font.el"
								  user-emacs-directory))
	 (add-to-list 'default-frame-alist
				  `(font . ,myfont))
	 (set-default-font myfont)
	 (load-theme 'ate-light t))
	((geiser paredit)
	 (add-hook 'geiser-mode-hook #'paredit-mode))
	((geiser undo-tree)
	 (add-hook 'geiser-mode-hook #'undo-tree-mode))
	((hexl)
	 (add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode)))
	((hideshow)
	 (do-for-hooks-in-list programming-modes-list
						   (lambda () (hs-minor-mode 1)))
	 ;; disabling for now
	 ;; (do-for-hooks-in-list lisp-mode-hooks
	 ;;						   (lambda () (keys-extend-local-keymap '(hideshow-lisp))))
	 )
	((hideshow diminish)
	 (diminish 'hs-minor-mode))
	((hideshow hydra)
	 (defhydra hydra-hideshow (:foreign-keys run)
	   "
^Global^           ^Local^
^^^^^^^----------------------------
_H_ hs-hide-all   _h_ hs-hide-block
_S_ hs-show-all   _s_ hs-show-block
				  _l_ hs-hide-level
"
	   ("H" hs-hide-all)
	   ("S" hs-show-all)
	   ("h" hs-hide-block)
	   ("s" hs-show-block)
	   ("l" hs-hide-level)
	   ("q" nil "quit" :exit t)))
	((highlight-indent-guides)
	 (setq highlight-indent-guides-responsive 'top)
	 (setq highlight-indent-guides-method 'fill))
	((hydra)
	 (defhydra hydra-hydra ()
	   "
_e_ english
_g_ magit
_h_ hideshow
_m_ markdown
_o_ org
_p_ paredit
"
	   ("e" hydra-english/body :exit t)
	   ("g" magit-status :exit t)
	   ("h" hydra-hideshow/body :exit t)
	   ("m" hydra-markdown-mode/body :exit t)
	   ("o" hydra-org/body :exit t)
	   ("p" hydra-paredit/body :exit t))
	 (do-for-hooks-in-list programming-modes-list
						   (lambda ()
							 (keys-extend-local-keymap '(hydra)))))
	((hydra define-word ispell)
	 (defhydra hydra-english (:foreign-keys run)
	   "
^Writing^
^^^^^^^---------
_d_ define-word
_w_ ispell-word
_b_ ispell-buffer
"
	   ("d" define-word-at-point)
	   ("w" ispell-word)
	   ("b" ispell-buffer)
	   ("q" nil "quit" :exit t)))
	((ispell)
	 (setq ispell-program-name "/usr/bin/aspell"))
	((keys)
	 (do-for-hooks-in-list programming-modes-list
						   (lambda ()
							 (keys-extend-local-keymap '(general)))))
	((lisp-mode)
	 (add-hook 'lisp-mode-hook
			   (lambda ()
				 (font-lock-mode 1)
				 (show-paren-mode 1))))
	((magit keys)
	 (add-hook 'magit-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(magit)))))
	((magit hydra)
	 (add-hook 'magit-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(hydra)))))
	((markdown-mode)
	 (add-hook 'markdown-mode-hook
			   (lambda ()
				 (setq indent-tabs-mode nil))))
	((markdown-mode hydra)
	 (defhydra hydra-markdown-mode (:foreign-keys run)
	   "
_B_ markdown-blockquote-region
"
	   ("B" markdown-blockquote-region)
	   ("q" nil "quit" :exit t)))
	((org)
	 (require 'esthlos-org))
	((org keys)
	 (do-for-hooks-in-list '(org-mode-hook
							 org-agenda-mode-hook)
						   (lambda ()
							 (keys-extend-local-keymap '(general org)))))
	((org hydra)
	 (defhydra hydra-org (:foreign-keys run)
	   "
_<_ org-promote-subtree
_>_ org-demote-subtree
_,_ org-set-priority
_A_ org-archive-subtree
_a_ org-agenda
_c_ org-capture
_C_ org-columns
_e_ org-set-effort
_j_ outline-next-visible-heading
_k_ outline-previous-visible-heading
_n_ org-narrow-to-subtree
_v_ org-paste-subtree
_s_ org-sort-entries
_x_ org-cut-subtree
_t_ org-todo
_h_ org-toggle-hold
_q_ widen
_q_ quit
"
	   ("<" org-promote-subtree)
	   (">" org-demote-subtree)
	   ("," org-set-priority)
	   ("a" org-agenda)
	   ("A" org-archive-subtree)
	   ("c" org-capture)
	   ("C" org-columns)
	   ("e" org-set-effort)
	   ("j" outline-next-visible-heading)
	   ("k" outline-previous-visible-heading)
	   ("n" org-narrow-to-subtree)
	   ("v" org-paste-subtree)
	   ("s" org-sort-entries)
	   ("x" org-cut-subtree)
	   ("t" org-todo)
	   ("h" org-toggle-hold)
	   ("w" widen)
	   ("q" nil "quit" :exit t)))
	((page-break-lines))
	((page-break-lines diminish)
	 (diminish page-break-lines-mode))
	((paredit)
	 (do-for-hooks-in-list lisp-mode-hooks
						   (lambda () (paredit-mode 1))))
	((paredit diminish)
	 (diminish 'paredit-mode))
	((paredit keys)
	 (add-hook 'paredit-mode-hook
			   (lambda () (keys-extend-local-keymap '(paredit)))))
	((paredit hydra)
	 (defhydra hydra-paredit (:foreign-keys run)
	   "
^Navigate^                       |  ^Mutate^                 | ^Interact^
^^^^^^-------------------------------|-------------------------|------------
		 C-M-u      C-M-o      |          C-M-i          |   << M-/ >>
			 \\     /           | C-,  <<    ^     >> C-.  |
	 C-M-j <----O----> C-m-l   |         ---O---         |   O       O
			 /     \\           | M-.  >>    V    << M-,  |
		  M-u       M-o        |          C-M-k          |   >> C-/ <<
"
	   ("C-M-i" paredit-raise-sexp)
	   ("C-M-j" paredit-backward)
	   ("C-M-k" paredit-wrap-sexp)
	   ("C-M-l" paredit-forward)
	   ("M-u" paredit-backward-down)
	   ("C-M-u" paredit-backward-up)
	   ("M-o" paredit-forward-down)
	   ("C-M-o" paredit-forward-up)
	   ("C-." paredit-forward-slurp-sexp)
	   ("C-," paredit-backward-slurp-sexp)
	   ("M-," paredit-forward-barf-sexp)
	   ("M-." paredit-backward-barf-sexp)
	   ("C-M-;" paredit-comment-dwim)
	   ("M-/" paredit-split-sexp)
	   ("C-/" paredit-splice-sexp)
	   ("q" nil "quit" :exit q)))
	((paren)
	 (setq show-paren-delay 0)
	 (setq show-paren-style 'expression)
	 (setq show-paren-when-point-in-periphery t)
	 (setq show-paren-when-point-inside-paren t))
	((puppet-mode))
	((rainbow-delimiters)
	 (do-for-hooks-in-list programming-modes-list
						   (lambda () (rainbow-delimiters-mode 1))))
	((sexpr)
	 (do-for-hooks-in-list lisp-mode-hooks
						   (lambda () (sexpr-mode 1))))
	((sexpr diminish)
	 (diminish 'sexpr-mode))
	((sexpr keys)
	 (add-hook 'sexpr-mode-hook
			   (lambda () (keys-extend-local-keymap '(sexpr)))))
	((sgml-mode)
	 (add-hook 'html-mode-hook
			   (lambda ()
				 (font-lock-mode 1)
				 (rainbow-delimiters-mode 1)
				 (electric-indent-local-mode -1))))
	((slime)
	 ;; note that this works instead of directly setting
	 ;; inferior-lisp-program
	 (setq slime-lisp-implementations
		   '((sbcl ("/usr/bin/sbcl"))
			 (ccl ("/usr/bin/ccl"))))
	 (mapcar (lambda (x) (add-to-list 'slime-contribs x))
			 '(slime-autodoc
			   slime-repl
			   slime-references
			   slime-highlight-edits
			   slime-scratch
			   slime-fancy-inspector))
	 (setq common-lisp-hyperspec-root "file:///home/ate/.clhs/")
	 (setq browse-url-firefox-program "firefox")
	 (defun override-slime-repl-bindings-with-paredit ()
	   (define-key slime-repl-mode-map
		 (read-kbd-macro paredit-backward-delete-key) nil))
	 (add-hook 'slime-repl-mode-hook
			   'override-slime-repl-bindings-with-paredit)
	 (require 'slime-autoloads)
	 (add-hook 'inferior-lisp-mode-hook
			   (lambda ()
				 (inferior-slime-mode t))))
	((slime keys)
	 (add-hook 'lisp-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(general slime))))
	 (add-hook 'slime-repl-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(slime-repl)))))
	((smart-tabs-mode)
	 (setq smart-tabs-mode t)
	 (smart-tabs-add-language-support tex plain-tex-mode-hook
	   ((c-indent-line . c-basic-offset)
		(c-indent-region . c-basic-offset)))
	 (smart-tabs-insinuate 'c 'python 'tex))
	((tabulated-list)
	 (add-hook 'tabulated-list-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(general)))))
	((term)
	 (global-set-key (kbd "C-c t") 'ansi-term)
	 (defun term-toggle-mode ()
	   (interactive)
	   (if (term-in-line-mode)
		   (term-char-mode)
		 (term-line-mode)))
	 (add-hook 'term-mode-hook
			   (lambda ()
				 (define-key term-mode-map
				   (kbd "C-c C-t")
				   'term-toggle-mode)
				 (define-key term-raw-map
				   (kbd "C-c C-t")
				   'term-toggle-mode)))
	 (add-hook 'term-mode-hook 'font-lock-mode)
	 (add-hook 'term-mode-hook
			   (lambda () (setq default-directory "~/")))
	 (defadvice ansi-term (after advise-ansi-term-coding-system)
	   (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
	 (ad-activate 'ansi-term))
	((tex-mode)
	 (setq tex-default-mode 'latex-mode)
	 (add-hook 'plain-tex-mode-hook
			   (lambda ()
				 (setq indent-tabs-mode t)
				 (setq tex-fontify-script nil)))
	 (add-hook 'latex-mode-hook
			   (lambda ()
				 (setq indent-tabs-mode t)
				 (setq tex-fontify-script nil))))
	((text-mode)
	 (add-hook 'text-mode-hook
			   (lambda ()
				 (setq fill-column 80)
				 (hl-line-mode 1)
				 (auto-fill-mode 1))))
	((tramp)
	 (defun find-file-root ()
	   (interactive)
	   (find-file
		(format "/%s::%s"
				(if (executable-find "sudo") "sudo" "su")
				(file-truename (read-file-name "su find file: ")))))
	 (add-hook 'term-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(file)))))
	((tramp gcloud))
	((tramp-term))
	((undo-tree)
	 (do-for-hooks-in-list programming-modes-list
						   (lambda () (undo-tree-mode 1))))
	((undo-tree keys)
	 (add-hook 'undo-tree-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(undo-tree-mode)))))
	((undo-tree diminish)
	 (diminish 'undo-tree-mode))
	((whitespace)
	 (setq whitespace-style
		   '(face trailing empty indentation
				  space-before-tab))
	 (setq whitespace-action
		   (quote
			(cleanup auto-cleanup report-on-bogus)))
	 (do-for-hooks-in-list programming-modes-list
						   (lambda () (whitespace-mode 1))))
	((whitespace diminish)
	 (diminish 'whitespace-mode))
	((with-editor))
	((with-editor keys)
	 (add-hook 'with-editor-mode-hook
			   (lambda ()
				 (keys-extend-local-keymap '(with-editor-mode)))))
	((yaml-mode))
	((yaml-mode highlight-indent-guides)
	 (add-hook 'yaml-mode-hook
			   (lambda ()
				 (highlight-indent-guides-mode 1)
				 (hl-line-mode -1))))))

(defun init|is-ok (x)
  (eql 'ok x))

(defun init|check-requirements (requirements)
  (let ((current-requirement nil))
	(condition-case nil
		(progn (dolist (requirement requirements)
				 (setq current-requirement requirement)
				 (require requirement))
			   'ok)
	  (error current-requirement))))

(defun init|eval-body (body-forms)
  (let ((current-form nil))
	(condition-case nil
		(progn (dolist (form body-forms)
				 (setq current-form form)
				 (eval form))
			   'ok)
	  (error current-form))))

(defun init|load-init-sexpr (s)
  (mapcar (lambda (p)
			(cons (car p)
				  (let ((requirement-report (init|check-requirements (car p))))
					(if (not (init|is-ok requirement-report))
						requirement-report
					  (init|eval-body (cdr p))))))
		  s))

(setq *init-report* (init|load-init-sexpr *init-sexpr*))

(setq *init-errors*
	  (seq-filter (lambda (i) (not (init|is-ok (cdr i))))
				  *init-report*))

(if (not (null *init-errors*))
	(progn
	  (switch-to-buffer "*init-errors*")
	  (princ ";; NOTE: Errors in the \"emacs\" category cause other errors further down!")
	  (princ (pp *init-errors*)
			 (get-buffer "*init-errors*"))
	  (beginning-of-buffer)
	  ;; (display-warning 'init
	  ;;                  "Error during init. Check *init-errors*."
	  ;;                  :warning)
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; useful utilities

(defun init|list-requirements ()
  "List all features required by the init sexpr."
  (sort (remove-duplicates (apply #'append
								  (mapcar #'car
										  *init-sexpr*)))
		(lambda (x y) (string< (symbol-name x)
							   (symbol-name y)))))

(defun init|needed-requirements ()
  "List all features which need to be installed."
  (seq-filter (lambda (x) (not (or (package-installed-p x)
								   (featurep x))))
			  (init|list-requirements)))

(defun init|install-all ()
  "Use the package manager to install all needed packages."
  (interactive)
  (let ((to-install (init|needed-requirements)))
	(if (null to-install)
		(message "Nothing to install!")
	  (progn
		(package-refresh-contents)
		(switch-to-buffer "*init-install*")
		(dolist (p to-install)
		  (princ (format "Installing %s ..." p)
				 (get-buffer "*init-install*"))
		  (condition-case nil
			  (package-install p)
			(princ (format "success.\n" p)
				   (get-buffer "*init-install*"))
			(error (princ (format "FAILURE.\n" p)
						  (get-buffer "*init-install*")))))))))
