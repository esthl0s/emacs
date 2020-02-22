(defconfig (emacs)
  (:keys general-modes-hooks (general))
  (add-to-list 'load-path
			   (expand-file-name "lisp/"
								 user-emacs-directory))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (display-battery-mode -1)
  (global-font-lock-mode 1)
  (set-scroll-bar-mode 'nil)
  (prefer-coding-system 'us-ascii)
  (defvar lisp-mode-hooks)
  (defvar programming-modes-hooks)
  (defvar general-modes-hooks)
  (setq default-directory "~/"
		inhibit-splash-screen t
		initial-scratch-message ""
		indent-line-function 'insert-tab
		electric-indent-mode 1
		delete-selection-mode 1
		sentence-end-double-space nil
		tab-stop-list '(0 3)
		fill-column 80
		backup-by-copying t
		read-quoted-char-radix 10
		mail-host-address "esthlos.com"
		browse-url-browser-function 'browse-url-firefox
		custom-theme-directory (expand-file-name "themes/"
												 user-emacs-directory)
		backup-directory-alist `(("." . ,(expand-file-name "saves"
														   user-emacs-directory)))
		lisp-mode-hooks '(common-lisp-mode-hook
						  clojure-mode-hook
						  cider-repl-mode-hook
						  emacs-lisp-mode-hook
						  lisp-mode-hook
						  scheme-mode-hook
						  slime-repl-mode-hook)
		programming-modes-hooks (append lisp-mode-hooks
										'(prog-mode-hook
										  ada-mode-hook
										  html-mode-hook
										  java-mode-hook
										  tex-mode-hook
										  shell-script-mode-hook
										  c-mode-hook
										  python-mode-hook
										  puppet-mode-hook
										  js-mode-hook
										  css-mode-hook
										  conf-mode-hook))
		general-modes-hooks (append programming-modes-hooks
									'(text-mode-hook
									  Info-mode-hook
									  help-mode-hook
									  apropos-mode-hook
									  Man-mode-hook
									  diff-mode-hook)))
  (setq-default indent-tabs-mode t
				tab-width 4)
  (global-set-key (kbd "<C-S-M-right>") 'shrink-window-horizontally)
  (global-set-key (kbd "<C-S-M-left>") 'enlarge-window-horizontally)
  (global-set-key (kbd "<C-S-M-down>") 'shrink-window)
  (global-set-key (kbd "<C-S-M-up>") 'enlarge-window)
  (global-set-key (kbd "C-'") 'comment-or-uncomment-region)
  ;; different operating systems
  (cond
   ((string-equal system-type "darwin")
	(setq mac-command-modifier 'meta
		  browse-url-browser-function 'browse-url-chrome))))

(defconfig (package)
  (package-initialize)
  (add-to-list 'package-archives
			   (cons "melpa" "http://melpa.milkbox.net/packages/")
			   t)
  ;; (add-to-list 'package-archives
  ;;				  (cons "melpa" "http://melpa.org/packages/")
  ;;				  t)
  )

(defconfig (minibuffer)
  (:keys minibuffer-inactive-mode-hook (general)
		 minibuffer-setup-hook (general)))

(defconfig (minibuffer counsel)
  (:keys minibuffer-inactive-mode-hook (counsel)
		 minibuffer-setup-hook (counsel)))

(defconfig (ace-jump-mode)
  (:keys general-modes-hooks (ace-jump)))

(defconfig (ace-window)
  (:keys general-modes-hooks (ace-window)))

(defconfig (ada-mode)
  (mapcar (lambda (x) (add-to-list 'auto-mode-alist (cons x 'ada-mode)))
		  (list "\\.gpr\\'" "\\.ads\\'" "\\.adb\\'")))

(defconfig (aggressive-indent)
  (:hooks lisp-mode-hooks (aggressive-indent-mode)))

(defconfig (aggressive-indent diminish)
  (diminish 'aggressive-indent-mode))

(defconfig (ascii))

(defconfig (autorevert diminish)
  (diminish 'auto-revert-mode))

(defconfig (bazel-mode))

(defconfig (calendar)
  (setq calendar-latitude 41.2)
  (setq calendar-longitude -73.7)
  (setq calendar-location-name "Armonk, NY"))

(defconfig (calfw))

(defconfig (calfw-org))

(defconfig (cc-mode)
  (:hooks (c-mode-hook java-mode-hook) (progn (setq indent-tabs-mode t)
											  (aggressive-indent-mode 1))))

(defconfig (cider)
  (:keys cider-mode-hook (cider))
  ;; to remove errors, which fuck with aggressive indent
  (defun cider-repl-emit-stderr (buffer string))
  ;; to remove the terrible huge map in overtone
  (setq cider-print-fn nil))

(defconfig (cider-repl)
  (:keys cider-repl-mode-hook (cider-repl))
  (setq cider-repl-display-help-banner nil))

(defconfig (company)
  (:hooks programming-modes-hooks (company-mode))
  (:hooks company-mode-hook (setq company-idle-delay 0)))

(defconfig (company diminish)
  (diminish 'company-mode))

(defconfig (counsel ivy)
  (:keys general-modes-hooks (counsel))
  (counsel-mode))

(defconfig (counsel ivy diminish)
  (diminish 'counsel-mode))

(defconfig (csv-mode)
  (:keys csv-mode-hook (general)))

(defconfig (custom)
  (setq custom-theme-directory
		(expand-file-name "themes/"
						  user-emacs-directory))
  (setq custom-file
		(expand-file-name "custom.el"
						  user-emacs-directory))
  (load custom-file))

(defconfig (define-word))

(defconfig (defconfig hydra)
  (defhydra hydra-defconfig (:foreign-keys run)
	"
^Command^
^^^^^^^----------------------
_d_ describe-config
_E_ explain-disabled-configs
_q_ quit
"
	("d" defconfig|describe-config)
	("E" defconfig|explain-disabled-configs)
	("q" nil "quit" :exit t)))

(defconfig (dired)
  (:keys dired-mode-hook (dired))
  (:hooks dired-mode-hook (dired-hide-details-mode 1))
  ;;(setq dired-listing-switches "--group-directories-first")
  )

(defconfig (dired hydra)
  (:keys dired-mode-hook (hydra)))

(defconfig (dired dired-subtree)
  (:keys dired-mode-hook (dired-subtree)))

(defconfig (dockerfile-mode)
  (:keys dockerfile-mode-hook (general)))

(defconfig (ediff)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(defconfig (eldoc diminish)
  (diminish 'eldoc-mode))

(defconfig (elpy)
  (:hooks elpy-mode-hook (progn (setq tab-width 4)
								(setq indent-tabs-mode nil)))
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'current))

(defconfig (elpy smart-tabs-mode)
  (:hooks elpy-mode-hook (setq smart-tabs-mode nil)))

(defconfig (elpy flycheck)
  (:hooks elpy-mode-hook (flycheck-mode))
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(defconfig (eshell)
  (:hooks eshell-mode-hook (set-buffer-process-coding-system 'utf-8-unix
															 'utf-8-unix))
  (:keys eshell-mode-hook (ace-jump ace-window general eshell hydra swiper))
  (defun eshell-new ()
	(interactive)
	(eshell 'N))
  (defun eshell|with-face (str &rest face-plist)
	(propertize str 'face face-plist))
  (defun ate-eshell-prompt ()
	(concat
	 (eshell|with-face (eshell/pwd) :foreground "green")
	 " "
	 (eshell|with-face "λ" :foreground "magenta")
	 " "))
  (setq eshell-prompt-regexp "^[^#$λ
]* [#$λ] ")
  (setq eshell-prompt-function 'ate-eshell-prompt)
  ;; (defun ate-eshell-prompt-long ()
  ;;	(let ((header-bg "grey23"))
  ;;	  (concat
  ;;	   (eshell|with-face (concat (eshell/pwd) " ")
  ;;						 :background header-bg)
  ;;	   (eshell|with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time))
  ;;						 :background header-bg :foreground "#888")
  ;;	   (eshell|with-face "\n" :background header-bg)
  ;;	   (eshell|with-face (eshell/whoami) :foreground "green")
  ;;	   "@"
  ;;	   (eshell|with-face (eshell/) "localhost" :foreground "purple")
  ;;	   (if (= (user-uid) 0)
  ;;		   (eshell|with-face " #" :foreground "red")
  ;;		 (eshell|with-face " λ" :foreground "green"))
  ;;	   " ")))
  )

(defconfig (exec-path-from-shell)
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

(defconfig (fill-column-indicator)
  (:hooks programming-modes-hooks (fci-mode))
  (setq fci-rule-use-dashes t)
  (setq fci-rule-color "blue"))

;; TODO rework this
(defconfig (font-core)
  (let ((font-file-path (expand-file-name "config.local/font.el"
										  user-emacs-directory)))
	(if (file-exists-p font-file-path)
		(load-file font-file-path)
	  (setq myfont "Source Code Pro")))
  (add-to-list 'default-frame-alist
			   `(font . ,myfont))
  (set-default-font myfont)
  (load-theme 'ate t))

(defconfig (geiser))

(defconfig (geiser paredit)
  (:hooks geiser-mode-hook (paredit-mode)))

(defconfig (geiser undo-tree)
  (:hooks geiser-mode-hook undo-tree-mode))

(defconfig (hexl)
  (add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode)))

(defconfig (hideshow)
  (:hooks programming-modes-hooks (hs-minor-mode))
  ;; disabling for now
  ;; (do-for-hooks-in-list lisp-mode-hooks
  ;;						   (lambda () (keys-extend-local-keymap '(hideshow-lisp))))
  )

(defconfig (hideshow diminish)
  (diminish 'hs-minor-mode))

(defconfig (hideshow hydra)
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

(defconfig (highlight-indent-guides)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-method 'fill))

(defconfig (hydra)
  (:keys general-modes-hooks (hydra))
  (defhydra hydra-hydra ()
	"
_d_ defconfig
_e_ english
_g_ magit
_h_ hideshow
_m_ markdown
_o_ org
_p_ paredit
"
	("d" hydra-defconfig/body :exit t)
	("e" hydra-english/body :exit t)
	("g" magit-status :exit t)
	("h" hydra-hideshow/body :exit t)
	("m" hydra-markdown-mode/body :exit t)
	("o" hydra-org/body :exit t)
	("p" hydra-paredit/body :exit t)))

(defconfig (hydra define-word ispell)
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

(defconfig (ispell)
  (setq ispell-program-name
		(cond ((file-exists-p "/usr/bin/aspell")
			   "/usr/bin/aspell")
			  ((file-exists-p "/usr/local/bin/aspell")
			   "/usr/local/bin/aspell"))))

(defconfig (ivy)
  (:keys ivy-mode-hook (general))
  (ivy-mode))

(defconfig (ivy diminish)
  (diminish 'ivy-mode))

(defconfig (lisp-mode)
  (:hooks lisp-mode-hook (progn (font-lock-mode)
								(show-paren-mode))))

(defconfig (magit)
  (:keys magit-mode-hook (general)))

(defconfig (magit hydra)
  (:keys magit-mode-hook (hydra)))

(defconfig (markdown-mode)
  (:hooks markdown-mode-hook (setq indent-tabs-mode nil)))

(defconfig (markdown-mode hydra)
  (defhydra hydra-markdown-mode (:foreign-keys run)
	"
_B_ markdown-blockquote-region
"
	("B" markdown-blockquote-region)
	("q" nil "quit" :exit t)))

(defconfig (muti-term))

(defconfig (org)
  (require 'esthlos-org))

(defconfig (org keys)
  (:keys (org-mode-hook org-agenda-mode-hook) (general org)))

(defconfig (org hydra)
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
_o_ org-toggle-ordered-property
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
	("o" org-toggle-ordered-property)
	("v" org-paste-subtree)
	("s" org-sort-entries)
	("x" org-cut-subtree)
	("t" org-todo)
	("h" org-toggle-hold)
	("w" widen)
	("q" nil "quit" :exit t)))

(defconfig (page-break-lines))

(defconfig (page-break-lines diminish)
  (diminish page-break-lines-mode))

(defconfig (paredit)
  (:keys lisp-mode-hooks (paredit))
  (defun paredit-forward-up-all ()
	(interactive)
	(paredit-forward-up 64))
  (defun paredit-backward-up-all ()
	(interactive)
	(paredit-backward-up 64)))

(defconfig (paredit diminish)
  (diminish 'paredit-mode))

(defconfig (paredit hydra)
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

(defconfig (paren)
  (:hooks lisp-mode-hooks (do (font-lock-mode)
							  (show-paren-mode)))
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren t))

(defconfig (proof)
  (:keys coq-mode-hook (general coq))
  (setq coq-mode-hook nil)
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
				 (insert "| "))))))

(defconfig (puppet-mode))

(defconfig (python)
  (:hooks python-mode-hook (setq tab-width 4
								 python-indent 4
								 python-indent-offset 4
								 indent-tabs-mode nilelectric-pair-mode 1)
		  inferior-python-mode-hook (setq electric-pair-mode 1))
  (:keys inferior-python-mode-hook (general python-shell))
  (setq python-shell-interpreter "/usr/local/bin/python3"))

(defconfig (python rainbow-delimiters)
  (:hooks inferior-python-mode-hook (rainbow-delimiters-mode)))

(defconfig (rainbow-delimiters)
  (:hooks programming-modes-hooks (rainbow-delimiters-mode)))

(defconfig (sexpr)
  (:hooks lisp-mode-hooks (sexpr-mode))
  (:keys lisp-mode-hooks (sexpr)))

(defconfig (sexpr diminish)
  (diminish 'sexpr-mode))

(defconfig (sgml-mode)
  (:hooks html-mode-hook (progn (font-lock-mode)
								(rainbow-delimiters-mode)
								(electric-indent-local-mode -1))))

(defconfig (slime)
  ;; note that this works instead of directly setting
  ;; inferior-lisp-program
  (:hooks slime-repl-mode-hook (define-key slime-repl-mode-map
								 (read-kbd-macro paredit-backward-delete-key) nil)
		  inferior-lisp-mode-hook (inferior-slime-mode t))
  (:keys lisp-mode-hook (general slime)
		 slime-repl-mode-hook (slime-repl))
  (setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))
									 (ccl ("/usr/bin/ccl")))
		common-lisp-hyperspec-root "file:///home/ate/.clhs/")
  (mapcar (lambda (x) (add-to-list 'slime-contribs x))
		  '(slime-autodoc
			slime-repl
			slime-references
			slime-highlight-edits
			slime-scratch
			slime-fancy-inspector))
  (require 'slime-autoloads))

(defconfig (smart-tabs-mode)
  (setq smart-tabs-mode t)
  (smart-tabs-add-language-support tex plain-tex-mode-hook
	((c-indent-line . c-basic-offset)
	 (c-indent-region . c-basic-offset)))
  (smart-tabs-insinuate 'c 'python 'tex))

;; TODO fix
(defconfig (swiper ivy)
  (global-set-key (kbd "C-s") 'swiper))

(defconfig (tabulated-list)
  (:keys tabulated-list-mode-hook (general)))

(defconfig (term)
  (:keys term-mode-hook (progn (font-lock-mode)
							   (setq default-directory "~/")))
  ;; TODO fix
  (global-set-key (kbd "C-c t") 'ansi-term)
  (defun term-toggle-mode ()
	(interactive)
	(if (term-in-line-mode)
		(term-char-mode)
	  (term-line-mode)))
  (define-key term-mode-map (kbd "C-c C-t") 'term-toggle-mode)
  (define-key term-raw-map (kbd "C-c C-t") 'term-toggle-mode)
  (defadvice ansi-term (after advise-ansi-term-coding-system)
	(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (ad-activate 'ansi-term))

(defconfig (tex-mode)
  (:hooks (plain-tex-mode-hook latex-mode-hook)
		  (do (setq indent-tabs-mode t
					tex-fontify-script nil)))
  (setq tex-default-mode 'latex-mode))

(defconfig (text-mode)
  (:hooks text-mode-hook (progn (setq fill-column 80)
								(hl-line-mode)
								(auto-fill-mode))))

(defconfig (tramp)
  (:keys term-mode-hook (file))
  (defun find-file-root ()
	(interactive)
	(find-file
	 (format "/%s::%s"
			 (if (executable-find "sudo") "sudo" "su")
			 (file-truename (read-file-name "su find file: "))))))

(defconfig (tramp gcloud))

(defconfig (undo-tree)
  (:hooks general-modes-hooks (undo-tree-mode 1))
  (:keys undo-tree-mode-hook (undo-tree-mode)))

(defconfig (undo-tree diminish)
  (diminish 'undo-tree-mode))

(defconfig (visual-regexp)
  (global-set-key (kbd "C-M-s") (function vr/query-replace)))

(defconfig (whitespace)
  (:hooks programming-modes-hooks (whitespace-mode))
  (setq whitespace-style '(face trailing empty indentation
								space-before-tab)
		whitespace-action '(cleanup auto-cleanup report-on-bogus)))

(defconfig (whitespace diminish)
  (diminish 'whitespace-mode))

(defconfig (with-editor)
  (:keys with-editor-mode-hook (with-editor-mode)))

(defconfig (yaml-mode))

(defconfig (yaml-mode highlight-indent-guides)
  (:hooks yaml-mode-hook (progn (highlight-indent-guides-mode)
								(hl-line-mode -1))))