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
  (defun flash-mode-line ()
	(invert-face 'mode-line)
	(run-with-timer 0.1 nil #'invert-face 'mode-line))
  (setq default-directory "~/"
		inhibit-splash-screen t
		initial-scratch-message ""
		indent-line-function 'insert-tab
		electric-indent-mode 1
		delete-selection-mode 1
		sentence-end-double-space nil
		tab-stop-list '(0 3)
		fill-column 80
		visible-bell nil
		ring-bell-function 'flash-mode-line
		backup-by-copying t
		delete-old-versions t
		kept-new-versions 6
		kept-old-versions 2
		version-control t
		read-quoted-char-radix 10
		mail-host-address "esthlos.com"
		browse-url-browser-function 'browse-url-firefox
		custom-theme-directory (expand-file-name "themes/"
												 user-emacs-directory)
		backup-directory-alist `(("." . ,(expand-file-name "recovery/backup/" user-emacs-directory)))
		auto-save-file-name-transforms `((".*"
										  ,(expand-file-name "recovery/autosave/" user-emacs-directory)
										  t))
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

(defconfig (cider hydra)
  (:keys cider-mode-hook (cider-hydra))
  (defhydra hydra-cider (:foreign-keys run)
	"
^Command^
^^^^^^^----------------------
_d_ cider-doc
_q_ quit
"
	("d" cider-doc)
	("q" nil "quit" :exit t)))

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
  (:keys markdown-mode-hook (prose))
  (:hooks markdown-mode-hook (progn (fci-mode)
									(setq indent-tabs-mode nil))))

(defconfig (markdown-mode hydra)
  (defhydra hydra-markdown-mode (:foreign-keys run)
	"
_B_ markdown-blockquote-region
"
	("B" markdown-blockquote-region)
	("q" nil "quit" :exit t)))

(defconfig (muti-term))

(defconfig (nix-mode)
  (:keys nix-mode-hook (general)))

(defconfig (org)
  (:keys (org-mode-hook org-agenda-mode-hook) (general org))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; custom commands

  (defun org-agenda-today ()
	(interactive)
	(org-agenda-list 1)
	(org-agenda-write "~/today.txt")
	(kill-buffer)
	(delete-window))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; general

  ;; the org directory
  ;;(add-to-list 'load-path "/usr/share/emacs/site-list/org-mode")
  (setq org-directory "~/org")

  ;; enable fontlocking in org mode
  (add-hook 'org-mode-hook
			(lambda ()
			  (font-lock-mode 1)))

  ;; list of modules to load
  (setq org-modules
		(quote
		 (org-habit
		  org-info
		  org-mhe)))

  ;; priorities
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?E)
  (setq org-default-priority ?C)

  ;; special beginning and end of line in headlines
  (setq org-special-ctrl-a/e t)

  ;; make the agenda split vertically
  (defadvice org-agenda (around split-vertically activate)
	(let ((split-width-threshold 80))
	  ad-do-it))

  ;; capture from anywhere
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; effort estimates
  (setq org-global-properties
		(quote (("Effort_ALL" .
				 "0 0:10 0:20 0:30 1:00 1:30 2:003:00 4:00 6:00 8:00 10:00 20:00"))))

  ;; enable indent mode
  (setq org-startup-indented t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; todo

  ;; set the keywords and their colors
  (setq org-todo-keywords
		'((sequence "|" "GOAL(g)")
		  (sequence "ACTION(n)" "INPROGRESS(i)"
					"|" "DONE(d!)" "DELEGATED(l@)")
		  (sequence "WAITING(w)" "|" "DONE(d!)" "DELEGATED(l@)")
		  (sequence "PROJECT(j)"
					"|" "DONE(d!)" "DELEGATED(l@)")
		  (sequence "EVENT(e)" "|" "DONE(d!)")
		  (sequence "|" "ABANDONED(a@)")))
  (setq org-todo-keyword-faces
		(quote (("GOAL" :foreground "black" :background "green")
				("PROJECT" :foreground "red")
				("ACTION" :foreground "cyan")
				("INPROGRESS" :foreground "magenta")
				("WAITING" :foreground "orange")
				("EVENT" :foreground "yellow")
				("DONE" :foreground "green")
				("DELEGATED" :foreground "green")
				("ABANDONED" :foreground "red"))))

  ;; enable logging
  (setq org-log-into-drawer t)

  ;; use a menu to select todo state
  (setq org-use-fast-todo-selection t)

  ;; the state to move to when an repeater is completed
  (setq org-todo-repeat-to-state nil)

  ;; log when a item is moved to a done state
  ;;(setq org-log-done t)

  ;; todo options
  (add-hook 'org-mode-hook
			(lambda ()
			  ;; enforce todo dependencies
			  (setq org-enforce-todo-dependencies t)
			  ;; track ordering as a property
			  (setq org-track-ordered-property-with-tag t)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; tags

  ;; Properties and columns
  (setq org-columns-default-format
		"%40ITEM %TODO %3PRIORITY %10TAGS %17Effort(Estimated Effort){:} %12CLOCKSUM")

  (setq org-tags-exclude-from-inheritance '("prioritized" "goal"))
  (setq org-use-property-inheritance '("PRIORITY"))

  (defun org-get-tags () (org-element-property :tags (org-element-at-point)))

  (defun org-toggle-tag (tag)
	(interactive)
	(let ((tags (org-get-tags)))
	  (if (member tag tags)
		  (setq tags (delete tag tags))
		(setq tags (cons tag tags)))
	  (org-set-tags-to tags)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; capture, refile, archive

  (setq org-default-notes-file (concat org-directory "/refile.org"))
  (setq org-capture-templates
		`(("o" "Note" entry (file )
		   "* Note: %?")
		  ("q" "Question" entry (file ,org-default-notes-file)
		   "* %? :refile:")
		  ("l" "Linked Items" entry (file ,org-default-notes-file)
		   "* TODO %?\n %a")
		  ("n" "Action" entry (file ,org-default-notes-file)
		   "* ACTION %?")
		  ("t" "Todo" entry (file ,org-default-notes-file)
		   "* TODO %?")
		  ("d" "Dream" entry (file+datetree ,(concat org-directory
													 "/dreams.org"))
		   "* %?\nEntered at %U\n %i")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  (setq org-completion-use-ido nil)
  ;; custom archiving command
  ;; set what headlines to be archived with custom command

  (defvar org-archive-headline-tags-to-archive "/DONE|ABANDONED"
	"Describes the kind of headlines to be archived. Should be of the
form \"TAGS/TODO\", where the standard syntax for tag and todo
searches applies.")
  ;; a function to archive everything in the current subtree

  (defun org-archive-completed-tasks ()
	"User-defined function to correctly archive everything
in the current buffer. Relies on `org-archive-headline-tags-to-archive'"
	(interactive)
	(org-map-entries (lambda ()
					   (org-archive-subtree)
					   (setq org-map-continue-from (outline-previous-heading)))
					 org-archive-headline-tags-to-archive
					 'file))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; agenda

  ;; set the correct agenda files
  (load-file "~/.emacs.d/config.local/org-files.el")

  ;;; look of the agenda
  ;; enable sticky agenda
  (setq org-agenda-sticky t)
  ;; show blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-start-on-weekday 0)
  (setq org-agenda-span 7)

  ;; set the format for the headline prefix
  (setq org-agenda-prefix-format
		(quote
		 ((agenda . " %i %-12:c%?-12t% s")
		  (timeline . "  % s")
		  (todo . " %i %-12:c")
		  (tags . " %i %-12c")
		  (search . " %i %-12:c"))))

  ;; set how many columns to let the tags go to
  (setq org-agenda-tags-column -78)

  ;; don't show tags in the headline copy
  (setq org-agenda-remove-tags t)

  ;;;;;;;;;;;;;;;;
  ;; building the agenda

  ;;; functions for building the agenda
  (defun outline-ascend-and-call (f)
	(save-excursion
	  (labels ((go-up-and-test (previous-point result)
							   (ignore-errors (outline-up-heading 1))
							   (cond
								(result result)
								((= (point) previous-point) nil)
								(t (go-up-and-test (point) (funcall f))))))
		(go-up-and-test (point) nil))))

  (defun outline-call-and-ascend (f)
	(save-excursion
	  (labels ((go-up-and-test (previous-point previous-result)
							   (cond
								(previous-result previous-result)
								((and (integer-or-marker-p previous-point)
									  (= (point) previous-point))
								 nil)
								(t (go-up-and-test (point)
												   (progn
													 (ignore-errors
													   (outline-up-heading 1))
													 (funcall f)))))))
		(go-up-and-test nil (funcall f)))))

  (defun outline-descend-and-call (f)
	"Depth-first search going down"
	(save-excursion
	  (let ((rtn nil))
		(org-narrow-to-subtree)
		(outline-next-heading)
		(while (not (or rtn (= (point) (point-max))))
		  (setq rtn (funcall f))
		  (outline-next-heading))
		(widen)
		rtn)))


  ;; functions for managing goals
  (defun org-link-goal ()
	"Link the headline at point to a goal."
	(interactive)
	(org-entry-put
	 (point)
	 "goal"
	 (message (format "[[%s]]"
					  (save-excursion
						(message "Place point on goal, then exit with C-M-c")
						(recursive-edit)
						(org-get-heading))))))

  (defun org-goto-goal ()
	"Jump to the goal of the current headline, or broadcast an error."
	(interactive)
	(let ((goal
		   (outline-call-and-ascend
			(lambda ()
			  (org-entry-get (point) "goal")))))
	  (if goal
		  (org-open-link-from-string goal))))


  ;; project itemization

  (defun org-headline-is-type (type)
	(equal type (org-entry-get (point) "TODO")))

  (defun org-headline-is-subproject ()
	(and (org-headline-is-type "PROJECT")
		 (outline-ascend-and-call
		  (lambda () (org-headline-is-type "PROJECT")))))

  (defun org-headline-doesnt-need-goal ()
	(if (not (or (org-headline-is-subproject)
				 (org-entry-get (point) "goal")))
		(point)))

  (defun org-headline-is-not-stuck-project ()
	(if (or (outline-descend-and-call
			 (lambda ()
			   (some #'org-headline-is-type
					 '("PROJECT" "ACTION" "INPROGRESS" "WAITING"))))
			(org-entry-blocked-p))
		(point)
	  nil))

  ;; want: list of things which are NOT blocked AND NOT scheduled
  ;; aka list of things which are NOT (blocked or scheduled)
  ;; so we need to skip things which are blocked or scheduled
  (defun org-headline-is-scheduled-or-blocked ()
	(if (or (org-entry-blocked-p)
			(org-entry-get (point) "SCHEDULED"))
		(point)
	  nil))

  (defun org-test ()
	(interactive)
	(message (if (org-headline-is-scheduled-or-blocked) "true" "false")))


  ;; functions to manage prioritization


  (setq org-agenda-custom-commands
		'(("z" "Present items"
		   ((agenda ""
					((org-agenda-overriding-header "Agenda")))
			(tags "+TODO=\"INPROGRESS\"-ice"
				  ((org-agenda-overriding-header "Actions in Progress")))
			(tags "+TODO=\"WAITING\"-ice"
				  ((org-agenda-overriding-header "Actions in Waiting")))
			(tags "+TODO=\"PROJECT\"-ice"
				  ((org-agenda-overriding-header "Stuck Projects")
				   (org-agenda-skip-function 'org-headline-is-not-stuck-project)))
			(tags "+TODO=\"ACTION\"-ice"
				  ((org-agenda-overriding-header "Unscheduled Actions")
				   (org-agenda-skip-function 'org-headline-is-scheduled-or-blocked)))))
		  ("x" "Present items"
		   ((agenda ""
					((org-agenda-overriding-header "In progress")))
			(tags "+TODO=\"INPROGRESS\""
				  ((org-agenda-overriding-header "In progress")))
			(tags "+TODO=\"WAITING\""
				  ((org-agenda-overriding-header "In Waiting")))
			(tags "refile"
				  ((org-agenda-overriding-header "To Refile")
				   (org-tags-match-list-sublevels nil)))
			(tags "+TODO=\"PROJECT\""
				  ((org-agenda-overriding-header "To Itemize")
				   (org-agenda-skip-function 'org-skip-if-actionable)
				   (org-agenda-sorting-strategy
					'(category-keep))))
			(tags "+TODO=\"PROJECT\""
				  ((org-agenda-overriding-header "To Itemize")
				   (org-agenda-skip-function 'org-skip-if-actionable)
				   (org-agenda-sorting-strategy
					'(category-keep)))))))))

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
								 indent-tabs-mode nil
								 electric-pair-mode 1)
		  inferior-python-mode-hook (setq electric-pair-mode 1))
  (:keys inferior-python-mode-hook (general python-shell))
  (setq python-shell-interpreter "/usr/bin/python3"))

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
  (:hooks slime-repl-mode-hook (slime|fix-repl-for-paredit)
		  inferior-lisp-mode-hook (inferior-slime-mode t))
  (:keys lisp-mode-hook (general slime)
		 slime-repl-mode-hook (slime-repl))
  ;; note that this works instead of directly setting
  ;; inferior-lisp-program
  (defun slime|fix-repl-for-paredit ()
	(define-key slime-repl-mode-map
	  (read-kbd-macro paredit-backward-delete-key) nil))
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

(defconfig (speedbar)
  (:keys speedbar-mode-hook (speedbar))
  (setq speedbar-use-images nil)
  (speedbar-add-supported-extension ".clj"))

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
  (:keys text-mode-hook (prose))
  (:hooks text-mode-hook (progn (setq fill-column 80)
								(hl-line-mode))))

(defconfig (tramp)
  (:keys term-mode-hook (file))
  (defun find-file-root ()
	(interactive)
	(find-file
	 (format "/%s::%s"
			 (if (executable-find "sudo") "sudo" "su")
			 (file-truename (read-file-name "su find file: "))))))

(defconfig (tramp gcloud))

(defconfig (treemacs)
  (:keys treemacs-mode-hook (general)))

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
