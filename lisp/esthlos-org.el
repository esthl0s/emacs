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
				  '(category-keep))))))))




(provide 'esthlos-org)
