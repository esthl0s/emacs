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
(setq org-directory "/data/primary/org")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; todo

;; set the keywords and their colors
(setq org-todo-keywords
      '((sequence "|" "RESPONSIBILITY(r)")
        (sequence "ACTION(n)" "INPROGRESS(i)"
                  "|" "DONE(d!)" "DELEGATED(l@)")
        (sequence "WAITING(w)" "|" "DONE(d!)" "DELEGATED(l@)")
        (sequence "PROJECT(j)"
                  "|" "DONE(d!)" "DELEGATED(l@)")
        (sequence "EVENT(e)" "|" "DONE(d!)")
        (sequence "|" "ABANDONED(a@)")))
(setq org-todo-keyword-faces
      (quote (("RESPONSIBILITY" :foreground "black" :background "yellow")
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


;;; functions for building the agenda

(defun outline-ascend-and-call (f)
  (save-excursion
    (labels ((go-up-and-test (previous-point)
                             (outline-up-heading 1)
                             (cond
                              ((= (point) previous-point)
                               nil)
                              ((funcall f) t)
                              (t (go-up-and-test (point))))))
      (go-up-and-test (point)))))

(defun outline-descend-and-call (f)
  "Depth-first search going down"
  (save-excursion
    (let ((rtn nil))
      (org-narrow-to-subtree)
      (outline-next-heading)
      (while (not (or rtn (= (point) (point-max))))
        (setq rtn (funcall f))
        i        (outline-next-heading))
      (widen)
      rtn)))

(defun org-headline-get-property (p)
  (cdr (assoc p (org-entry-properties (point)))))

(defun org-headline-is-project ()
  (equal "PROJECT" (org-headline-get-property "TODO")))

(defun org-headline-has-responsibility ()
  (if (equal nil (org-headline-get-property "responsibility"))
      nil
    (point)))

(defun org-headline-has-goal ()
  (if (equal nil (org-headline-get-property "goal"))
      nil
    (point)))

(defun org-headline-is-action ()
  (let ((todo (org-headline-get-property "TODO")))
    (or (equal todo "ACTION")
        (equal todo "WAITING")
        (equal todo "INPROGRESS"))))

(defun org-headline-is-holding ()
  (member "hold" (org-get-tags)))

(defun org-unactionable-project ()
  (and (org-headline-is-project)
       (not (org-headline-is-holding))
       (not (outline-descend-and-call
             (lambda ()
               (or (org-unactionable-project)
                   (org-headline-is-action)))))))

(defun org-skip-if-actionable ()
  (if (org-unactionable-project) nil (point)))

(defun org-todo-is-not-todo-action-waiting-inprogress ()
  (if (member (org-headline-get-property "TODO")
              (list "TODO" "ACTION" "WAITING" "INPROGRESS"))
      nil
    (point)))

(defun org-todo-is-not-action-inprogress ()
  (if (member (org-headline-get-property "TODO")
              (list "ACTION" "INPROGRESS"))
      nil
    (point)))

(setq org-agenda-custom-commands
      '(("z" "Present items"
         ((agenda)
          (tags "+TODO=\"INPROGRESS\""
                ((org-agenda-overriding-header "Items in progress")))
          (tags "+TODO=\"WAITING\""
                ((org-agenda-overriding-header "Items in Waiting")))
          (tags "refile"
                ((org-agenda-overriding-header "Items to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags "+TODO=\"TODO\"-hold"
                ((org-agenda-overriding-header "Stuck Projects")
                 (org-agenda-skip-function 'org-skip-if-actionable)
                 (org-agenda-sorting-strategy
                  '(category-keep))))
          (tags "-repeater-hold-prioritized"
                ((org-agenda-overriding-header "Unprioritized, Unblocked")
                 (org-agenda-skip-function 'org-todo-is-not-todo-action-waiting-inprogress)
                 (org-agenda-sorting-strategy
                  '(category-keep))))
          (tags "-repeater-BLOCKED=\"t\"+SCHEDULED=\"\"-hold+prioritized"
                ((org-agenda-overriding-header "Prioritized, Unscheduled, Unblocked")
                 (org-agenda-skip-function 'org-todo-is-not-action-inprogress)))
          (tags "-repeater+TODO=\"ACTION\"-BLOCKED=\"t\"-SCHEDULED=\"\"-hold+prioritized"
                ((org-agenda-overriding-header "Prioritized, Scheduled, Unblocked")))))
        ("J" "troublesome projects"
         ((tags "+TODO=\"PROJECT\"-hold"
                ((org-agenda-overriding-header "Projects without goals")
                 (org-agenda-skip-function 'org-headline-has-goal)))
          (tags "+TODO=\"PROJECT\"-hold"
                ((org-agenda-overriding-header "Projects without responsibilities")
                 (org-agenda-skip-function 'org-headline-has-responsibility)))))
        ("S" "stasis"
         ((tags "+hold"
                ((org-agenda-overriding-header "Items on hold")))
          (tags "+BLOCKED=\"t\""
                ((org-agenda-overriding-header "Blocked")
                 (org-agenda-skip-function 'org-todo-is-not-todo-action-waiting-inprogress)))
          (tags "STYLE=\"habit\""
                ((org-agenda-overriding-header "Habitual items")))))
        ("d" "Main list of things to do"
         ((todo "INPROGRESS")
          (tags "refile")
          (tags "+TODO=\"ACTION\"-BLOCKED=\"t\"-hold-repeater")))
        ("h" "List of items on hold"
         ((tags "+hold")))
        ("b" "List of habits"
         ((tags "STYLE=\"habit\"")))))


;; Custom definition of stuck projects


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extras

(defun org-toggle-hold ()
  (interactive)
  (org-toggle-tag "hold"))

(defun org-set-priority ()
  (interactive)
  (org-toggle-tag "prioritized")
  (org-priority))

(provide 'esthlos-org)
