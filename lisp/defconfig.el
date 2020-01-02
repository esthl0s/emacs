(provide 'defconfig)
(require 'seq)
(require 'cl)

(defvar defconfig|data
  `((:keymaps)
	(:configs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fundamental functions on configs

(defun defconfig|generate-namespace (features)
  (intern (apply #'concat
				 (mapcar #'symbol-name
						 features))))

(defun defconfig|lookup-config (features)
  (let* ((matches (seq-filter (lambda (c) (equal (cdr (assoc :features c))
											  features))
					   (cdr (assoc :configs defconfig|data)))))
	  (if matches (car matches) nil)))

(defun defconfig|run-code (c)
  (let ((code-value-list '()))
	(dolist (code (cdr (assoc :code c)))
	  (push (cons code (eval code))
			code-value-list))
	(setcdr (assoc :code-active c)
			(reverse code-value-list))))

(defun defconfig|read-config (feature-set args)
  ;; using the backtick macro here gave paradoxical issues
  ;; with defconfig effects "lingering" between evals.
  (list (cons :features feature-set)
		(cons :missing-features (defconfig|missing-features feature-set))
		(cons :enabled '())
		(cons :namespace (defconfig|generate-namespace feature-set))
		(cons :hooks (cdr (assoc :hooks args)))
		(cons :hooks-active '())
		(cons :keys (cdr (assoc :keys args)))
		(cons :keys-active '())
		(cons :code (assq-delete-all :keys
									 (assq-delete-all :hooks
													  args)))
		(cons :code-active '())))

;; (defun defconfig|read-config (feature-set args)
;;   `((:features . ,feature-set)
;;	(:missing-features . ())
;;	(:enabled . nil)
;;	(:namespace . ,(defconfig|generate-namespace feature-set))
;;	(:hooks . ,(cdr (assoc :hooks args)))
;;	(:hooks-active . ())
;;	(:keys . ,(cdr (assoc :keys args)))
;;	(:keys-active . ())
;;	(:code . ,(assq-delete-all :keys
;;							   (assq-delete-all :hooks
;;												args)))
;;	(:code-active . ())))

(defun defconfig|missing-features (feature-set)
  (seq-filter (lambda (f) (condition-case nil
							  (progn (require f) nil)
							(error t)))
			  feature-set))

(defun defconfig|add-config (c)
  (push (cons (cdr (assoc :namespace c)) c)
		(cdr (assoc :configs defconfig|data))))

(defun defconfig|remove-config (c)
  (setcdr (assoc :configs defconfig|data)
		  (assq-delete-all (cdr (assoc :namespace c))
						   (cdr (assoc :configs defconfig|data)))))

(defun defconfig|enable-config (c)
  (when (not (cdr (assoc :enabled c)))
	(defconfig|run-code c)
	(defconfig|enable-hooks c)
	(defconfig|enable-keys c)
	(setcdr (assoc :enabled c) t)))

(defun defconfig|disable-config (c)
  (when (cdr (assoc :enabled c))
	(defconfig|disable-hooks c)
	(defconfig|disable-keys c)
	(setcdr (assoc :enabled c) nil)))

(defmacro defconfig (feature-set &rest args)
  (let* ((new-config (defconfig|read-config feature-set args))
		 (old-config (defconfig|lookup-config feature-set)))
	(when old-config
	  (defconfig|disable-config old-config)
	  (defconfig|remove-config old-config))
	(defconfig|add-config new-config)
	(if (not (cdr (assoc :missing-features new-config)))
		(progn
		  (defconfig|enable-config new-config)
		  "enabled")
	  "added")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keys

(defun defconfig|generate-keymap (keymap-data)
  (let ((keymap (make-sparse-keymap)))
	(dotimes (i (/ (length keymap-data) 2))
	  (define-key
		keymap
		(kbd (nth (* 2 i) keymap-data))
		(nth (+ 1 (* 2 i)) keymap-data)))
	keymap))

(defun defconfig|generate-keymap-symbol (name)
  (intern (concat "defkeys-" (symbol-name name) "-map" )))

(defun defconfig|generate-keymap-map (name keymap-data)
  `((:symbol . ,(defconfig|generate-keymap-symbol name))
	(:keymap . ,(defconfig|generate-keymap keymap-data))))

(defun defconfig|update-keys-in-buffers (keymap-map)
  (let ((keymap-symbol (cdr (assoc :symbol keymap-map))))
	(dolist (b (buffer-list))
	  (let ((x (assoc keymap-symbol
					  (buffer-local-value 'minor-mode-overriding-map-alist
										  b))))
		(when x
		  (setcdr x (cdr (assoc :keymap keymap-map))))))))

(defun defconfig|enable-defkeys (name args)
  (let ((keymap-map (defconfig|generate-keymap-map name args)))
	(set (cdr (assoc :symbol keymap-map))
		 (cdr (assoc :keymap keymap-map)))
	(defconfig|update-keys-in-buffers keymap-map)
	(push (cons name keymap-map)
		  (cdr (assoc :keymaps defconfig|data)))))

(defun defconfig|disable-defkeys (name)
  (let ((keymap-map (cdr (assoc name (cdr (assoc :keymaps defconfig|data))))))
	(makunbound (cdr (assoc :symbol keymap-map)))
	(setcdr (assoc :keymaps defconfig|data)
			(assq-delete-all name
							 (cdr (assoc :keymaps defconfig|data))))))

(defmacro defkeys (name &rest args)
  (if (assoc name
			 (cdr (assoc :keymaps defconfig|data)))
	  (defconfig|disable-defkeys name))
  (defconfig|enable-defkeys name args)
  "ok")

(defun defconfig|lookup-keymap-map-by-name (keyset-name)
  (cdr (assoc keyset-name
			  (cdr (assoc :keymaps defconfig|data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hook maps
;;
;; these maps are of the form:
;; (<keyword> ((<hook-set-or-single> . <data>)
;;             (<hook-set-or-single> . <data>)
;;             ... )
;;
;; how <data> will be interpreted is out of scope of hook maps proper
;;
;; <hook-set-or-single> is a symbol and will be evaluated. Its value
;; should be either a list of symbols which evaluate to hooks, or a hook.

;; (defun defconfig|expand-hook-map (hook-map)
;;   "
;; Takes in the hooks from a configuration, and returns an alist of
;; hooks to functions.
;; "
;;   (let ((expanded-hooks '()))
;;	(dolist (h hook-map)
;;	  (let ((hook-value (eval (car h))))
;;		;; the only way to "type" hooks is by their name...yuck
;;		(if (and (listp hook-value)
;;				 (every #'symbolp hook-value)
;;				 (every (lambda (x) (string-match-p "hook$" (symbol-name x)))
;;						hook-value))
;;			(dolist (g hook-value)
;;			  (push `(,(cons :hook-symbol g)
;;					  ,(cons :hook-data (cdr h))) expanded-hooks))
;;		  (push `(,(cons :hook-symbol (car h))
;;				  ,(cons :hook-data (cdr h))) expanded-hooks))))
;;	(reverse expanded-hooks)))

(defun defconfig|expand-hook-map (hook-map)
  "
Takes in the hooks from a configuration, and returns an alist of
hooks to functions.
"
  (let ((expanded-hooks '()))
	(dotimes (i (/ (length hook-map) 2))
	  (let* ((hook-symbol (nth (* i 2)
							   hook-map))
			 (hook-value (cond
						  ((listp hook-symbol) hook-symbol)
						  ((symbolp hook-symbol)
						   (progn (condition-case nil
									  (symbol-value hook-symbol)
									(error (set hook-symbol nil)))
								  (eval hook-symbol)))))
			 (hook-data (nth (+ 1 (* i 2))
							 hook-map)))
		;; the only way to "type" hooks is by their name...yuck
		(if (and (listp hook-value)
				 (every #'symbolp hook-value)
				 (every (lambda (x) (string-match-p "hook$" (symbol-name x)))
						hook-value))
			(dolist (g hook-value)
			  (push `(,(cons :hook-symbol g)
					  ,(cons :hook-data hook-data))
					expanded-hooks))
		  (push `(,(cons :hook-symbol hook-symbol)
				  ,(cons :hook-data hook-data))
				expanded-hooks))))
	(reverse expanded-hooks)))

(defun defconfig|inject-hook-names (expanded-hook-map namespace key)
  (dotimes (i (length expanded-hook-map))
	(push (cons :hook-name (intern (concat (symbol-name namespace)
										   "-"
										   (substring (symbol-name key) 1)
										   "-"
										   (number-to-string i))))
		  (nth i expanded-hook-map)))
  expanded-hook-map)

(defun defconfig|generate-active-config (c key)
  (defconfig|inject-hook-names
	(defconfig|expand-hook-map (cdr (assoc key c)))
	(cdr (assoc :namespace c))
	key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hook functonality

(defun defconfig|enable-hooks (c)
  (let ((active-hook-config (defconfig|generate-active-config c :hooks)))
	(setf (alist-get :hooks-active c)
		  active-hook-config)
	(dolist (h active-hook-config)
	  (eval `(defun ,(cdr (assoc :hook-name h)) ()
			   ,(cdr (assoc :hook-data h))))
	  (add-hook (cdr (assoc :hook-symbol h))
				(cdr (assoc :hook-name h))))))

(defun defconfig|disable-hooks (c)
  (dolist (h (cdr (assoc :hooks-active c)))
	(remove-hook (cdr (assoc :hook-symbol h))
				 (cdr (assoc :hook-name h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key functionality

(defun defconfig|enable-keys (c)
  (let ((active-key-config (defconfig|generate-active-config c :keys)))
	(setf (alist-get :keys-active c)
		  active-key-config)
	(dolist (k active-key-config)
	  (eval `(defun ,(cdr (assoc :hook-name k)) ()
			   (dolist (s (quote ,(cdr (assoc :hook-data k))))
				 (let ((keymap-map (defconfig|lookup-keymap-map-by-name s)))
				   (push (cons (cdr (assoc :symbol keymap-map))
							   (cdr (assoc :keymap keymap-map)))
						 minor-mode-overriding-map-alist)))))
	  (add-hook (cdr (assoc :hook-symbol k))
				(cdr (assoc :hook-name k))))))

(defun defconfig|disable-keys (c)
  (dolist (h (cdr (assoc :keys-active c)))
	(remove-hook (cdr (assoc :hook-symbol h))
				 (cdr (assoc :hook-name h)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpful functions

(defun defconfig|pp (c)
  (let ((cc (defconfig|lookup-config c)))
	(when cc
	  (pp cc))))

(defun defconfig|explain-disabled-configs ()
  (mapcar (lambda (p)
			(print (format "Config %s is missing features %s"
						   (car p) (cdr p))))
		  (mapcar (lambda (c) (cons (cdr (assoc :features c))
									(cdr (assoc :missing-features c))))
				  (seq-filter (lambda (c) (not (cdr (assoc :enabled c))))
							  (cdr (assoc :configs defconfig|data))))))

(defun defconfig|list-needed-packages ()
  (remove-duplicates (apply (function append) (mapcar (lambda (c) (cdr (assoc :missing-features c)))
				  (seq-filter (lambda (c) (not (cdr (assoc :enabled c))))
							  (cdr (assoc :configs defconfig|data)))))))

;; (dolist (p (defconfig|list-needed-packages))
;;   (condition-case
;;	  nil
;;	  (package-install p)
;;	(error (print (format "Failed installing %s" p)))))

(defmacro defconfig|clear-data! ()
  `(progn (setcdr (assoc :configs defconfig|data) nil)
		  (setcdr (assoc :keymaps defconfig|data) nil)))
