(defun all-required (requirements)
  (condition-case nil
      (progn (mapcar 'require
                     requirements)
             'ok)
    (error 'fail)))

(defun eval-body (body-forms)
  (condition-case nil
      (progn (mapcar 'eval body-forms)
             'ok ())
    (error 'body)))

(defun try-load-init-sexpr (s)
  (mapcar (lambda (p)
            (cons (car p)
                  (if (eql 'fail (all-required (car p)))
                      'requirements
                    (eval-body (cdr p)))))
          s))


(setq
 *init-report*
 (try-load-init-sexpr
  '(((emacs)
     ;; look and feel
     (prefer-coding-system 'us-ascii)
     (global-font-lock-mode 1)
     (setq initial-scratch-message "")
     (display-battery-mode 1)
     (set-scroll-bar-mode 'nil)
     (setq-default indent-tabs-mode nil)
     (setq indent-line-function 'insert-tab)
     (setq electric-indent-mode 1)
     (setq delete-selection-mode 1)
     (setq sentence-end-double-space nil)
     (setq tab-stop-list '(0 3))
     (setq fill-column 80)
     (setq-default tab-width 4)
     (setq backup-by-copying t)
     (setq read-quoted-char-radix 10)
     (global-set-key (kbd "C-c t") 'ansi-term)
     (defun term-toggle-mode ()
       (interactive)
       (if (term-in-line-mode)
           (term-char-mode)
         (term-line-mode)))
     (add-hook 'term-mode-hook
               (lambda ()
                 (define-key term-mode-map
                   (kbd "C-c C-a")
                   'term-toggle-mode)
                 (define-key term-raw-map
                   (kbd "C-c C-a")
                   'term-toggle-mode)))
     (global-set-key (kbd "<C-S-M-right>") 'shrink-window-horizontally)
     (global-set-key (kbd "<C-S-M-left>") 'enlarge-window-horizontally)
     (global-set-key (kbd "<C-S-M-down>") 'shrink-window)
     (global-set-key (kbd "<C-S-M-up>") 'enlarge-window)
     (global-set-key (kbd "C-'") 'comment-or-uncomment-region)
     (setq ispell-program-name "/usr/bin/aspell")
     (setq mail-host-address "esthlos.com")
     (setq browse-url-browser-function 'browse-url-firefox)
     (add-hook 'text-mode-hook
               (lambda ()
                 (setq fill-column 80)
                 (hl-line-mode 1)
                 (auto-fill-mode 1)))
     ;; load paths
     (add-to-list 'load-path "/home/ate/.emacs.d/lisp")
     (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
     (setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
     (setq default-directory "~/")
     ;; easy binding
     (defmacro bind-in-map (map bindings)
       `(dolist (binding-pair ,bindings)
          (define-key ,map (kbd (car binding-pair))
            (cadr binding-pair))))
     (defmacro bind-everywhere (bindings)
       `(dolist (binding-pair ,bindings)
          (global-set-key (kbd (car binding-pair))
                          (cadr binding-pair))))
     ;; easy add to hook set
     (defun do-for-hooks-in-list (hook-list function)
       (dolist (hook hook-list)
         (add-hook hook function)))
     (defvar lisp-mode-hooks '(common-lisp-mode-hook
                               clojure-mode-hook
                               cider-repl-mode-hook
                               emacs-lisp-mode-hook
                               lisp-mode-hook
                               scheme-mode-hook))
     (defvar programming-modes-list (append lisp-mode-hooks
                                            '(ada-mode
                                              html-mode-hook
                                              tex-mode-hook
                                              shell-script-mode-hook
                                              c-mode-hook
                                              python-mode-hook
                                              text-mode-hook
                                              js-mode-hook
                                              css-mode-hook))))
    ((package)
     (require 'package)
     (package-initialize)
     (add-to-list 'package-archives
                  '("melpa" . "http://melpa.milkbox.net/packages/")
                  t))
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
    ((cc-mode)
     (add-hook 'c-mode-hook
               (lambda ()
                 (setq indent-tabs-mode t))))
    ((cider-repl)
     (add-hook 'cider-repl-mode-hook
               (lambda ()
                 (keys-extend-local-keymap '(cider-repl)))))
    ((elisp-mode)
     (add-hook 'emacs-lisp-mode-hook
               (lambda ()
                 (font-lock-mode 1)
                 (setq indent-tabs-mode nil)
                 (show-paren-mode 1)
                 (setq indent-tabs-mode nil))))
    ((eldoc diminish)
     (diminish 'eldoc-mode))
    ((fill-column-indicator)
     (setq fci-rule-use-dashes t)
     (setq fci-rule-color "blue")
     (do-for-hooks-in-list programming-modes-list
                           (lambda () (fci-mode))))
    ((font-core)
     (load-file "~/.emacs.d/config.local/font.el")
     (add-to-list 'default-frame-alist
                  `(font . ,myfont))
     (set-default-font myfont))
    ((geiser paredit)
     (add-hook 'geiser-mode-hook #'paredit-mode))
    ((geiser undo-tree)
     (add-hook 'geiser-mode-hook #'undo-tree-mode))
    ((hexl)
     (add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode)))
    ((hideshow)
     (do-for-hooks-in-list programming-modes-list
                           (lambda () (hs-minor-mode 1)))
     (do-for-hooks-in-list lisp-mode-hooks
                           (lambda () (keys-extend-local-keymap '(hideshow-lisp)))))
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
    ((hydra)
     (defhydra hydra-hydra ()
       "
_h_ hideshow
_m_ markdown
_o_ org
_p_ paredit
"
       ("h" hydra-hideshow/body :exit t)
       ("m" hydra-markdown-mode/body :exit t)
       ("o" hydra-org/body :exit t)
       ("p" hydra-paredit/body :exit t))
     (do-for-hooks-in-list programming-modes-list
                           (lambda ()
                             (keys-extend-local-keymap '(hydra)))))
    ((keys)
     (do-for-hooks-in-list programming-modes-list
                           (lambda ()
                             (keys-extend-local-keymap '(general)))))
    ((lisp-mode)
     (add-hook 'lisp-mode-hook
               (lambda ()
                 (font-lock-mode 1)
                 (setq indent-tabs-mode nil)
                 (show-paren-mode 1)
                 (setq indent-tabs-mode nil))))
    ((markdown-mode))
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
_c_ org-capture
_C_ org-columns
_e_ org-set-effort
_j_ outline-next-visible-heading
_k_ outline-previous-visible-heading
_v_ org-paste-subtree
_s_ org-sort-entries
_x_ org-cut-subtree
_t_ org-todo
_h_ org-toggle-hold
_q_ quit
"
       ("<" org-promote-subtree)
       (">" org-demote-subtree)
       ("," org-set-priority)
       ("c" org-capture)
       ("C" org-columns)
       ("e" org-set-effort)
       ("j" outline-next-visible-heading)
       ("k" outline-previous-visible-heading)
       ("v" org-paste-subtree)
       ("s" org-sort-entries)
       ("x" org-cut-subtree)
       ("t" org-todo)
       ("h" org-toggle-hold)
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
             '(slime-repl
               slime-references
               slime-highlight-edits
               slime-scratch))
     (setq common-lisp-hyperspec-root "lisp.esthlos.com/clhs/")
     (setq browse-url-firefox-program "firefox-bin")
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
                 (keys-extend-local-keymap '(general slime)))))
    ((smart-tabs-mode)
     (setq smart-tabs-mode t)
     (smart-tabs-add-language-support tex plain-tex-mode-hook
       ((c-indent-line . c-basic-offset)
        (c-indent-region . c-basic-offset)))
     (smart-tabs-insinuate 'c 'python 'tex))
    ((term)
     (add-hook 'term-mode-hook 'font-lock-mode)
     (add-hook 'term-mode-hook
               (lambda () (setq default-directory "~/"))))
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
    ((tramp)
     (defvar find-file-root-prefix (if (featurep 'xemacs)
                                       "/[sudo/root@localhost]"
                                     "/sudo:root@localhost:"))
     (defvar find-file-root-history nil)
     (defvar find-file-root-hook nil)
     (defun find-file-root ()
       (interactive)
       (require 'tramp)
       (let* ( ;; use a separate history list for "root" files.
              (file-name-history find-f (ile-root-history))
              (name (or buffer-file-name default-directory))
              (tramp (and (tramp-tramp-file-p name)
                          (tramp-dissect-file-name name)))
              path dir file)
         ;; If called from a "root" file, we need to fix up the path.
         (when tramp
           (setq path (tramp-file-name-localname tramp)
                 dir (file-name-directory path)))
         (when (setq file (read-file-name "Find file (UID = 0): " dir path))
           (find-file (concat find-file-root-prefix file))
           ;; If this all succeeded save our new history list.
           (setq find-file-root-history file-name-history)
           ;; allow some user customization
           (run-hooks 'find-file-root-hook))))
     (global-set-key [(control x) (control r)] 'find-file-root))
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
           '(face tabs trailing newline empty
                  indentation space-after-tab space-before-tab))
     (setq whitespace-action
           (quote
            (cleanup auto-cleanup report-on-bogus)))
     (do-for-hooks-in-list programming-modes-list
                           (lambda () (whitespace-mode 1))))
    ((whitespace diminish)
     (diminish 'whitespace-mode))
    ((with-editor))
    ((x)
     (menu-bar-mode -1)
     (tool-bar-mode -1)
     (setq inhibit-splash-screen t)))))

(setq *init-errors*
      (append (seq-filter (lambda (i) (eq (cdr i) 'requirements))
                          *init-report*)
              (seq-filter (lambda (i) (eq (cdr i) 'body))
                          *init-report*)))

(if (not (null *init-errors*))
    (display-warning 'init
                     "Error during init. Check *init-errors*."
                     :warning))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (ate-light)))
 '(custom-safe-themes
   (quote
    ("53f65fd5042438d3f0d484dda7cae17e1105ec87de1ac52fee6941247f49098a" "34b9a79a465da3e91300b99ba3b2e533cbfc777ae39887be3c5e93469d183614" "655b7e2225ced08b3a31f086a5357eca10f2bedd40c481e532e570f4d3db62bb" "90e339f5fbc3f739c6bdb82f3553c0fe8377b5b44586414d6ec64951af1fd99b" "ae8da78bab18b45212184256d397ab713383374cc717b6f8229da235f9a55ce9" "2b16f250227e1bb0e98d14dd2327ca4fdbf3b54752b898033d4499f96121e4cb" "5f9bccf201fd171f630ba426d9870069534363bd92e327419bf269dd0345a0e6" "b25d2fffca5d821cff86895eb03d8e72af37419ec612474692db26aff34e2abf" "de8448a27a8f9cf378e31f3023d329cda6743ee2cb9c68a8db87f5d728c72ee1" "d2636cff590df97c4cb5bb6ad7f1f3f72ac841acedb6e5259a29573862637364" default)))
 '(package-selected-packages
   (quote
    (tramp-term markdown-mode hydra sr-speedbar cider ace-window geiser fill-column-indicator dictionary with-editor page-break-lines paredit rainbow-delimiters smart-tabs-mode undo-tree diminish slime alert sly helm-descbinds helm-describe-modes aggressive-indent ecb haskell-mode flycheck-pyflakes py-autopep8)))
 '(send-mail-function (quote sendmail-send-it))
 '(znc-servers
   (quote
    (("remote.esthlos.com" 6697 nil
      ((irc\.freenode\.org "esthlos" "Queen4nnesRevenge")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
