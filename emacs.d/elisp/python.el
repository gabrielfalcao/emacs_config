;;;
;;; PYTHON CONFIGURATIONS    -*- Mode: Emacs-Lisp; -*-
;;;
;;; Guilherme Gondim (semente) <semente at taurinus.org>
;;; http://semente.taurinus.org
;;;

;; (setq python-python-command "python2.5")
(setq python-use-skeletons nil)

;; turn on ElDoc mode on Python mode
(add-hook 'python-mode-hook '(lambda () (eldoc-mode 1)) t)

(add-hook 'python-mode-hook '(lambda () (setq show-trailing-whitespace t)))

;; indent on newline
(add-hook 'python-mode-hook
	  '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; support for code check with flymake and pylint (errors only)
(when (load "flymake" t)
  (defun flymake-python-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "/usr/bin/pylint" (list "--errors-only"
				    "--output-format=parseable"
				    local-file))))

  (setq flymake-allowed-file-name-masks
	(cons '("\\.py$"
		flymake-python-init
		flymake-simple-cleanup
		flymake-get-real-file-name)
	      flymake-allowed-file-name-masks))

  (setq flymake-err-line-patterns
	(cons '("\\([a-z0-9A-Z_.-]+\\):\\([0-9]+\\): \\\[E\\(, [a-zA-Z_][a-z0-9A-Z_.]*\\)?\\\] \\(.*\\)"
		1 2 nil 4)
	      flymake-err-line-patterns)))

(add-hook 'python-mode-hook (lambda () (flymake-mode 1)))

 (add-hook 'python-mode-hook
 	  (lambda ()
 	    (define-key python-mode-map "\"" 'electric-pair)
 	    (define-key python-mode-map "\'" 'electric-pair)
 	    (define-key python-mode-map "(" 'electric-pair)
 	    (define-key python-mode-map "[" 'electric-pair)
 	    (define-key python-mode-map "{" 'electric-pair)))

 (defun electric-pair ()
   "Insert character pair without sournding spaces"
   (interactive)
   (let (parens-require-spaces)
     (insert-pair)))

;; Django Template mode
;; (load-library "django-html-mode")

;; ROPE - for refactoring and auto-completion
(pymacs-load "ropemacs" "rope-")
;; enabling autoimport
(setq ropemacs-enable-autoimport 't)
;; telling ropemacs which modules to cache
(setq ropemacs-autoimport-modules '("django"))

