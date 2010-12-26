;; Overwrite flymake-display-warning so that no annoying dialog box is
;; used.

;; This version uses lwarn instead of message-box in the original version.
;; lwarn will open another window, and display the warning in there.
;; where to get the latest emacs snapshot gtk
;; deb http://emacs.orebokech.com sid main
;; deb-src http://emacs.orebokech.com sid main

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-epiphany))
 '(column-number-mode t)
 '(face-font-family-alternatives (quote (("Monaco-12") ("helv" "helvetica" "arial" "fixed"))))
 '(inhibit-startup-echo-area-message "gabriel")
 '(initial-buffer-choice t)
 '(initial-scratch-message "")
 '(menu-bar-mode nil)
 '(py-beep-if-tab-change nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(setq visible-bell t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tooltip-mode nil)
 '(transient-mark-mode t))
;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;;'(default ((t (:stipple nil :background "black" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :family "misc-fixed")))))

(setq-default c-basic-offset 4)
(setq-default html-basic-offset 4)
(setq tab-width 4) ; or any other preferred value

(setq python-python-command "ipython")
;; Expanding the load-path
(setq load-path (cons "~/.emacs.d/elisp/" load-path))
(setq default-directory "~/")
;(set-default-font "Monospace-9")
;(set-fontset-font (frame-parameter nil 'font)
;                    'han '("cwTeXHeiBold" . "unicode-bmp"))
(setenv "PATH" (concat "/usr/local/bin:/usr/local/git/bin:" (concat (getenv "HOME") "/usr/bin:") (getenv "PATH")))

(set-default-font "Monaco-12")
; Python mode
(load-file "~/.emacs.d/elisp/python-mode.el")

(setenv "PYMACS_PYTHON" "python2.5")
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;; Auto completion inline
(load-file "~/.emacs.d/elisp/auto-complete.el") ;;loading
(require 'auto-complete);; preparing
(global-auto-complete-mode t) ;;enabling
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-dwim t)

(load-file "~/.emacs.d/elisp/two-mode-mode.el")
(load-file "~/.emacs.d/elisp/bar-cursor.el")
(load-file "~/.emacs.d/elisp/maxframe.el")
;; making new emacs windows have a pretty font
;;(add-to-list 'default-frame-alist '(font . "Bitstream Vera SansMono-8"))

;; color theme
(require 'color-theme)


(load "~/.emacs.d/elisp/color-theme-twilight.el")
(load "~/.emacs.d/elisp/color-theme-tango.el")
(load "~/.emacs.d/elisp/color-theme-blackboard.el")
(load "~/.emacs.d/elisp/color-theme-zenburn.el")
(load "~/.emacs.d/elisp/color-theme-sunburst.el")
(load "~/.emacs.d/elisp/color-theme-arjen.el")
(load "~/.emacs.d/elisp/inspiration630889.el")
(load "~/.emacs.d/elisp/inspiration715644.el")
(load "~/.emacs.d/elisp/inspiration733956.el")
(load "~/.emacs.d/elisp/inspiration976777.el")

(setq color-theme-is-global t)

(inspiration-733956)
;;(color-theme-tangotango)
;;(color-theme-blippblopp) ;; best light theme ! (default)
;;(color-theme-ld-dark) ;; pretty cool dark theme
;;(color-theme-hober) ;; good dark theme
;;(color-theme-andreas) ;; almost good light theme
(setq color-theme-is-global nil)
(setq-default indent-tabs-mode nil)

;; cucumber.el -- Emacs mode for editing plain text user stories
;; \C-c ,v - Verify all scenarios in the current buffer file.
;; \C-c ,s - Verify the scenario under the point in the current buffer.
;; \C-c ,f - Verify all features in project. (Available in feature and ruby files)
;; \C-c ,r - Repeat the last verification process.

;; textmate-like snippets
;; http://code.google.com/p/yasnippet/
(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/elisp/snippets")

;; Exploring more goodies =)
;;(tool-bar-mode)
(bar-cursor-mode)

;;(global-linum-mode)
;;(setq standard-indent 2)
;; ========== Place Backup Files in Specific Directory ==========
;;
;; Enable backup files.
;;(setq make-backup-files t)
;;
;; Enable versioning with default values (keep five last versions, I think!)
;;(setq version-control t)
;;
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; ===== Function to delete a line =====

;; First define a variable which will store the previous column position
(defvar previous-column nil "Save the column position")

;; Define the nuke-line function. The line is killed, then the newline
;; character is deleted. The column which the cursor was positioned at is then
;; restored. Because the kill-line function is used, the contents deleted can
;; be later restored by usibackward-delete-char-untabifyng the yank commands.
(defun nuke-line()
  "Kill an entire line, including the trailing newline character"
  (interactive)

  ;; Store the current column position, so it can later be restored for a more
  ;; natural feel to the deletion
  (setq previous-column (current-column))

  ;; Now move to the end of the current line
  (end-of-line)

  ;; Test the length of the line. If it is 0, there is no need for a
  ;; kill-line. All that happens in this case is that the new-line character
  ;; is deleted.
  (if (= (current-column) 0)
    (delete-char 1)

    ;; This is the 'else' clause. The current line being deleted is not zero
    ;; in length. First remove the line by moving to its start and then
    ;; killing, followed by deletion of the newline character, and then
    ;; finally restoration of the column position.
    (progn
      (beginning-of-line)
      (kill-line)
      (delete-char 1)
      (move-to-column previous-column))))

;; Now bind the delete line function to the F8 key
(global-set-key [f8] 'nuke-line)
;;(global-hl-line-mode 1)

;; Yes, I'm a web developer =/
(add-hook 'before-make-frame-hook '(lambda()
    (set-default-font "Monaco-12")
))
(add-hook 'message-mode-hook 'color-theme-tangotango)
(add-hook 'gnus-article-mode-hook 'color-theme-tangotango)

(autoload 'js-mode "js-mode" nil t)
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)


(load "~/.emacs.d/elisp/haml-mode.el")
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)

;; Python stuff !

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(autoload 'py-complete-init "py-complete")
;(add-hook 'python-mode-hook 'py-complete-init) pisses me off
(add-hook 'python-mode-hook 'flymake-mode)

;; better flymake colors
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "#ff3131" :foreground "#101010"))))
 '(rst-level-1-face ((t (:background "white" :foreground "black"))) t)
 '(rst-level-2-face ((t (:background "white" :foreground "black"))) t)
 '(rst-level-3-face ((t (:background "white" :foreground "black"))) t)
 '(rst-level-4-face ((t (:background "grey64"))) t))

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (append
       (list
        '("\\.sgm$" . sgml-mode)
        '("\\.zpt$" . html-mode)
        '("\\.html$" . html-mode)
        '("\\.xml$" . xml-mode)
        '("\\.md$" . markdown-mode)
        '("\\.markdown$" . markdown-mode)
        '(".emacs" . lisp-mode)
        '("emacs" . lisp-mode)
        '("\\.el$" . lisp-mode)
        '("Makefile.*" . makefile-mode)
        '("\\.pt$" . html-mode)
        '("\\.[hc]$" . c-mode)
        '("\\.py$" . python-mode)
        '("\\.migration$" . sql-mode)
        '("\\.sql$" . sql-mode)
        '("\\.rb$" . ruby-mode)
        '("Gemfile" . ruby-mode)
        '("Rakefile" . ruby-mode)
        '("\\.feature$" . ruby-mode)
        '("\\.ru$" . ruby-mode)
        '("\\.tex$" . tex-mode)
        '("\\.sh$" . shell-script-mode)
        '("\\.erl$" . erlang-mode)
        '("\\.php$" . php-mode)
        '("\\.acc$" . python-mode)
        '("\\.java$" . java-mode)
        '("\\.yml$" . yaml-mode)
        '("\\.yaml$" . yaml-mode)
        '("\\.haml$" . haml-mode)
        '("\\.rst$" . rst-mode)
        '("\\.css$" . css-mode)
        '("\\.less$" . css-mode)
        auto-mode-alist)))

(global-set-key (kbd "<up>") 'ignore)
(global-set-key (kbd "<down>") 'ignore)
(global-set-key (kbd "<left>") 'ignore)
(global-set-key (kbd "<right>") 'ignore)
(require 'linum)
(global-linum-mode)

;; Copyright (C) 2007 by Tapsell-Ferrier Limited

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;; Boston, MA  02110-1301  USA

;;(require 'twit)

(defvar twit-user "gabrielfalcao")

(fset 'yes-or-no-p 'y-or-n-p)
;; don't show so many messages on startup
;(setq inhibit-startup-message t)
;(setq inhibit-startup-echo-area-message t)
;; nice config file modes
(require 'generic-x)

;; electric bindings for help mode
(require 'ehelp)
(font-lock-add-keywords 'python-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

 (font-lock-add-keywords 'c-mode
  '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

  (font-lock-add-keywords 'latex-mode
   '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

(defun make-all ()
  (interactive)
  (shell-command "make all"))

(defun make-build ()
  (interactive)
  (shell-command "make build"))

(global-set-key (kbd "M-#") 'uncomment-region)
(global-set-key (kbd "C-#") 'comment-region)
(global-set-key (kbd "M-SPC") 'hippie-expand)
(global-set-key (kbd "<f9>") 'highlight-beyond-fill-column)

(global-set-key (kbd "<f8>") 'font-lock-fontify-buffer)
(global-set-key (kbd "C->") 'increase-left-margin)
(global-set-key (kbd "C-<") 'decrease-left-margin)

(global-set-key (kbd "C-.") 'increase-left-margin)
(global-set-key (kbd "C-,") 'decrease-left-margin)
;; compile/make
(global-set-key (kbd "<f5>")   'make-all)
(global-set-key (kbd "S-<f5>") 'make-build)

;; fullscreen editing
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(global-set-key (kbd "<f11>") 'switch-full-screen)

;;try to fix strange stuff in css mode.

;; clear trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))

(setq html-indent-level 4)
(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(put 'downcase-region 'disabled nil)
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")

(load-library "init_python")

(set-default-font "Monaco-12")

(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))

(global-set-key (kbd "<s-return>") 'maximize-frame)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;; (ns-toggle-fullscreen)
(maximize-frame)
(maximize-frame)
(maximize-frame)
(server-start)
(put 'upcase-region 'disabled nil)

(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (lwarn 'flymake :warning warning))

;; Using lwarn might be kind of annoying on its own, popping up windows and
;; what not. If you prefer to recieve the warnings in the mini-buffer, use:
(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (message warning))

;; (defcustom flymake-allowed-file-name-masks
;;   '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init)
;;     ;("\\.xml\\'" flymake-xml-init)
;;     ;("\\.html?\\'" flymake-xml-init)
;;     ("\\.cs\\'" flymake-simple-make-init)
;;     ("\\.p[ml]\\'" flymake-perl-init)
;;     ("\\.php[345]?\\'" flymake-php-init)
;;     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
;;     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
;;     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
;;     ("\\.tex\\'" flymake-simple-tex-init)
;;     ("\\.idl\\'" flymake-simple-make-init)
;;     ;; ("\\.cpp\\'" 1)
;;     ;; ("\\.java\\'" 3)
;;     ;; ("\\.h\\'" 2 ("\\.cpp\\'" "\\.c\\'")
;;     ;; ("[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\"" 1 2))
;;     ;; ("\\.idl\\'" 1)
;;     ;; ("\\.odl\\'" 1)
;;     ;; ("[0-9]+\\.tex\\'" 2 ("\\.tex\\'")
;;     ;; ("[ \t]*\\input[ \t]*{\\(.*\\)\\(%s\\)}" 1 2 ))
;;     ;; ("\\.tex\\'" 1)
;;     ))
