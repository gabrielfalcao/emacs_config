
;; used.

;; This version uses lwarn instead of message-box in the original version.
;; lwarn will open another window, and display the warning in there.
;; where to get the latest emacs snapshot gtk
;; deb http://emacs.orebokech.com sid main
;; deb-src http://emacs.orebokech.com sid main
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq default-directory "~/projects/")

;; Adding marmalade as a repo to the package module
(require 'package)
(add-to-list
 'package-archives
 '("marmalade" .
   "http://marmalade-repo.org/packages/")
 '("melpa" .
   "http://melpa.milkbox.net/packages/"))
(package-initialize)

(setenv "GOPATH" "~/projects/go-packages")
(setenv "PATH" (concat (getenv "PATH") ":" "~/projects/go-packages/bin"))
;;(setq exec-path (append exec-path (list (expand-file-name "~/projects/go-packages/bin"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(circe-default-nick "falcaogabriel")
 '(column-number-mode t)
 '(custom-safe-themes (quote ("dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "6f3060ac8300275c990116794e1ba897b6a8af97c51a0cb226a98759752cddcf" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "3341f6db5ac17e4174f7488c40676e7f0464f1e88519a59303dc7e7774245bbf" default)))
 '(inhibit-startup-echo-area-message "gabrielfalcao")
 '(initial-buffer-choice t)
 '(initial-scratch-message "# Be welcome, my master:
# I hope you're up for so much hacking.
        - Your very editor, Emacs

")
 '(jabber-account-list (quote ("gabrielfalcao@jabber-br.org")))
 '(menu-bar-mode nil)
 '(py-beep-if-tab-change nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(setq visible-bell t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tooltip-mode nil)
 '(transient-mark-mode t))
 '(coffee-tab-width 2)
;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;;'(default ((t (:stipple nil :background "black" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :family "misc-fixed")))))

(custom-set-faces
 '(flymake-errline ((((class color)) (:background "#FF80F4" :foreground "#333")))))
 '(flymake-warnline ((((class color)) (:background "#FF80F4" :foreground "#333")))))

(setq-default c-basic-offset 2)
;(setq-default vc-follow-symlinks t)
(setq-default c++-basic-offset 2)
(setq-default html-basic-offset 4)
(setq tab-width 4) ; or any other preferred value

(setq python-python-command "ipython")
;; Expanding the load-path
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
;;    (require 'tex-site)
;(set-default-font "Monospace-9")
;(set-fontset-font (frame-parameter nil 'font)
;                    'han '("cwTeXHeiBold" . "unicode-bmp"))
(setenv "PATH" (concat "/Users/gabrielfalcao/.nvm/v0.8.4/bin:/usr/local/bin:/usr/local/git/bin:" (concat (getenv "HOME") "/usr/bin:") (getenv "PATH")))

(set-default-font "Monaco-15")
(set-frame-font "Monaco-15")

; Lesscss mode
(load-file "~/.emacs.d/elisp/less-css-mode.el")
(require 'esk)
; Python mode
(load-file "~/.emacs.d/elisp/python.el")
(load-file "~/.emacs.d/elisp/sunrise-commander.el")
;; (add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
; Php mode
(load-file "~/.emacs.d/elisp/php-mode.el")
; Feature mode  (lettuce)
(load-file "~/.emacs.d/elisp/feature-mode.el")

; Coffee-script mode
(add-to-list 'load-path "~/.emacs.d/elisp/coffee-mode.el")
(require 'coffee-mode)
(setq coffee-tab-width 2)

(setenv "PYMACS_PYTHON" "python2.6")
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;; Auto completion inline
(load-file "~/.emacs.d/elisp/auto-complete.el") ;;loading
(require 'auto-complete);; preparing
(global-auto-complete-mode t) ;;enabling
(global-auto-revert-mode t)  ;; auto revert files that were changed
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-dwim t)

(load-file "~/.emacs.d/elisp/two-mode-mode.el")
(load-file "~/.emacs.d/elisp/bar-cursor.el")
(load-file "~/.emacs.d/elisp/maxframe.el")
;; making new emacs windows have a pretty font
;;(add-to-list 'default-frame-alist '(font . "Bitstream Vera SansMono-8"))

(require 'python)
;; color theme
(require 'color-theme)


(load "~/.emacs.d/elisp/color-theme-twilight.el")
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
(load "~/.emacs.d/elisp/color-theme-tomorrow.el")


(setq color-theme-is-global t)
;;(color-theme-tomorrow-night)
;; (setq color-theme-is-global nil)
(setq-default indent-tabs-mode nil)
(load-theme 'spolsky t)
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

;; Turning regexp into the default search method

;; (global-set-key [(ctrl c) (c)] 'comment-region)

(global-set-key [(ctrl x) (r) (w)] 'kill-rectangle)
(global-set-key [(ctrl x) (r) (y)] 'yank-rectangle)
(global-set-key "\C-s" 'isearch-forward-regexp)


;; Now bind the delete line function to the F8 key
(global-set-key [f8] 'nuke-line)
;;(global-hl-line-mode 1)

(add-hook 'message-mode-hook 'color-theme-tangotango)
(add-hook 'gnus-article-mode-hook 'color-theme-tangotango)

(autoload 'js-mode "js-mode" nil t)
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)


(load "~/.emacs.d/elisp/haml-mode.el")
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rst-level-1-face ((t (:background "white" :foreground "black"))) t)
 '(rst-level-2-face ((t (:background "white" :foreground "black"))) t)
 '(rst-level-3-face ((t (:background "white" :foreground "black"))) t)
 '(rst-level-4-face ((t (:background "grey64"))) t))

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

(require 'feature-mode)
(require 'web-mode)
(require 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(global-set-key (kbd "<backtab>") 'zencoding-expand-line)

(set-face-attribute 'web-mode-doctype-face nil :foreground (face-foreground font-lock-function-name-face))
(set-face-attribute 'web-mode-html-tag-face nil :foreground (face-foreground font-lock-function-name-face))
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground (face-foreground font-lock-variable-name-face))
(set-face-attribute 'web-mode-html-attr-value-face nil :foreground (face-foreground font-lock-type-face))

(setq auto-mode-alist
      (append
       (list
        '("\\.sgm$" . sgml-mode)
        '("\\.zpt$" . html-mode)
        '("\\.m$" . objc-mode)
        '("\\.mm$" . objc-mode)
        '("\\.html$" . web-mode)
        '("\\.svg$" . web-mode)
        '("\\.phtml\\'" . web-mode)
        '("\\.tpl\\.php\\'" . web-mode)
        '("\\.jsp\\'" . web-mode)
        '("\\.as[cp]x\\'" . web-mode)
        '("\\.erb\\'" . web-mode)
        '("\\.mustache\\'" . web-mode)
        '("\\.djhtml\\'" . web-mode)
        '("\\.xml$" . xml-mode)
        '("\\.md$" . markdown-mode)
        '("\\.markdown$" . markdown-mode)
        '(".emacs" . lisp-mode)
        '("\\.coffee$" . coffee-mode)
        '("\\.Cakefile$" . coffee-mode)
        '("emacs" . lisp-mode)
        '("\\.el$" . lisp-mode)
        '("Makefile.*" . makefile-mode)
        '("\\.pt$" . html-mode)
        '("\\.[hc]$" . c-mode)
        '("sagacity" . sh-mode)
        '("\\.cpp$" . c++-mode)
        '("\\.cc$" . c++-mode)
        '("\\.hh$" . c++-mode)
        '("\\.mm$" . objc-mode)
        '("\\.sip$" . c++-mode)
        '("\\.py$" . describe-mode)
        '("\\.migration$" . sql-mode)
        '("\\.sql$" . sql-mode)
        '("\\.rb$" . ruby-mode)
        '("Gemfile" . ruby-mode)
        '("Vagrantfile" . ruby-mode)
        '("Berksfile" . ruby-mode)
        '("Rakefile" . ruby-mode)
        '("\\.feature$" . feature-mode)
        '("\\.ru$" . ruby-mode)
;;        '("\\.tex$" . latex-mode)
        '("\\.sh$" . shell-script-mode)
        '(".*bash.*$" . shell-script-mode)
        '("\\.erl$" . erlang-mode)
        '("\\.php$" . php-mode)
        '("\\.go$" . go-mode)
        '("\\.acc$" . describe-mode)
        '("\\.java$" . java-mode)
        '("\\.yml$" . yaml-mode)
        '("\\.yaml$" . yaml-mode)
        '("\\.haml$" . haml-mode)
        '("\\.jade$" . haml-mode)
        '("\\.rst$" . rst-mode)
        '("\\.css$" . css-mode)
        '("\\.less$" . less-css-mode)
        '("\\.sass$" . less-css-mode)
        '("\\.scss$" . less-css-mode)
        auto-mode-alist)))


;; <Objective-C>

;; (add-to-list 'magic-mode-alist
;;                 `(,(lambda ()
;;                      (and (string= (file-name-extension buffer-file-name) "h")
;;                           (re-search-forward "@\\<interface\\>"
;; 					     magic-mode-regexp-match-limit t)))
;;                   . objc-mode))

(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))

(defadvice ff-get-file-name (around ff-get-file-name-framework
				    (search-dirs
				     fname-stub
				     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
   (or
    (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
	(let* ((framework (match-string 1 fname-stub))
	       (header (match-string 2 fname-stub))
	       (fname-stub (concat framework ".framework/Headers/" header)))
	  ad-do-it))
      ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)
(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
			      "/System/Library/Frameworks" "/Library/Frameworks"))

;; </Objective-C>
;; (global-set-key (kbd "<up>") 'ignore)
;; (global-set-key (kbd "<down>") 'ignore)
;; (global-set-key (kbd "<left>") 'ignore)
;; (global-set-key (kbd "<right>") 'ignore)
(require 'linum)
(global-linum-mode)



(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))
(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'dired-zip-files))
(defun dired-zip-files (zip-file)
  "Create an archive containing the marked files."
  (interactive "sEnter name of zip file: ")

  ;; create the zip file
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command
     (concat "zip "
             zip-file
             " "
             (concat-string-list
              (mapcar
               '(lambda (filename)
                  (file-name-nondirectory filename))
               (dired-get-marked-files))))))

  (revert-buffer)

  ;; remove the mark on all the files  "*" to " "
  ;; (dired-change-marks 42 ?\040)
  ;; mark zip file
  ;; (dired-mark-files-regexp (filename-to-regexp zip-file))
  )

(defun concat-string-list (list)
   "Return a string which is a concatenation of all elements of the list separated by spaces"
    (mapconcat '(lambda (obj) (format "%s" obj)) list " "))
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
(font-lock-add-keywords 'describe-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

 (font-lock-add-keywords 'c-mode
  '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

;;  (font-lock-add-keywords 'latex-mode
;;   '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

(defun make-all ()
  (interactive)
  (shell-command "make all"))

(defun make-build ()
  (interactive)
  (shell-command "make build"))
(defun make-test ()
  (interactive)
  (shell-command "make test"))
(defun make-unit ()
  (interactive)
  (shell-command "make unit"))
(defun make-functional ()
  (interactive)
  (shell-command "make functional"))
(defun make-release ()
  (interactive)
  (shell-command "make release"))
(defun make-docs ()
  (interactive)
  (shell-command "make docs"))
(defun make-integration ()
  (interactive)
  (shell-command "make integration"))

(global-set-key (kbd "C-O") 'next-multiframe-window)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-#") 'comment-region)
(global-set-key (kbd "M-SPC") 'hippie-expand)
(global-set-key (kbd "<f9>") 'highlight-beyond-fill-column)

(global-set-key (kbd "<f8>") 'font-lock-fontify-buffer)
(global-set-key (kbd "C->") 'increase-left-margin)
(global-set-key (kbd "C-<") 'decrease-left-margin)

(global-set-key (kbd "C-.") 'increase-left-margin)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-,") 'decrease-left-margin)
;; compile/make
(global-set-key (kbd "<f5>")   'make-all)
(global-set-key (kbd "S-<f5>") 'make-build)

;; fullscreen editing
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(global-set-key (kbd "M-RET") 'switch-full-screen)

;;try to fix strange stuff in css mode.

;; clear trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 2)
            (setq indent-tabs-mode t)))

(setq html-indent-level 2)
(setq cssm-indent-level 2)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

(put 'downcase-region 'disabled nil)
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("Jakefile$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(load-library "init_python")



(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))

(global-set-key (kbd "<s-return>") 'maximize-frame)
;;(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;; (ns-toggle-fullscreen)

(put 'upcase-region 'disabled nil)

(setq initial-major-mode 'python-mode)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "#D1F9FF" :foreground "#333")))))

(setq flymake-python-pyflakes-executable "/usr/local/bin/flake8")
(setq flymake-python-pyflakes-extra-arguments '("--ignore=E801"))
(require 'flymake-cursor)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(tool-bar-mode 0)
(setq mouse-wheel-progressive-speed nil)
;(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(defun beautify-json ()
  (interactive)
    (let ((b (if mark-active (min (point) (mark)) (point-min)))
            (e (if mark-active (max (point) (mark)) (point-max))))
                (shell-command-on-region b e
                     "python -mjson.tool" (current-buffer) t)))

(defun kill-all-buffers-mercilessly ()
  "*DANGEROUS* function that kills all the buffers mercilessly

I suggest you to DO NOT bind it to any keyboard shortcut and
please, be careful, once called, it can't be stopped!"
  (interactive)
  (mapcar '(lambda (b)
             (ignore-errors
               (revert-buffer 1 1))
             (kill-buffer b))
          (buffer-list)))

(defun clear-blank-lines ()
  (interactive)
  (goto-char 1)
  (flush-lines "^\t*$" nil t)
  (flush-lines "^[ ]*$" nil t))

(load-file "~/.private.el")

(setq circe-network-options
      `(("Freenode"
         :nick "falcaogabriel"
         :channels ("#yipit" "#guake", "#python", "#python-dev", "#github")
         :nickserv-password ,freenode-password
         )))
(defun irc ()
  "Connect to IRC"
  (interactive)
  (circe "Freenode"))

(when (eq system-type 'darwin)
 ;; default Latin font (e.g. Consolas)
 (set-face-attribute 'default nil :family "Monaco")
 ;; default font size (point * 10)
 ;;
 ;; WARNING!  Depending on the default font,
 ;; if the size is not supported very well, the frame will be clipped
 ;; so that the beginning of the buffer may not be visible correctly.
 (set-face-attribute 'default nil :height 150)
 )
(setq flymake-gui-warnings-enabled nil)
;; (load "~/.emacs.d/elisp/smartparens.el")
;; (smartparens-global-mode 1)

;; Changing the frame title to show my host name and full path of file open on
;; the current buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; go flymake
;; (add-to-list 'load-path "~/projects/go-packages/src/github.com/dougm/goflymake")
;; (require 'go-flymake)
;; (require 'go-flycheck)

;; transparency
(defun set-frame-alpha (arg &optional active)
  (interactive "nEnter alpha value (1-100): \np")
  (let* ((elt (assoc 'alpha default-frame-alist))
         (old (frame-parameter nil 'alpha))
         (new (cond ((atom old)     `(,arg ,arg))
                    ((eql 1 active) `(,arg ,(cadr old)))
                    (t              `(,(car old) ,arg)))))
    (if elt (setcdr elt new) (push `(alpha ,@new) default-frame-alist))
    (set-frame-parameter nil 'alpha new)))
(global-set-key (kbd "C-c t") 'set-frame-alpha)
(set-frame-alpha 97)

;; Auto complete
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-dwim 2)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(require 'find-file-in-repository)
(global-set-key (kbd "C-x f") 'find-file-in-repository)
(server-force-delete)
(server-mode)
