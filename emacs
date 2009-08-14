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
 '(face-font-family-alternatives (quote (("Monaco-10") ("helv" "helvetica" "arial" "fixed"))))
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

;; Expanding the load-path
(setq load-path (cons "~/.emacs.d/elisp/" load-path))
;(set-default-font "Monospace-9")
;(set-fontset-font (frame-parameter nil 'font)
;                    'han '("cwTeXHeiBold" . "unicode-bmp"))
(set-default-font "Monaco-11")
; Python mode
;(load-file "~/.emacs.d/elisp/python.el")

;; making new emacs windows have a pretty font
;;(add-to-list 'default-frame-alist '(font . "Bitstream Vera SansMono-8"))

;; color theme
(require 'color-theme)

(load "~/.emacs.d/elisp/color-theme-twilight.el")
(load "~/.emacs.d/elisp/color-theme-tango.el")
(load "~/.emacs.d/elisp/color-theme-blackboard.el")
(load "~/.emacs.d/elisp/color-theme-zenburn.el")
(load "~/.emacs.d/elisp/color-theme-sunburst.el")

(setq color-theme-is-global t)
(color-theme-sunburst)
;;(color-theme-blippblopp) ;; best light theme ! (default)
;;(color-theme-ld-dark) ;; pretty cool dark theme
;;(color-theme-hober) ;; good dark theme
;;(color-theme-andreas) ;; almost good light theme
(setq color-theme-is-global nil)
(setq-default indent-tabs-mode nil)

;; Exploring more goodies =)
(tool-bar-mode)
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
(add-hook 'javascript-mode-hook 'js-mode)
(add-hook 'before-make-frame-hook '(lambda()
    (set-default-font "Monaco-10")
))
(autoload 'js-mode "js-mode" nil t)
(autoload 'css-mode "css-mode")

(setq auto-mode-alist
     (cons '("\\.js\\'" . javscript-mode) auto-mode-alist))
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)

;; Python stuff !
;;(autoload 'py-complete-init "py-complete")
;;(add-hook 'python-mode-hook 'py-complete-init)
;;(add-hook 'python-mode-hook 'flymake-mode)

(setq auto-mode-alist
      (append
       (list
        '("\\.sgm$" . sgml-mode)
        '("\\.zpt$" . html-mode)
        '("\\.html$" . html-mode)
        '("\\.pt$" . html-mode)
        '("\\.acc$" . ruby-mode)
        auto-mode-alist)))

;; keybindings para o tabbar (Ps.: Essa porra não tá funfando...)
;; (tabbar-mode)
;; (define-key global-map [C-tab] 'tabbar-forward-tab)
;; (define-key global-map [C-S-iso-lefttab] 'tabbar-backward-tab)
(global-set-key (kbd "<up>") 'ignore)
(global-set-key (kbd "<down>") 'ignore)
(global-set-key (kbd "<left>") 'ignore)
(global-set-key (kbd "<right>") 'ignore)
(require 'linum)
(global-linum-mode)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "Red" :foreground "white"))))
 '(flymake-warnline ((((class color)) (:background "Cyan" :foreground "black")))))

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

(require 'twit)

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
(global-set-key (kbd "<f5>")   'recompile)
(global-set-key (kbd "S-<f5>") 'compile)

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
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(pymacs-load "bikeemacs" "brm-")
(brm-init)

