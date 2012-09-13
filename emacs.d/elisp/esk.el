;;; esk.el --- Emacs search kit

;; Copyright (C) 2012  Lincoln de Sousa and Suneel Chakravorty

;; Author: Suneel Chakravorty <suneel0101@gmail.com>
;; Author: Lincoln de Sousa <lincoln@comum.org>
;; Keywords: search, find
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a very simple extention to search for files and for contents
;; inside files.
;; 
;; Enjoy!

;;; Code:




(defun esk-find-file (pattern)
  "Searches for a pattern"
  (interactive "sPattern to search: ")
  (esk-find-nearest-git-directory (esk-get-current-buffer-directory))
  (esk-fire-command (esk-find-nearest-git-directory (esk-get-current-buffer-directory)) pattern))


(defun esk-find-top-dir (flag dir)
  "Looks for a directory that contains a directory called `flag' and stops at `/'"
  (if (or (equal dir "/") (file-exists-p (concat dir flag)))
      dir
      (esk-find-top-dir flag (expand-file-name (concat dir "../")))))


(defun esk-find-nearest-git-directory (dir)
  "Looks for the nearest directory containing a .git directory"
  (esk-find-top-dir ".git" dir))


(defun esk-get-current-buffer-directory ()
  "Returns the directory of the current buffer"
  (if (file-directory-p buffer-file-name)
      buffer-file-name
     (concat
      (mapconcat 'identity (butlast (split-string buffer-file-name "/") 1) "/")
      "/")))
     

(defun esk-fire-command (dir pattern)
  "Issues the find command to search matching the given `pattern'"
  (shell-command (concat "find " dir " -name '*" pattern "*'")))


(provide 'esk)

;;; esk.el ends here
