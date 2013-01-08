
;;; image-dired+.el --- Image-dired extensions

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: image-dired extensions
;; URL: http://github.com/mhayashi1120/Emacs-image-diredx/raw/master/image-dired+.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.5.4

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install:

;; Please install the ImageMagick before installing this elisp.

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'image-dired+)

;;; Usage:

;; * To be disable the asynchronous feature
;;
;;  M-x image-diredx-async-mode

;;; TODO:

;; * Check on windows (file name has multibyte string)

;;; Code:

;;
;; image-dired extensions
;;

(eval-when-compile
  (require 'cl)
  (require 'easy-mmode))

(require 'advice)
(require 'image-dired)

(defvar image-diredx-async-advices
  '(
    (image-dired-display-thumbs image-diredx-display-thumbs)
    ))

;;;###autoload
(define-minor-mode image-diredx-async-mode
  "Extension for `image-dired' asynchrounous image thumbnail."
  :global t
  :group 'image+
  (funcall (if image-diredx-async-mode
               'ad-enable-advice
             'ad-disable-advice)
           'image-dired-display-thumbs 'around
           'image-diredx-display-thumbs)
  (ad-activate 'image-dired-display-thumbs))

(defadvice image-dired-display-thumbs
  (around image-diredx-display-thumbs
          (&optional arg append do-not-pop) disable)
  ;; arg non-nil means retrieving single file.
  (if arg
      (setq ad-return-value ad-do-it)
    (setq ad-return-value
          (image-diredx--display-thumbs append do-not-pop))))

(defun image-diredx--display-thumbs (&optional append do-not-pop)
  "Like `image-dired-display-thumbs' but asynchronously display thumbnails
of marked files.
"
  (let* ((buf (image-dired-create-thumbnail-buffer))
         (dir (dired-current-directory))
         (dired-buf (current-buffer))
         (items (loop for f in (dired-get-marked-files)
                      collect (list f dired-buf))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless append
          (erase-buffer)))
      (cd dir)
      (image-diredx--invoke-process items buf))
    (if do-not-pop
        (display-buffer image-dired-thumbnail-buffer)
      (pop-to-buffer image-dired-thumbnail-buffer))))

;; NOTE: duplicated from `image-dired-display-thumbs'
(defun image-diredx--prepare-line-up ()
  (cond
   ((eq 'dynamic image-dired-line-up-method)
    (image-dired-line-up-dynamic))
   ((eq 'fixed image-dired-line-up-method)
    (image-dired-line-up))
   ((eq 'interactive image-dired-line-up-method)
    (image-dired-line-up-interactive))
   ((eq 'none image-dired-line-up-method)
    nil)
   (t
    (image-dired-line-up-dynamic))))

(defun image-diredx--redisplay-window-function (frame)
  (when (eq major-mode 'image-dired-thumbnail-mode)
    (image-diredx--redisplay-list-with-point)))

(defun image-diredx--redisplay-list-with-point ()
  (let ((file (image-dired-original-file-name)))
    (image-diredx--prepare-line-up)
    (image-diredx--goto-file file)))

(defvar image-diredx--suppress-invoking nil)

(defun image-diredx--invoke-process (items thumb-buf)
  (when (and items
             (not (buffer-local-value
                   'image-diredx--suppress-invoking thumb-buf)))
    (let* ((item (car items))
           (curr-file (car item))
           (dired-buf (cadr item))
           (thumb-name (image-dired-thumb-name curr-file))
           (caller-is-ad (ad-is-active 'call-process)))
      ;; `flet' replace `call-process' definition
      ;; when `call-process' is advised.
      (when caller-is-ad
        (ad-deactivate 'call-process))
      (unwind-protect
          (flet ((call-process
                  (program &optional infile buffer display &rest args)
                  (apply 'start-process "image-dired+" nil program args)))
            (let ((proc
                   (if (or
                        (not (file-exists-p curr-file))
                        (and (file-exists-p thumb-name)
                             (file-newer-than-file-p thumb-name curr-file)))
                       ;;FIXME async trick
                       (start-process "image-dired+" nil shell-file-name
                                      shell-command-switch "")
                     (image-dired-create-thumb curr-file thumb-name))))
              (set-process-sentinel proc 'image-diredx--thumb-process-sentinel)
              (process-put proc 'thumb-name thumb-name)
              (process-put proc 'curr-file curr-file)
              (process-put proc 'dired-buf dired-buf)
              (process-put proc 'thumb-buf thumb-buf)
              (process-put proc 'items (cdr items))
              proc))
        (when caller-is-ad
          (ad-activate 'call-process))))))

(defun image-diredx--cleanup-processes ()
  (set (make-local-variable 'image-diredx--suppress-invoking) t)
  (unwind-protect
      (let (proc)
        (while (setq proc (get-process "image-dired+"))
          (delete-process proc)))
    (kill-local-variable 'image-diredx--suppress-invoking)))

(defun image-diredx--thumb-process-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (let ((thumb-name (process-get proc 'thumb-name))
          (curr-file (process-get proc 'curr-file))
          (dired-buf (process-get proc 'dired-buf))
          (thumb-buf (process-get proc 'thumb-buf))
          (items (process-get proc 'items)))
      (when (buffer-live-p thumb-buf)
        (unwind-protect
            (condition-case err
                (if (and (not (file-exists-p thumb-name))
                         (not (= 0 (process-exit-status proc))))
                    (message "Thumb could not be created for file %s" curr-file)
                  (image-diredx--thumb-insert
                   thumb-buf thumb-name curr-file dired-buf))
              (error (message "%s" err)))
          (image-diredx--invoke-process items thumb-buf))))))

(defun image-diredx--thumb-insert (buf thumb file dired)
  (with-current-buffer buf
    ;; save current point and filename.
    ;; restore point or filename.
    (let ((pf (image-dired-original-file-name))
          (pp (point)))
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          ;; `image-dired-insert-thumbnail' destroy current window (ex: minibuffer)
          (save-window-excursion
            (image-dired-insert-thumbnail thumb file dired))
          (image-diredx--prepare-line-up)))
      (cond
       (pf
        (image-diredx--goto-file pf))
       (pp
        (goto-char pp))))))

(defun image-diredx--goto-file (file)
  (let ((point (save-excursion
                 (goto-char (point-min))
                 (condition-case nil
                     (progn
                       (while (not (equal file (image-dired-original-file-name)))
                         (image-dired-forward-image))
                       (point))
                   (error nil)))))
    (when point
      (goto-char point))))

(defun image-diredx-next-line ()
  "`image-dired-next-line' with preserve column"
  (interactive)
  (let ((left (image-diredx--thumb-current-left)))
    (image-dired-next-line)
    (image-diredx--thumb-goto-column left)))

(defun image-diredx-previous-line ()
  "`image-dired-previous-line' with preserve column"
  (interactive)
  (let ((left (image-diredx--thumb-current-left)))
    (image-dired-previous-line)
    (image-diredx--thumb-goto-column left)))

(defun image-diredx--thumb-current-left ()
  (save-excursion
    (let ((first (point))
          (acc 0))
      (beginning-of-line)
      (while (< (point) first)
        (let* ((img (get-text-property (point) 'display))
               (size (image-size img)))
          (setq acc (+ acc
                       ;; calculate both side margin
                       (* (plist-get (cdr img) :margin) 2)
                       (car size) )))
        (image-dired-forward-image))
      acc)))

(defun image-diredx--thumb-goto-column (tleft)
  (let ((point
         (save-excursion
           (save-restriction
             (narrow-to-region (line-beginning-position) (line-end-position))
             (goto-char (point-min))
             (let ((left 0)
                   ;; diff between target and first column
                   (diff tleft)
                   hist)
               (condition-case nil
                   (while (or (null hist)
                              (progn
                                (setq left (image-diredx--thumb-current-left))
                                (setq diff (abs (- tleft left)))
                                ;; Break when incresing differences,
                                ;; this means obviously exceed target column
                                (<= diff (caar hist))))
                     (setq hist (cons (list diff (point)) hist))
                     (image-dired-forward-image))
                 (error nil))
               (cond
                ((null hist)
                 (point))
                ((or (null (cdr hist))
                     (> (car (cadr hist)) (car (car hist))))
                 (cadr (car hist)))
                (t
                 (cadr (cadr hist)))))))))
        (goto-char point)))

(defun image-diredx--thumb-revert-buffer (&rest ignore)
  (when image-diredx-async-mode
    (image-diredx--cleanup-processes)
    (let* ((bufs (image-diredx--associated-dired-buffers))
           (items (loop for b in bufs
                        if (buffer-live-p b)
                        append (with-current-buffer b
                                 (loop for f in (dired-get-marked-files)
                                       collect (list f b))))))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (image-diredx--invoke-process items (current-buffer)))))

(defun image-diredx--associated-dired-buffers ()
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (condition-case nil
          (while t
            (let ((buf (image-dired-associated-dired-buffer)))
              (unless (or (null buf) (memq buf res))
                (setq res (cons buf res))))
            (image-dired-forward-image))
        (error nil))
      (nreverse res))))

(defun image-diredx-flagged-delete ()
  "Execute flagged deletion with imaging confirmation, like `dired' buffer."
  (interactive)
  (let ((flagged
         (loop for buf in (image-diredx--associated-dired-buffers)
               append (with-current-buffer buf
                        (let* ((dired-marker-char dired-del-marker)
                               (files (dired-get-marked-files nil nil nil t)))
                          (cond
                           ;; selected NO file point of cursor filename
                           ((= (length files) 1)
                            nil)
                           ((eq (car files) t)
                            (cdr files))
                           (t files)))))))
    (cond
     ((null flagged)
      (message "(No deletions requested)"))
     ((not (image-diredx--confirm flagged))
      (message "(No deletions performed)"))
     (t
      (loop with count = 0
            with failures = '()
            for f in flagged
            do (condition-case err
                   (progn
                     (dired-delete-file f)
                     (incf count)
                     (image-diredx--delete-entry f)
                     (dired-fun-in-all-buffers
                      (file-name-directory f) (file-name-nondirectory f)
                      (function dired-delete-entry) f))
                 (error
                  (dired-log "%s\n" err)
                  (setq failures (cons f failures))))
            finally (if (not failures)
                        (message "%d deletion%s done"
                                 count (dired-plural-s count))
                      (dired-log-summary
                       (format "%d of %d deletion%s failed"
                               (length failures) count
                               (dired-plural-s count))
                       failures)))
      (image-diredx--redisplay-list-with-point)))))

(defun image-diredx--confirm (files)
  (let ((thumbs (loop for f in files
                      collect
                      (let ((thumb (image-dired-thumb-name f)))
                        (unless (file-exists-p thumb)
                          ;;TODO or insert only string?
                          (error "Thumbnail has not been created for %s" f))
                        thumb))))
    ;; TODO Show prompt buffer multiple times if exceed frame size.
    ;; same as dired.el
    (with-current-buffer (get-buffer-create " *Deletions*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; show all image as much as possible.
        (setq truncate-lines nil)
        (loop for thumb in thumbs
              do (insert-image (create-image
                                thumb nil nil
                                :relief image-dired-thumb-relief
                                :margin image-dired-thumb-margin))))
      (save-window-excursion
        (dired-pop-to-buffer (current-buffer))
        (funcall dired-deletion-confirmer "Delete image? ")))))

(defun image-diredx--delete-entry (file)
  (save-excursion
    (and (image-diredx--goto-file file)
         (let* ((cursor (point))
                (start (previous-single-property-change cursor 'display))
                (end (next-single-property-change cursor 'display))
                (inhibit-read-only t))
           (delete-region start end)))))

;;;###autoload
(defun image-diredx--setup ()
  "Setup image-dired extensions."
  (define-key image-dired-thumbnail-mode-map
    "x" 'image-diredx-flagged-delete)
  (set (make-local-variable 'revert-buffer-function)
       'image-diredx--thumb-revert-buffer)
  (add-hook 'window-size-change-functions
            'image-diredx--redisplay-window-function nil t))

;;;
;;; activate/deactivate marmalade install or github install.
;;;

;; For `unload-feature'
(defun image-dired+-unload-function ()
  (image-diredx-async-mode -1)
  (remove-hook 'image-dired-thumbnail-mode-hook 'image-diredx--setup))

;; setup key or any feature.
;;;###autoload(add-hook 'image-dired-thumbnail-mode-hook 'image-diredx--setup)
(add-hook 'image-dired-thumbnail-mode-hook 'image-diredx--setup)

;; activate the asynchronous mode
;;;###autoload(image-diredx-async-mode 1)
(image-diredx-async-mode 1)

(provide 'image-dired+)

;;; image-dired+.el ends here
