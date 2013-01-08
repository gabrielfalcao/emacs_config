;;; image-dired+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (image-diredx--setup image-diredx-async-mode) "image-dired+"
;;;;;;  "image-dired+.el" (20678 26127))
;;; Generated autoloads from image-dired+.el

(defvar image-diredx-async-mode nil "\
Non-nil if Image-Diredx-Async mode is enabled.
See the command `image-diredx-async-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `image-diredx-async-mode'.")

(custom-autoload 'image-diredx-async-mode "image-dired+" nil)

(autoload 'image-diredx-async-mode "image-dired+" "\
Extension for `image-dired' asynchrounous image thumbnail.

\(fn &optional ARG)" t nil)

(autoload 'image-diredx--setup "image-dired+" "\
Setup image-dired extensions.

\(fn)" nil nil)
(add-hook 'image-dired-thumbnail-mode-hook 'image-diredx--setup)
(image-diredx-async-mode 1)

;;;***

;;;### (autoloads nil nil ("image-dired+-pkg.el") (20678 26127 367681))

;;;***

(provide 'image-dired+-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; image-dired+-autoloads.el ends here
