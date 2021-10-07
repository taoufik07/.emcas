;;; .emacs --- My custom emacs file
;;; Commentary:
;;; Gathered in the urge
;;; S/O internet

;;; Code:

;; Update GC thresholds during startup

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook (lambda ()
				  (setq gc-cons-threshold normal-gc-cons-threshold))))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory) user-emacs-directory)
	((boundp 'user-init-directory) user-init-directory)
	(t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "./conf/utils.el")
(load-user-file "./conf/init-misc-defaults.el")
(load-user-file "./conf/init-osx-keys.el")
(load-user-file "./conf/init-themes.el")
(load-user-file "./conf/init-packages.el")
(load-user-file "./conf/init-copy-paste-term.el")
(load-user-file "./conf/init-misc-packages.el")
(load-user-file "./conf/init-misc-editing.el")
(load-user-file "./conf/init-buffers.el")
(load-user-file "./conf/init-languages.el")


(provide 'init)
;;; init.el ends here
