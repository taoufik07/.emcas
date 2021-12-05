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

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "./conf/utils.el")
(load-user-file "./conf/init-osx-keys.el")
(load-user-file "./conf/init-misc-defaults.el")
(load-user-file "./conf/init-themes.el")
(load-user-file "./conf/init-packages.el")
(load-user-file "./conf/init-copy-paste-term.el")
(load-user-file "./conf/init-misc-packages.el")
(load-user-file "./conf/init-misc-editing.el")
(load-user-file "./conf/init-buffers.el")
(load-user-file "./conf/init-languages.el")


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(clipetty rjsx-mode go-mode rust-mode markdown-mode yaml-mode terraform-mode shfmt elpy flycheck iedit multiple-cursors async projectile magit try dracula-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
