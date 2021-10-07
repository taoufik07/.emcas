;;; init-misc-defaults.el --- Setup misc defaults options
;;; Commentary:

;;; Code:


(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t initial-scratch-message ";; Happy Hacking")

(delete-selection-mode t)
(global-auto-revert-mode t)

;; rgrep
(setq grep-save-buffers nil)

;;; show matching parenthesis and set delay to 0.
(show-paren-mode t)
(setq show-paren-delay 0)

;; Prevent opening new windows
(setq split-height-threshold nil split-width-threshold nil)

;;; Locals
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(setq coding-system-for-read 'utf-8 coding-system-for-write 'utf-8)
(set-input-method nil)
(setq-default buffer-file-coding-system 'utf-8)

;;; Global keybindings
;;; Useful keybindings
;;; Previous window (Revese C-x o)
(global-set-key (kbd "C-x p")
		(lambda ()
		  (interactive)
		  (other-window -1)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-misc-defaults)
;;; init-misc-defaults.el ends here
