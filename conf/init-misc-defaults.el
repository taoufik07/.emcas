;;; init-misc-defaults.el --- Setup misc defaults options
;;; Commentary:

;;; Code:


(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t initial-scratch-message ";; Happy Hacking")

(delete-selection-mode t)
(global-auto-revert-mode t)
(global-display-line-numbers-mode t)

;; rgrep
;; (setq grep-save-buffers nil)

;;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/my-temp-backups")))

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/my-temp-backups" t)))

;;; show matching parenthesis and set delay to 0.
(show-paren-mode t)
(setq show-paren-delay 0)

;; Prevent opening new windows
(setq split-height-threshold nil split-width-threshold nil)

(defun delete-word (arg)
  "Delete word"
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
(global-set-key (kbd "M-DEL") 'backward-delete-word)

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

;; Resize window
(global-set-key (kbd "<M-S-up>") 'shrink-window)
(global-set-key (kbd "<M-S-down>") 'enlarge-window)
(global-set-key (kbd "<M-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-S-right>") 'enlarge-window-horizontally)

(provide 'init-misc-defaults)
;;; init-misc-defaults.el ends here
