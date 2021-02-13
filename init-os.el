;;; .emacs --- My custom emacs file
;;; Commentary:
;;; Gathered in the urge
;;; S/O internet

;;; Code:

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook (lambda ()
				  (setq gc-cons-threshold normal-gc-cons-threshold))))

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type) ,@body))

;;; My defaults
(display-battery-mode)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-conservatively 100)

(setq line-number-mode t line-format "%d " column-number-mode t)

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

;;; UTF-8 everywhere.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)
(setq-default buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Defaults hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Useful keybindings
;;; Previous window (Revese C-x o)
(global-set-key (kbd "C-x p")
		(lambda ()
		  (interactive)
		  (other-window -1)))
;; Resize window
(global-set-key (kbd "<M-S-up>") 'shrink-window)
(global-set-key (kbd "<M-S-down>") 'enlarge-window)
(global-set-key (kbd "<M-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-S-right>") 'enlarge-window-horizontally)

(with-system darwin
  ;; Translate escape sequences for mac OS
  (define-key input-decode-map "\e[1;10A" [M-S-up])
  (define-key input-decode-map "\e[1;10B" [M-S-down])
  (define-key input-decode-map "\e[1;10C" [M-S-right])
  (define-key input-decode-map "\e[1;10D" [M-S-left])
  (define-key input-decode-map "\e[1;9A" [M-up])
  (define-key input-decode-map "\e[1;9B" [M-down])
  (define-key input-decode-map "\e[1;9C" [M-right])
  (define-key input-decode-map "\e[1;9D" [M-left]))

;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; Put Emacs auto-save and backup files to /tmp/ or C:/Temp/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-by-copying t		; Avoid symlinks
      version-control t delete-old-versions t kept-new-versions 10 kept-old-versions 5
      auto-save-list-file-prefix emacs-tmp-dir auto-save-file-name-transforms `((".*" ,emacs-tmp-dir
										 t)) ; Change autosave dir to tmp
      backup-directory-alist `((".*" . ,emacs-tmp-dir)))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

;;; Misc: Copy and Cut to the system clipboard and emacs buffer
(defconst copy-system-cmd "xclip")
(with-system darwin
  (setq copy-system-cmd "pbcopy"))

(defun x-copy (beg end &optional region)
  "Copy to system clipboard."
  (interactive "r")
  (call-process-region beg end copy-system-cmd nil nil nil "-selection" "c")
  (kill-ring-save beg end region))

(global-set-key (kbd "M-w") 'x-copy)

(defun x-cut (beg end &optional region)
  "Cut to system clipboard."
  (interactive "r")
  (call-process-region beg end copy-system-cmd nil nil nil "-selection" "c")
  (kill-region beg end))

(global-set-key (kbd "C-w") 'x-cut)

;;; Misc: Window Split
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)


(defun move-text-internal (arg)
  (cond ((and
	  mark-active
	  transient-mark-mode)
	 (if (> (point)
		(mark))
	     (exchange-point-and-mark))
	 (let ((column (current-column))
	       (text (delete-and-extract-region (point)
						(mark))))
	   (forward-line arg)
	   (move-to-column column t)
	   (set-mark (point))
	   (insert text)
	   (exchange-point-and-mark)
	   (setq deactivate-mark nil)))
	(t (beginning-of-line)
	   (when (or (> arg 0)
		     (not (bobp)))
	     (forward-line)
	     (when (or (< arg 0)
		       (not (eobp)))
	       (transpose-lines arg))
	     (forward-line -1)))))


(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
	(save-excursion (indent-rigidly (region-beginning)
					(region-end) distance)
			(push-mark mark t t)
			(setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
		    (line-end-position) distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text 4)
  ;;(forward-char 4)
  )

(defun shift-left (count)
  (interactive "p")
  (backward-char 4)
  (shift-text (- 4)))

(global-set-key (kbd "<M-right>") 'shift-right)
(global-set-key (kbd "<M-left>") 'shift-left)

;;;
(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-c <RET>") 'insert-line-below)

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<M-RET>") 'insert-line-above)

;; New empty buffer
(defun new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

(global-set-key (kbd "M-n") 'new-empty-buffer)


(defun revert-all-buffers ()
  "Refresh all open buffers from their respective files."
  (interactive)
  (message "Begin reverting all buffers...")
  (let* (
	 ;; pairs of (filename, buf)
	 (ls (mapcar (lambda (buf)
		       (list (buffer-file-name buf) buf))
		     (seq-filter 'buffer-file-name (buffer-list))))
	 (count (length ls))
	 (index 1))
    (while ls (cl-destructuring-bind (filename buf)
		  (pop ls)
		;; Revert only buffers containing files, which are not modified;
		;; do not try to revert non-file buffers like *Messages*.
		(message "Reverting [%3d of %3d] %4d%%: %S" index count (round (* 100 (/ (float
											  index)
											 count)))
			 filename)
		(if (file-exists-p filename)
		    ;; If the file exists, revert the buffer.
		    (with-demoted-errors "Error: %S" (with-current-buffer buf (revert-buffer
									       :ignore-auto
									       :noconfirm)))
		  ;; If the file doesn't exist, kill the buffer.
		  (let (kill-buffer-query-functions) ; No query done when killing buffer.
		    (kill-buffer buf)
		    (message "Killed non-existing file buffer: %s" buf)))
		(setq index (1+ index))))
    (message "Finished reverting %d file buffer(s)." count)))

(global-set-key (kbd "C-c r") 'revert-all-buffers)


;;; Packages
(use-package
  try
  :ensure t)

;;; TODO: Use this hash for iterm on macos https://raw.githubusercontent.com/dracula/emacs/3e6bbd3618316aa8e2b354e8ad14008589f6fdb9/dracula-theme.el
(use-package
  dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

(use-package
  magit
  :ensure t
  :bind ("C-x g" . 'magit-status))

;;; Use async whenever it's possible
(use-package
  async
  :ensure t
  :init (dired-async-mode 1))

;;; Packages: Misc
(use-package
  find-file-in-project
  :ensure t
  :bind (("M-p" . 'find-file-in-project)))

(use-package
  multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package
  iedit
  :ensure t
  :bind ("C-c ;" . 'iedit-mode))

(use-package
  flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package
  org
  :ensure t
  :init (setq org-src-window-setup 'current-window)
  :hook ((org-mode . visual-line-mode)
	 (org-mode . org-indent-mode))
  :config (use-package
	    htmlize
	    :ensure t)
  (use-package
    org-bullets
    :ensure t
    :hook (org-mode . org-bullets-mode)))

;;; Packages: prog mode
(use-package
  elpy
  :ensure t
  :init (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  :config (elpy-enable))

(use-package
  elisp-format
  :ensure t
  :hook (before-save . elisp-format-buffer))

(use-package
  shfmt					; Shell format
  :ensure t
  :hook (sh-mode . shfmt-on-save-mode))

(use-package
  terraform-mode
  :ensure t
  :hook ((terraform-mode . terraform-format-on-save-mode)))

(use-package
  yaml-mode
  :ensure t)

(use-package
  markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package
  go-mode
  :defer t
  :init (progn (add-to-list 'exec-path "~/go/bin")
	       (setq gofmt-command "goimports")
	       (add-hook 'before-save-hook 'gofmt-before-save)
	       (add-hook 'go-mode-hook 'auto-complete-for-go))
  ;; (bind-key "M-." 'godef-jump))
  ;; :bind (
  ;; ("M-." . lsp-find-definition)
  ;; ("C-c C-d" . lsp-describe-thing-at-point))
  :hook ((before-save . gofmt-before-save)
	 (go-mode . auto-complete-for-go)
	 ;; (go-mode . lsp-deferred)
	 ;; (before-save . lsp-format-buffer)
	 ;; (before-save . lsp-organize-imports)
	 ))

;;; Web

;; (defun my/enable-auto-format-on-css ()
;;   (setq-local js-auto-format-command "prettier")
;;   (setq-local js-auto-format-command-args "--write --no-color")
;;   (js-auto-format-mode))

;; (add-hook 'css-mode-hook #'my/enable-auto-format-on-css)

(use-package
  rjsx-mode
  :ensure t
  :mode ("\\.js[x]\\'" . rjsx-mode))
;; :hook (before-save . lint-js-fix-file))

;; (defun taoufik/use-node-modules-bin ()
;;   (let* ((root (locate-dominating-file (or (buffer-file-name) default-directory) "node_modules"))
;;          (eslint (and root (expand-file-name "node_modules/.bin/eslint" root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (defun lint-js-fix-file ()
;;   (interactive)
;;   (message "Linting...")
;;     (message  (concat (taoufik/use-node-modules-bin) " --fix "  (buffer-file-name)))
;;   (shell-command (concat (taoufik/use-node-modules-bin) " --fix "  (buffer-file-name))))
;; ;;; dasd

;; (defun lint-js-fix-file ()
;;   (interactive)
;;   (message "prettier --write" (buffer-file-name))
;;   (shell-command (concat "yarn run lint:fix " (buffer-file-name))))

;; (defun lint-js-fix-file-and-revert ()
;;   (interactive)
;;   (lint-js-fix-file))

;; (add-hook 'js2-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook #'lint-js-fix-file-and-revert)))

(provide 'init)
;;;
