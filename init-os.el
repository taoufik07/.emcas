;;; My custom emacs config
    
(setq inhibit-startup-message t
      initial-scratch-message ";; Happy Hacking")

(setq global-display-line-numbers-mode t)

(setq line-number-mode t
      line-format "%d "
      column-number-mode t)

(which-function-mode t)
(global-auto-revert-mode t)

(display-battery-mode)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq scroll-conservatively 100)

(fset 'yes-or-no-p 'y-or-n-p)

;;; Previous window (Revese C-x o)
(global-set-key (kbd "C-x p") (lambda ()
                          (interactive)
                          (other-window -1)))

;; make sure that UTF-8 is used everywhere.
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


;; delete the selection on a new insertion
(delete-selection-mode)

;; show matching parenthesis and set delay to 0.
(setq show-paren-delay 0)
(show-paren-mode t)

;; Prevent opening new windows
(setq split-height-threshold nil
      split-width-threshold nil)

;; rgrep
(setq grep-save-buffers nil)

;; Translate escape sequences for mac OS
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])

(define-key input-decode-map "\e[1;9A" [M-up])
(define-key input-decode-map "\e[1;9B" [M-down])
(define-key input-decode-map "\e[1;9C" [M-right])
(define-key input-decode-map "\e[1;9D" [M-left])

;; Resize window
(global-set-key (kbd "<M-S-up>") 'shrink-window)
(global-set-key (kbd "<M-S-down>") 'enlarge-window)
(global-set-key (kbd "<M-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-S-right>") 'enlarge-window-horizontally)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
	     :ensure t)

;; (use-package better-defaults
;; 	     :ensure t)

(use-package magit
	     :ensure t
	     :bind ("C-x g" . 'magit-status))

(use-package find-file-in-project
	     :ensure t
	     :bind (("M-p" . 'find-file-in-project)))

(use-package dracula-theme
	     :ensure t
	     :config (load-theme 'dracula t))

(use-package elpy
	     :ensure t
	     :init
	     (setq elpy-rpc-python-command "python3")
	     (setq python-shell-interpreter "python3")
	     :config
	     (elpy-enable))

(use-package expand-region
  :ensure t
  :bind ("M-=" . 'er/expand-region)
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; (use-package auto-complete
;;   :ensure f
;;   :init
;;   (ac-config-default))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package terraform-mode
  :ensure t
  :hook ((terraform-mode . terraform-format-on-save-mode)))

(use-package yaml-mode
	     :ensure t)

(use-package org
  :ensure t
  :init
  (setq org-src-window-setup 'current-window)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (use-package htmlize :ensure t)
  (use-package org-bullets
    :ensure t
    :hook (org-mode . org-bullets-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package go-mode
  :ensure t
  :hook (go-mode . gofmt-before-save))

(use-package shfmt
  :ensure t
  :hook (sh-mode . shfmt-on-save-mode))

;; Init the auto complete modules
(ac-config-default)
(global-auto-complete-mode t)
(require 'go-autocomplete)

;; Enable auto-complete
(auto-complete-mode 1)

;; Define keymaps
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(global-set-key (kbd"C-c C-c") 'godef-jump)

;; Set some quick config vals
(setq ac-auto-start 1)
(setq ac-auto-show-menu 0.8)

;; Just to make sure go tools are enabled
(add-to-list 'exec-path "~/go/bin")

;; Automatically format code on save
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'auto-complete-for-go)

;; ?/// GO END <<<<<<<===========>>>>>>>>>>>>>>>

(use-package irony
	     :ensure t)

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
    (lambda ()
      (progn
        (when (locate-dominating-file "." ".clang-format")
          (clang-format-buffer))
        ;; Continue to save.
        nil))
    nil
    ;; Buffer local hook.
    t))

(use-package clang-format
  :ensure t
  :init (fset 'c-indent-region 'clang-format-region)
  :hook (c-mode . clang-format-save-hook-for-this-buffer))

(use-package neotree
	     :ensure t
	     :init
             (setq neo-window-fixed-size nil)
	     :bind (([F8] . 'neotree-toggle)))

(use-package iedit
  :ensure t
  :bind ("C-c ;" . 'iedit-mode))


(use-package js-auto-format-mode
    :ensure t
    :config
    (add-hook 'js-mode-hook #'js-auto-format-mode))

(use-package add-node-modules-path
    :ensure t
    :config
    (add-hook 'js-mode-hook #'add-node-modules-path))
    
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-linum-mode t)
 '(js-auto-format-command "prettier")
 '(js-auto-format-command-args "--write")
 '(package-selected-packages
   '(web-mode emmet-mode k8s-mode swiper shfmt rjsx-mode xref-js2 expand-region php-mode yaml-mode use-package try terraform-mode org-bullets neotree markdown-mode magit js2-mode js-auto-format-mode irony-eldoc iedit htmlize google-c-style go-mode flymake-google-cpplint flycheck-irony find-file-in-project f elpy dracula-theme company-irony-c-headers company-irony company-c-headers clang-format better-defaults auto-complete add-node-modules-path)))

(defun my/enable-auto-format-on-css ()
  (setq-local js-auto-format-command "prettier")
  (setq-local js-auto-format-command-args "--write --no-color")
  (js-auto-format-mode))

(add-hook 'css-mode-hook #'my/enable-auto-format-on-css)
    
;; (add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
;; (add-to-list 'default-frame-alist '(background-color . "#262626"))
;; (add-to-list 'default-frame-alist '(cursor-color . "red"))

(defun x-copy (beg end &optional region)
  ;; Copy to the system clipboard and emacs buffer 
  (interactive "r")
  (call-process-region beg end "pbcopy" nil nil nil "-selection" "c")
  (kill-ring-save beg end region))

(global-set-key (kbd "M-w") 'x-copy)

(defun x-cut (beg end &optional region)
  ;; Cut to the system clipboard and emacs buffer 
  (interactive "r")
  (call-process-region beg end "pbcopy" nil nil nil "-selection" "c")
  (kill-region beg end))

(global-set-key (kbd "C-w") 'x-cut)

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

(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))

(global-set-key (kbd "C-c c") 'daedreth/copy-whole-line)
(global-set-key (kbd "C-x x") 'kill-whole-line)

(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
    (end-of-line)
    (open-line 1)
    (forward-line ))

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (back-to-indentation))

(global-set-key (kbd "C-x <RET>") 'insert-line-above)
(global-set-key (kbd "C-c <RET>") 'insert-line-below)

;; New empty buffer
(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named ‚Äúuntitled‚Äù or ‚Äúuntitled<2>‚Äù, ‚Äúuntitled<3>‚Äù, etc.
It returns the buffer (for elisp programing)."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(global-set-key (kbd "M-n") 'new-empty-buffer)

(defun revert-all-buffers ()
  "Refresh all open buffers from their respective files."
  (interactive)

  (message "Begin reverting all buffers...")
  (let*
    (
      ;; pairs of (filename, buf)
      (ls
        (mapcar
          (lambda (buf) (list (buffer-file-name buf) buf))
          (seq-filter 'buffer-file-name (buffer-list))))
      (count (length ls))
      (index 1)
      )
    (while ls
      (cl-destructuring-bind (filename buf) (pop ls)
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers like *Messages*.
        (message "Reverting [%3d of %3d] %4d%%: %S"
          index count (round (* 100 (/ (float index) count))) filename)
        (if (file-exists-p filename)
          ;; If the file exists, revert the buffer.
          (with-demoted-errors "Error: %S"
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm))
            )
          ;; If the file doesn't exist, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer.
            (kill-buffer buf)
            (message "Killed non-existing file buffer: %s" buf)
            )
          )
        (setq index (1+ index))
        )
      )
    (message "Finished reverting %d file buffer(s)." count)
    )
  )

(global-set-key (kbd "C-c r") 'revert-all-buffers)


(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
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
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

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

;; JS
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-object-property ((t (:inherit font-lock-variable-name-face)))))

(use-package rjsx-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  )

(defun lint-js-fix-file ()
  (interactive)
  (message "prettier --write" (buffer-file-name))
  (shell-command (concat "yarn run lint:fix " (buffer-file-name))))

(defun lint-js-fix-file-and-revert ()
  (interactive)
  (lint-js-fix-file))

(add-hook 'js2-hook
          (lambda ()
            (add-hook 'before-save-hook #'lint-js-fix-file-and-revert)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

