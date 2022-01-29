;;; init-languages.el --- Setup languages
;;; Commentary:

;;; Code:

(use-package org)

(use-package
  flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package
  which-key
  :ensure t
  :init (which-key-mode))

(use-package
  elpy
  :ensure t
  :init (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  :config (elpy-enable))

(use-package
  pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package
  pyenv-mode
  :ensure t)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

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
  rust-mode
  :ensure t
  :init (setq rust-format-on-save t))

(use-package
  go-mode
  :ensure t
  :init (progn (add-to-list 'exec-path "~/go/bin")
	       (setq gofmt-command "goimports")
	       (add-hook 'before-save-hook 'gofmt-before-save)
	       ;; (add-hook 'go-mode-hook 'auto-complete-for-go)
	       (add-hook 'go-mode-hook (lambda ()
					 (setq tab-width 4))))
  :hook ((before-save . gofmt-before-save)
	 ;;(go-mode . auto-complete-for-go)
	 ))

(use-package
  add-node-modules-path
  :ensure t
  :hook ((flycheck-mode-hook . add-node-modules-path)))

(use-package
  rjsx-mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)
		 ("\\.jsx\\'" . rjsx-mode))
   :hook ((after-save . prettier-fix-file-and-revert)))

;; (add-hook 'rjsx-mode
;;           (lambda ()
;;             (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

(defun prettier-fix-file ()
  (interactive)
  (message "prettier --fixing the file" (buffer-file-name))
  (call-process-shell-command
    (concat "yarn run prettier --write " (buffer-file-name)))
   nil "*Shell Command Output*" t)

(defun prettier-fix-file-and-revert ()
  (interactive)
  (when (eq major-mode 'rjsx-mode)
	(prettier-fix-file)
	(revert-buffer t t)))

(provide 'init-languages)
;;; init-languages.el ends here
