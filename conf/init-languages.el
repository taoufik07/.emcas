;;; init-languages.el --- Setup languages
;;; Commentary:

;;; Code:

(use-package
  flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package
  which-key
  :ensure t
  :init (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  (elpy-enable))

;; (require 'flymake-ruff)
;; (add-hook 'python-mode-hook #'flymake-ruff-load)

(use-package flymake-ruff
  :ensure t
  :init
  :hook (python-mode .  flymake-ruff-load))

;; (use-package
;;   pipenv
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

;; (use-package
;;   pyenv-mode
;;   :ensure t)

;; (defun projectile-pyenv-mode-set ()
;;   "Set pyenv version matching project name."
;;   (let ((project (projectile-project-name)))
;;     (if (member project (pyenv-mode-versions))
;;         (pyenv-mode-set project)
;;       (pyenv-mode-unset))))

;; (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

(use-package
  shfmt
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
	       (add-hook 'go-mode-hook 'auto-complete-for-go)
	       (add-hook 'go-mode-hook (lambda ()
					 (setq tab-width 4)))))

(use-package
  add-node-modules-path
  :ensure t
  :hook ((flycheck-mode-hook . add-node-modules-path)))

(use-package
  rjsx-mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)
	 ("\\.jsx\\'" . rjsx-mode)
	 ("\\.ts\\'" . rjsx-mode)
	 ("\\.tsx\\'" . rjsx-mode))
  ;; :hook ((after-save . prettier-fix-file-and-revert))
  )

(add-hook 'rjsx-mode
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

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

;; (add-hook 'typescript-mode
;;           (lambda ()
;;             (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

(use-package
  typescript-mode
  :ensure t
  :mode  (("\\.ts\\'" . typescript-mode)
		  ("\\.tsx\\'" . typescript-mode))
)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-mode . lsp)
		 ;; (terraform-mode . lsp)
		 (go-mode . lsp-deferred)
		 (go-mode-hook . lsp-deferred)
		 ;; (lsp-mode . lsp-enable-which-key-integration)
		 (before-save . lsp-organize-imports)
		 ;; (before-save . lsp-format-buffer)
	 )
  :config (setq lsp-go-use-gofumpt t)
  :commands (lsp lsp-deferred))

;; (use-package lsp-ui
;;   :ensure  t
;;   :commands lsp-ui-mode)

(provide 'init-languages)
;;; init-languages.el ends here
