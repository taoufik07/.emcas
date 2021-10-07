;;; init-languages.el --- Setup languages
;;; Commentary:

;;; Code:

(use-package org)

(use-package
  flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package
  elpy
  :ensure t
  :init (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  :config (elpy-enable))

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
  rjsx-mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)
	 ("\\.jsx\\'" . rjsx-mode)))

(provide 'init-languages)
;;; init-languages.el ends here
