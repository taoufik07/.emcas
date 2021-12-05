;;; init-misc-packages.el --- Setup packages
;;; Commentary:

;;; Code:

(use-package
  try
  :ensure t)

(use-package
  magit
  :ensure t
  :bind ("C-x g" . 'magit-status))

(use-package
  projectile
  :ensure t
  :bind (("M-p" . 'projectile-command-map)))

;;; Use async whenever it's possible
(use-package
  async
  :ensure t
  :init (dired-async-mode 1))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(use-package
  multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-." . mc/mark-next-like-this)

	 ("C-<" . mc/mark-previous-like-this)
	 ("C-," . mc/mark-previous-like-this)

	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package
  iedit
  :ensure t
  :bind ("C-c ;" . 'iedit-mode))

(provide 'init-misc-packages)
;;; init-misc-packages.el ends here
