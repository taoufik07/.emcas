;;; init-misc-packages.el --- Setup packages
;;; Commentary:

;;; Code:

(use-package
  try
  :ensure t)

(use-package
  magit
  :ensure t
  :bind (("C-x g" . 'magit-status)
	 ("M-<return>" . 'magit-diff-visit-file-other-window)))

(use-package
  projectile
  :ensure t
  :init
  (projectile-mode)
  (remove-hook 'buffer-list-update-hook #'projectile-track-known-projects-find-file-hook)
  :bind ("M-p" . 'projectile-command-map)
  :custom ((setq projectile-enable-caching t)
	   (setq projectile-switch-project-action 'projectile-dired)
	   (setq projectile-completion-system 'default)))

;; (use-package
;;   find-file-in-project
;;   :ensure t
;;   :bind (("M-p f" . 'find-file-in-project)))

;;; Use async whenever it's possible
(use-package
  async
  :ensure t
  :init (dired-async-mode 1))

(use-package
  clipetty
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

	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-c C-," . mc/mark-all-like-this)))

(use-package
  iedit
  :ensure t
  :bind ("C-c ;" . 'iedit-mode))

(use-package
  vertico
  :ensure t
  :init
  (vertico-mode))

(use-package
  marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(provide 'init-misc-packages)
;;; init-misc-packages.el ends here
