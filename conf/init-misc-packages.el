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
  :bind (("M-p" . 'projectile-command-map)
		 ("M-<return>" . 'magit-diff-visit-file-other-window))
  :init
  (projectile-global-mode)
  :custom ((setq projectile-enable-caching t)))

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
  (vertico-mode)
  :custom
  (setq vertico-cycle t))

(use-package
  marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil)))


;; (use-package bufler
;;   :ensure t
;;   :bind (("C-x b" . bufler-switch-buffer))
;;   :custom
;;   (setf bufler-groups
;;         (bufler-defgroups
;;           ;; Subgroup collecting all named workspaces.
;;           (group (auto-workspace))
;;           ;; Subgroup collecting buffers in a projectile project.
;;           (group (auto-projectile))
;;           ;; Grouping browser windows
;;           (group
;;            (group-or "Browsers"
;;                      (name-match "Vimb" (rx bos "vimb"))
;;                      (name-match "Qutebrowser" (rx bos "Qutebrowser"))
;;                      (name-match "Chromium" (rx bos "Chromium"))))
;;           (group
;;            (group-or "Chat"
;;                      (mode-match "Telega" (rx bos "telega-"))))
;;           (group
;;            ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
;;            (group-or "Help/Info"
;;                      (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
;;                      ;; (mode-match "*Helpful*" (rx bos "helpful-"))
;;                      (mode-match "*Info*" (rx bos "info-"))))
;;           (group
;;            ;; Subgroup collecting all special buffers (i.e. ones that are not
;;            ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
;;            ;; through to other groups, so they end up grouped with their project buffers).
;;            (group-and "*Special*"
;;                       (name-match "**Special**"
;;                                   (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
;;                       (lambda (buffer)
;;                         (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
;;                                              buffer)
;;                                     (funcall (mode-match "Dired" (rx bos "dired"))
;;                                              buffer)
;;                                     (funcall (auto-file) buffer))
;;                           "*Special*"))))
;;           ;; Group remaining buffers by major mode.
;;           (auto-mode))))

(provide 'init-misc-packages)
;;; init-misc-packages.el ends here
