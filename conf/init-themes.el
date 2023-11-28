;;; init-themes.el --- Setup themes
;;; Commentary:

;;; Code:

(display-battery-mode)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-conservatively 100)
(setq tab-width 4)

(setq inhibit-startup-message t initial-scratch-message ";; Happy Hacking")

;;; TODO: Auto checkout to this commit iterm on macos https://raw.githubusercontent.com/dracula/emacs/3e6bbd3618316aa8e2b354e8ad14008589f6fdb9/dracula-theme.el
(use-package
  dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

(provide 'init-themes)
;;; init-themes.el ends here
