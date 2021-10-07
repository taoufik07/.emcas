;;; init-packages.el --- Setup packages
;;; Commentary:

;;; Code:

;;; Setup package.el
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(unless package--initialized (package-initialize))

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'init-packages)
;;; init-packages.el ends here
