;;; init-frame.el --- Setup frames
;;; Commentary:

;;; Code:

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

(provide 'init-frame)
;;; init-frame.el ends here
