;;; init-copy-paste-term.el --- Setup themes
;;; Commentary:

;;; Code:

(defconst copy-system-cmd "xclip")
(with-system darwin
  (setq copy-system-cmd "pbcopy"))

(defun x-copy (beg end &optional region)
  "Copy to system clipboard."
  (interactive "r")
  (call-process-region beg end copy-system-cmd nil nil nil "-selection" "c")
  (kill-ring-save beg end region))

(global-set-key (kbd "M-w") 'x-copy)

(defun x-cut (beg end &optional region)
  "Cut to system clipboard."
  (interactive "r")
  (call-process-region beg end copy-system-cmd nil nil nil "-selection" "c")
  ;; (kill-region beg end))
  (delete-region beg end))

(global-set-key (kbd "C-w") 'x-cut)

(provide 'init-copy-paste-term)
;;; init-copy-paste-term.el ends here
