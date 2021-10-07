;;; init-buffers.el --- Buffer misc
;;; Commentary:

;;; Code:

(defun new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

(global-set-key (kbd "M-n") 'new-empty-buffer)


(defun revert-all-buffers ()
  "Refresh all open buffers from their respective files."
  (interactive)
  (message "Begin reverting all buffers...")
  (let* (
	 ;; pairs of (filename, buf)
	 (ls (mapcar (lambda (buf)
		       (list (buffer-file-name buf) buf))
		     (seq-filter 'buffer-file-name (buffer-list))))
	 (count (length ls))
	 (index 1))
    (while ls (cl-destructuring-bind (filename buf)
		  (pop ls)
		;; Revert only buffers containing files, which are not modified;
		;; do not try to revert non-file buffers like *Messages*.
		(message "Reverting [%3d of %3d] %4d%%: %S" index count (round (* 100 (/ (float
											  index)
											 count)))
			 filename)
		(if (file-exists-p filename)
		    ;; If the file exists, revert the buffer.
		    (with-demoted-errors "Error: %S" (with-current-buffer buf (revert-buffer
									       :ignore-auto
									       :noconfirm)))
		  ;; If the file doesn't exist, kill the buffer.
		  (let (kill-buffer-query-functions) ; No query done when killing buffer.
		    (kill-buffer buf)
		    (message "Killed non-existing file buffer: %s" buf)))
		(setq index (1+ index))))
    (message "Finished reverting %d file buffer(s)." count)))

(global-set-key (kbd "C-c r") 'revert-all-buffers)

(provide 'init-buffers)
;;; init-buffers.el ends here
