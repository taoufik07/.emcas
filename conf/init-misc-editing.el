;;; init-misc-editing.el --- Setup frames
;;; Commentary:

;;; Code:

(delete-selection-mode t)
(global-auto-revert-mode t)

;;; show matching parenthesis and set delay to 0.
(show-paren-mode t)
(setq show-paren-delay 0)

;; Prevent opening new windows
(setq split-height-threshold nil split-width-threshold nil)

(defun move-text-internal (arg)
  (cond ((and
	  mark-active
	  transient-mark-mode)
	 (if (> (point)
		(mark))
	     (exchange-point-and-mark))
	 (let ((column (current-column))
	       (text (delete-and-extract-region (point)
						(mark))))
	   (forward-line arg)
	   (move-to-column column t)
	   (set-mark (point))
	   (insert text)
	   (exchange-point-and-mark)
	   (setq deactivate-mark nil)))
	(t (beginning-of-line)
	   (when (or (> arg 0)
		     (not (bobp)))
	     (forward-line)
	     (when (or (< arg 0)
		       (not (eobp)))
	       (transpose-lines arg))
	     (forward-line -1)))))


(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "p")
  (move-text-internal (- arg)))

(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
	(save-excursion (indent-rigidly (region-beginning)
					(region-end) distance)
			(push-mark mark t t)
			(setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
		    (line-end-position) distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text 4)
  ;; (forward-char 4)
  )

(defun shift-left (count)
  (interactive "p")
  ;; (backward-char 4)
  (shift-text (- 4)))

(global-set-key (kbd "<M-right>") 'shift-right)
(global-set-key (kbd "<M-left>") 'shift-left)

;;;
(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-<RET>") 'insert-line-below)

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "M-<RET>") 'insert-line-above)

(provide 'init-misc-editing)
;;; init-misc-editing.el ends here
