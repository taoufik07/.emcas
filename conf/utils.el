;;; utils.el --- Defines utils func
;;; Commentary:

;;; Code:

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type) ,@body))

(provide 'utils)
;;; utils.el ends here
