;;; init-osx-keys.el --- Setup themes
;;; Commentary:

;;; Code:

(with-system darwin
  ;; Translate escape sequences for mac OS
  (define-key input-decode-map "\e[1;10A" [M-S-up])
  (define-key input-decode-map "\e[1;10B" [M-S-down])
  (define-key input-decode-map "\e[1;10C" [M-S-right])
  (define-key input-decode-map "\e[1;10D" [M-S-left])
  (define-key input-decode-map "\e[1;9A" [M-up])
  (define-key input-decode-map "\e[1;9B" [M-down])
  (define-key input-decode-map "\e[1;9C" [M-right])
  (define-key input-decode-map "\e[1;9D" [M-left])
  (define-key input-decode-map "\e[27;5;59~" (kbd "C-;")))

(provide 'init-osx-keys)
;;; init-osx-keys.el ends here
