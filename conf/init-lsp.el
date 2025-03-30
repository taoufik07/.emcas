;;; init-lsp.el --- Setup lsp
;;; Commentary:

;;; Code:

(use-package lsp-eslint
  :demand t
  :after lsp-mode
  :custom
  (lsp-eslint-enable 0)
  (lsp-eslint-auto-fix-on-save t))

(use-package
  lsp-treemacs
  :ensure t
  :after (lsp-mode)
  :init
  (setq lsp-treemacs-sync-mode 1))

;;; Copied from https://github.com/ovistoica/emacs/blob/main/init.el
;;; I like how he listed all config
(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         (before-save . lsp-organize-imports)
	 (before-save . lsp-format-buffer)
         ((go-ts-mode
           tsx-ts-mode
           jtsx-tsx-mode
           jtsx-typescript-mode
           typescript-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-treemacs-sync-mode 1)
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  ;; (lsp-enable-file-watchers nil)
  (lsp-enable-indentation t)
  ;; (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job
  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  )

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
	      ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode)
  :config (setq lsp-ui-doc-enable t))

(provide 'init-lsp)
;;; init-lsp.el ends here
