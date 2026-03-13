;; Modern Go development with gopls LSP
(use-package go-mode
  :straight t
  :after lsp-mode
  :hook ((go-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred))
  :config
  ;; Go environment setup
  (when (getenv "GOROOT")
    (add-to-list 'exec-path (concat (getenv "GOROOT") "/bin")))
  (when (getenv "GOPATH")
    (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin")))

  ;; gopls settings via lsp-mode
  (with-eval-after-load 'lsp-mode
    (setq lsp-go-use-placeholders t)
    (setq lsp-go-analyses '((unusedparams . t)
                            (shadow . t)))
    (setq lsp-go-import-shortcut "Both")
    (setq lsp-go-use-gofumpt t))

  ;; Format on save with goimports-reviser + gofumpt chain
  (defun go-format-buffer-chain ()
    "Format Go buffer with goimports-reviser then gofumpt"
    (when (derived-mode-p 'go-mode 'go-ts-mode)
      (let ((original-point (point)))
        ;; First pass: goimports-reviser for import organization
        (when (executable-find "goimports-reviser")
          (shell-command-on-region
           (point-min) (point-max)
           "goimports-reviser -stdin"
           t t))
        ;; Second pass: gofumpt for consistent formatting
        (when (executable-find "gofumpt")
          (shell-command-on-region
           (point-min) (point-max)
           "gofumpt"
           t t))
        (goto-char original-point))))

  ;; Replace gofmt with modern formatting chain
  (remove-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'before-save-hook 'go-format-buffer-chain)

  ;; Optional: go-eldoc for documentation
  (use-package go-eldoc
    :straight t
    :hook (go-mode . go-eldoc-setup)))
