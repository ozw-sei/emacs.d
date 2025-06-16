;; Protocol Buffers development support with buf integration
(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode)
  :hook ((protobuf-mode . eglot-ensure))
  :config
  ;; Configure buf-language-server for protobuf
  (add-to-list 'eglot-server-programs
               '(protobuf-mode . ("buf" "beta" "lsp")))
  
  ;; Protobuf indentation and style
  (setq protobuf-indent-level 2)
  
  ;; Buf integration functions
  (defun protobuf-buf-format ()
    "Format protobuf files using buf"
    (interactive)
    (when (executable-find "buf")
      (shell-command-on-region
       (point-min) (point-max)
       "buf format --write -"
       t t)))
  
  (defun protobuf-buf-lint ()
    "Lint protobuf files using buf"
    (interactive)
    (when (executable-find "buf")
      (let ((default-directory (projectile-project-root)))
        (compile "buf lint"))))
  
  (defun protobuf-buf-generate ()
    "Generate code from protobuf files using buf"
    (interactive)
    (when (executable-find "buf")
      (let ((default-directory (projectile-project-root)))
        (compile "buf generate"))))
  
  (defun protobuf-buf-breaking ()
    "Check for breaking changes in protobuf"
    (interactive)
    (when (executable-find "buf")
      (let ((default-directory (projectile-project-root)))
        (compile "buf breaking --against '.git#branch=main'"))))
  
  ;; Auto-format on save
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'protobuf-buf-format nil t)))
  
  ;; Key bindings for buf operations
  (define-key protobuf-mode-map (kbd "C-c C-p f") 'protobuf-buf-format)
  (define-key protobuf-mode-map (kbd "C-c C-p l") 'protobuf-buf-lint)
  (define-key protobuf-mode-map (kbd "C-c C-p g") 'protobuf-buf-generate)
  (define-key protobuf-mode-map (kbd "C-c C-p b") 'protobuf-buf-breaking))

;; Buf configuration file support
(use-package yaml-mode
  :straight t
  :mode (("buf\\.yaml\\'" . yaml-mode)
         ("buf\\.lock\\'" . yaml-mode))
  :config
  ;; Special handling for buf.yaml
  (defun buf-yaml-setup ()
    "Setup buf.yaml specific configuration"
    (when (string-match-p "buf\\.yaml" (buffer-file-name))
      (setq-local yaml-indent-offset 2)
      (flycheck-mode 1)))
  
  (add-hook 'yaml-mode-hook #'buf-yaml-setup))

;; Enhanced protobuf development utilities
(defun protobuf-find-service-definition ()
  "Find and navigate to service definition in protobuf files"
  (interactive)
  (let ((service (thing-at-point 'symbol t)))
    (when service
      (consult-grep (format "service %s" service)))))

(defun protobuf-find-message-definition ()
  "Find and navigate to message definition in protobuf files"
  (interactive)
  (let ((message (thing-at-point 'symbol t)))
    (when message
      (consult-grep (format "message %s" message)))))

;; gRPC service integration
(defun protobuf-grpc-list-services ()
  "List gRPC services from current protobuf file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((services '()))
      (while (re-search-forward "^service \\([A-Za-z0-9_]+\\)" nil t)
        (push (match-string 1) services))
      (if services
          (message "Services: %s" (string-join (reverse services) ", "))
        (message "No services found")))))

;; Protocol buffer compilation pipeline
(defun protobuf-compile-to-go ()
  "Compile protobuf to Go code"
  (interactive)
  (let ((proto-file (buffer-file-name))
        (default-directory (projectile-project-root)))
    (when (and proto-file (executable-find "protoc"))
      (compile (format "protoc --go_out=. --go_opt=paths=source_relative %s"
                      (file-relative-name proto-file))))))

(defun protobuf-compile-to-typescript ()
  "Compile protobuf to TypeScript code"
  (interactive)
  (let ((proto-file (buffer-file-name))
        (default-directory (projectile-project-root)))
    (when (and proto-file (executable-find "protoc"))
      (compile (format "protoc --ts_out=. %s"
                      (file-relative-name proto-file))))))

;; Integration with project-wide protobuf workflow
(defun protobuf-project-generate-all ()
  "Generate all protobuf code in project"
  (interactive)
  (cond
   ((executable-find "buf")
    (let ((default-directory (projectile-project-root)))
      (compile "buf generate")))
   ((executable-find "protoc")
    (let ((default-directory (projectile-project-root)))
      (compile "find . -name '*.proto' -exec protoc --go_out=. --go_opt=paths=source_relative {} \\;")))
   (t (error "Neither buf nor protoc found"))))

;; Add to hydra or keybindings if needed
(with-eval-after-load 'protobuf-mode
  (define-key protobuf-mode-map (kbd "C-c C-g s") 'protobuf-find-service-definition)
  (define-key protobuf-mode-map (kbd "C-c C-g m") 'protobuf-find-message-definition)
  (define-key protobuf-mode-map (kbd "C-c C-g l") 'protobuf-grpc-list-services)
  (define-key protobuf-mode-map (kbd "C-c C-c g") 'protobuf-compile-to-go)
  (define-key protobuf-mode-map (kbd "C-c C-c t") 'protobuf-compile-to-typescript)
  (define-key protobuf-mode-map (kbd "C-c C-c a") 'protobuf-project-generate-all))