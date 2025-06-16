;; GraphQL development support with comprehensive tooling
(use-package graphql-mode
  :straight t
  :mode (("\\.graphql\\'" . graphql-mode)
         ("\\.gql\\'" . graphql-mode))
  :hook ((graphql-mode . eglot-ensure))
  :config
  ;; Configure GraphQL Language Service
  (add-to-list 'eglot-server-programs
               '(graphql-mode . ("graphql-lsp" "server" "-m" "stream")))

  ;; GraphQL indentation and formatting
  (setq graphql-indent-level 2)

  ;; GraphQL schema validation and introspection
  (defun graphql-validate-schema ()
    "Validate GraphQL schema using graphql-cli"
    (interactive)
    (when (executable-find "graphql")
      (let ((default-directory (projectile-project-root)))
        (compile "graphql validate"))))

  (defun graphql-introspect-schema (endpoint)
    "Introspect GraphQL schema from endpoint"
    (interactive "sGraphQL endpoint: ")
    (when (executable-find "graphql")
      (let ((default-directory (projectile-project-root)))
        (compile (format "graphql get-schema --endpoint %s" endpoint)))))

  (defun graphql-codegen ()
    "Generate TypeScript types from GraphQL schema"
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (cond
       ((file-exists-p "codegen.yml")
        (compile "graphql-codegen"))
       ((file-exists-p "codegen.ts")
        (compile "graphql-codegen --config codegen.ts"))
       ((executable-find "gql.tada")
        (compile "gql.tada generate"))
       (t (error "No GraphQL codegen configuration found")))))

  ;; Key bindings for GraphQL operations
  (define-key graphql-mode-map (kbd "C-c C-g v") 'graphql-validate-schema)
  (define-key graphql-mode-map (kbd "C-c C-g i") 'graphql-introspect-schema)
  (define-key graphql-mode-map (kbd "C-c C-g c") 'graphql-codegen))

;; GraphQL-specific project integration
(defun graphql-find-schema-files ()
  "Find all GraphQL schema files in project"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (consult-find "\\.graphql$\\|\\.gql$")))

(defun graphql-find-queries ()
  "Find GraphQL query/mutation files"
  (interactive)
  (consult-grep "query\\|mutation\\|subscription" "\\.graphql$\\|\\.gql$\\|\\.ts$\\|\\.tsx$"))

(defun graphql-find-resolvers ()
  "Find GraphQL resolver implementations"
  (interactive)
  (consult-grep "resolver" "\\.ts$\\|\\.js$\\|\\.go$\\|\\.py$"))

;; Apollo Client integration
(defun graphql-apollo-client-cache-types ()
  "Generate Apollo Client cache types"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (when (file-exists-p "apollo.config.js")
      (compile "apollo client:codegen"))))

;; gql.tada integration for type-safe queries
(defun graphql-gqltada-check ()
  "Check gql.tada type safety"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (when (executable-find "gql.tada")
      (compile "gql.tada turbo"))))

;; GraphQL Federation support
(defun graphql-federation-compose ()
  "Compose federated GraphQL schema"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (when (executable-find "rover")
      (compile "rover supergraph compose --config supergraph.yaml"))))

;; GraphQL playground integration
(defun graphql-open-playground ()
  "Open GraphQL playground for current endpoint"
  (interactive)
  (let ((endpoint (read-string "GraphQL endpoint: " "http://localhost:4000/graphql")))
    (browse-url (concat "https://studio.apollographql.com/sandbox/explorer?endpoint=" endpoint))))

;; Schema-driven workflow automation
(defun graphql-watch-schema-changes ()
  "Watch for schema changes and trigger codegen"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (when (executable-find "graphql-codegen")
      (compile "graphql-codegen --watch"))))

;; Enhanced GraphQL development utilities
(defun graphql-format-query ()
  "Format GraphQL query using prettier"
  (interactive)
  (when (executable-find "prettier")
    (shell-command-on-region
     (point-min) (point-max)
     "prettier --parser graphql"
     t t)))

(defun graphql-extract-fragment ()
  "Extract selection into a reusable fragment"
  (interactive)
  (if (use-region-p)
      (let ((selection (buffer-substring-no-properties (region-beginning) (region-end)))
            (fragment-name (read-string "Fragment name: ")))
        (delete-region (region-beginning) (region-end))
        (insert (format "...%s" fragment-name))
        (save-excursion
          (goto-char (point-min))
          (insert (format "fragment %s on %s {\n%s\n}\n\n"
                         fragment-name
                         (read-string "Type: ")
                         selection))))
    (error "No region selected")))

;; GraphQL query performance analysis
(defun graphql-analyze-query-complexity ()
  "Analyze GraphQL query complexity"
  (interactive)
  (when (executable-find "graphql")
    (let ((query-file (buffer-file-name)))
      (when query-file
        (compile (format "graphql query-complexity %s" query-file))))))

;; Integration with existing Emacs GraphQL ecosystem

;; GraphQL request execution
(use-package graphql-doc
  :straight t
  :hook (graphql-mode . graphql-doc-mode))

;; Auto-completion for GraphQL schemas
(defun graphql-setup-completion ()
  "Setup GraphQL-specific completion"
  (when (derived-mode-p 'graphql-mode)
    (setq-local completion-at-point-functions
                (append completion-at-point-functions
                        '(graphql-completion-at-point)))))

(add-hook 'graphql-mode-hook #'graphql-setup-completion)

;; Global key bindings for GraphQL utilities
(with-eval-after-load 'graphql-mode
  (define-key graphql-mode-map (kbd "C-c C-s f") 'graphql-find-schema-files)
  (define-key graphql-mode-map (kbd "C-c C-s q") 'graphql-find-queries)
  (define-key graphql-mode-map (kbd "C-c C-s r") 'graphql-find-resolvers)
  (define-key graphql-mode-map (kbd "C-c C-a c") 'graphql-apollo-client-cache-types)
  (define-key graphql-mode-map (kbd "C-c C-t c") 'graphql-gqltada-check)
  (define-key graphql-mode-map (kbd "C-c C-f c") 'graphql-federation-compose)
  (define-key graphql-mode-map (kbd "C-c C-p p") 'graphql-open-playground)
  (define-key graphql-mode-map (kbd "C-c C-w w") 'graphql-watch-schema-changes)
  (define-key graphql-mode-map (kbd "C-c C-r f") 'graphql-format-query)
  (define-key graphql-mode-map (kbd "C-c C-r e") 'graphql-extract-fragment)
  (define-key graphql-mode-map (kbd "C-c C-q a") 'graphql-analyze-query-complexity))

;; File associations for GraphQL-related files
(add-to-list 'auto-mode-alist '("\\.*gql\\.d\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("graphql-env\\.d\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("graphql-cache\\.d\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("codegen\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("apollo\\.config\\.js\\'" . js-mode))
