;; Enhanced TypeScript/React development environment
(use-package typescript-mode
  :straight t
  :hook ((typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure))
  :custom
  (typescript-indent-level 2)
  :config
  ;; Configure tsserver for TypeScript
  (add-to-list 'eglot-server-programs 
               '((typescript-mode typescript-ts-mode) . ("typescript-language-server" "--stdio")))
  
  ;; TypeScript compiler options via eglot
  (setq-default eglot-workspace-configuration
                (append eglot-workspace-configuration
                        '((:typescript . 
                           (:preferences 
                            (:includeCompletionsForModuleExports t
                             :includeCompletionsWithInsertText t
                             :enableCodeActions t)))))))

;; React/JSX support  
(use-package rjsx-mode
  :straight t
  :hook (rjsx-mode . eglot-ensure)
  :mode (("\\.jsx?$" . rjsx-mode)
         ("\\.tsx$" . rjsx-mode))
  :config
  ;; JSX development configuration
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js-indent-level 2
        sgml-basic-offset 2))

;; Node.js module path integration
(use-package add-node-modules-path
  :straight t
  :hook ((js-mode javascript-mode typescript-mode rjsx-mode) . add-node-modules-path))

;; pnpm/npm integration for monorepos
(use-package npm-mode
  :straight t
  :hook ((js-mode javascript-mode typescript-mode rjsx-mode) . npm-mode)
  :config
  ;; pnpm workspace support
  (defun npm-mode-command-pnpm (cmd)
    "Run pnpm command in workspace context"
    (interactive "sCommand: ")
    (let ((default-directory (projectile-project-root)))
      (compile (concat "pnpm " cmd))))
  
  (define-key npm-mode-keymap (kbd "C-c C-n p") 'npm-mode-command-pnpm))

;; Prettier integration for formatting
(use-package prettier-js
  :straight t
  :hook ((typescript-mode rjsx-mode css-mode scss-mode json-mode) . prettier-js-mode)
  :custom
  (prettier-js-args '("--trailing-comma" "all"
                      "--bracket-spacing" "true"
                      "--single-quote" "true"
                      "--semi" "false"))
  :config
  ;; Auto-install prettier if not found  
  (defun setup-prettier ()
    (when (and (not (executable-find "prettier"))
               (projectile-project-root))
      (let ((default-directory (projectile-project-root)))
        (when (file-exists-p "package.json")
          (shell-command "pnpm add -D prettier")))))
  
  (add-hook 'after-init-hook #'setup-prettier))

;; Enhanced JSON support for package.json, tsconfig.json, etc.
(use-package json-mode
  :straight t
  :hook (json-mode . eglot-ensure)
  :mode (("\\.json$" . json-mode)
         ("\\.jsonc$" . json-mode))
  :custom
  (json-reformat:indent-width 2))

;; Vue.js support (keeping existing)
(use-package vue-mode
  :straight t
  :hook (vue-mode . eglot-ensure))

;; Web mode for mixed HTML/JS/CSS files
(use-package web-mode
  :straight t
  :mode (("\\.html$" . web-mode)
         ("\\.jsx$" . web-mode)
         ("\\.tsx$" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t))
