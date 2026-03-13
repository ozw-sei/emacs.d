;; TypeScript/JavaScript development with lsp-mode + tree-sitter modes

;; Indentation settings for JS/TS
(setq js-indent-level 2)
(setq typescript-ts-mode-indent-offset 2)

;; lsp-mode TypeScript settings
(with-eval-after-load 'lsp-mode
  (setq lsp-typescript-suggest-complete-function-calls t)
  (setq lsp-javascript-suggest-complete-function-calls t))

;; Node.js module path integration
(use-package add-node-modules-path
  :straight t
  :hook ((js-ts-mode tsx-ts-mode typescript-ts-mode) . add-node-modules-path))

;; pnpm/npm integration for monorepos
(use-package npm-mode
  :straight t
  :hook ((js-ts-mode tsx-ts-mode typescript-ts-mode) . npm-mode)
  :config
  (defun npm-mode-command-pnpm (cmd)
    "Run pnpm command in workspace context"
    (interactive "sCommand: ")
    (let ((default-directory (projectile-project-root)))
      (compile (concat "pnpm " cmd))))

  (define-key npm-mode-keymap (kbd "C-c C-n p") 'npm-mode-command-pnpm))

;; Prettier integration for formatting
(use-package prettier-js
  :straight t
  :hook ((tsx-ts-mode typescript-ts-mode js-ts-mode css-mode scss-mode json-ts-mode) . prettier-js-mode)
  :custom
  (prettier-js-args '("--trailing-comma" "all"
                      "--bracket-spacing" "true"
                      "--single-quote" "true"
                      "--semi" "false"))
  :config
  (defun setup-prettier ()
    (when (and (not (executable-find "prettier"))
               (projectile-project-root))
      (let ((default-directory (projectile-project-root)))
        (when (file-exists-p "package.json")
          (shell-command "pnpm add -D prettier")))))

  (add-hook 'after-init-hook #'setup-prettier))
