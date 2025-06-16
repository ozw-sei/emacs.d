;; Modern Python development with pylsp and uv integration
(use-package python-mode
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config
  ;; Configure pylsp for comprehensive Python support
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp")))
  
  ;; pylsp configuration for enhanced development
  (setq-default eglot-workspace-configuration
                (append eglot-workspace-configuration
                        '((:pylsp . 
                           (:configurationSources ["flake8"]
                            :plugins 
                            (:pycodestyle (:enabled nil)
                             :mccabe (:enabled nil)
                             :flake8 (:enabled t)
                             :black (:enabled t)
                             :isort (:enabled t)
                             :rope_completion (:enabled t)
                             :rope_autoimport (:enabled t)
                             :pylsp_mypy (:enabled t :live_mode nil)))))))
  
  ;; UV runtime detection and integration  
  (defun python-detect-uv-project ()
    "Detect if current project uses uv and configure accordingly"
    (when-let ((project-root (projectile-project-root)))
      (or (file-exists-p (expand-file-name "pyproject.toml" project-root))
          (file-exists-p (expand-file-name "uv.lock" project-root)))))
  
  (defun python-setup-uv-environment ()
    "Setup Python environment with uv if available"
    (when (and (python-detect-uv-project)
               (executable-find "uv"))
      (let* ((project-root (projectile-project-root))
             (venv-path (expand-file-name ".venv" project-root))
             (python-executable (expand-file-name "bin/python" venv-path)))
        (when (file-exists-p python-executable)
          (setq-local python-shell-interpreter python-executable)
          (setq-local eglot-python-executable python-executable)))))
  
  (add-hook 'python-mode-hook #'python-setup-uv-environment)
  
  ;; UV task runner integration
  (defun python-uv-run-task (task)
    "Run uv task in current project"
    (interactive (list (read-string "UV task: " "test")))
    (if (python-detect-uv-project)
        (let ((default-directory (projectile-project-root)))
          (compile (format "uv run %s" task)))
      (error "Not in a uv project")))
  
  (defun python-uv-sync ()
    "Sync uv dependencies"
    (interactive)
    (when (python-detect-uv-project)
      (let ((default-directory (projectile-project-root)))
        (compile "uv sync"))))
  
  ;; Key bindings for uv integration
  (define-key python-mode-map (kbd "C-c C-u t") 'python-uv-run-task)
  (define-key python-mode-map (kbd "C-c C-u s") 'python-uv-sync)
  
  ;; Unset conflicting keybinding
  (local-unset-key (kbd "C-c C-p")))

;; Poetry integration (fallback for non-uv projects)
(use-package poetry
  :straight t
  :hook (python-mode . poetry-tracking-mode)
  :config
  (defun poetry-activate-if-available ()
    "Activate poetry environment if not using uv"
    (unless (python-detect-uv-project)
      (when (poetry-find-project-root)
        (poetry-venv-activate))))
  
  (add-hook 'python-mode-hook #'poetry-activate-if-available))

;; Enhanced testing support
(use-package python-pytest
  :straight t
  :hook (python-mode . python-pytest-mode)
  :custom
  (python-pytest-executable "python -m pytest")
  :config
  ;; UV-aware pytest execution
  (defun python-pytest-run-uv (args)
    "Run pytest through uv if available"
    (if (python-detect-uv-project)
        (python-pytest-run (concat "uv run " args))
      (python-pytest-run args)))
  
  (define-key python-pytest-mode-map (kbd "C-c C-t u") 'python-pytest-run-uv))

;; Black formatting integration (handled by pylsp)
(use-package blacken
  :straight t
  :hook (python-mode . blacken-mode)
  :custom
  (blacken-fast-unsafe t)
  (blacken-line-length 88))
