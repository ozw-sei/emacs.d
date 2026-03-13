;; Tree-sitter configuration with treesit-auto
(use-package treesit
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :straight t
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
