;; use-package を使用している場合
(use-package treesit
  :config
  (setq treesit-font-lock-level 4))



(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))

;; tree-sitter が有効になるように設定
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
