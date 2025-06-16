;; use-package を使用している場合
(use-package tree-sitter
  :config


  (require 'tree-sitter-hl)) ; ハイライトのために必要



(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))

;; tree-sitter が有効になるように設定
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
