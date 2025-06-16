;; use-package を使用している場合
(use-package tree-sitter
  :straight t
  :config
  (require 'tree-sitter-hl)) ; ハイライトのために必要

;; tree-sitter が有効になるように設定
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
