(use-package undohist
  :straight t
  :config
  (setq undohist-ignored-files
      '("/tmp/" "COMMIT_EDITMSG"))
  (undohist-initialize)
  )
;; undo-treeモードの設定

(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  :diminish undo-tree-mode
  :bind
  ("C-c u" . 'undo-tree-visualize))
