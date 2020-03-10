(use-package goto-line-preview
  :straight t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

(bind-key "M-g" 'goto-line)
