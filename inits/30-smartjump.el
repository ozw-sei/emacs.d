(use-package smart-jump
  :straight t
  :config
  ;; Suppress ad-handle-definition warnings
  (setq ad-redefinition-action 'accept)
  (smart-jump-setup-default-registers)
  :bind
  ("C-." . smart-jump-go)
  ("C-," . smart-jump-back)
  ("C->" . smart-jump-peek))
