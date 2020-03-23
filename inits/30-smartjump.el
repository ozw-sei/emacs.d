(use-package smart-jump
  :straight t
  :config
  (smart-jump-setup-default-registers)
  :bind
  ("C-." . smart-jump-go)
  ("C-," . smart-jump-back)
  ("C->" . smart-jump-peek))
