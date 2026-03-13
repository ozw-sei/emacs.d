;; TOML support
(use-package toml-mode
  :straight t
  :mode (("\\Pipfile$" . toml-mode)))
;; lsp-mode auto-detects taplo
;; Install: npm i -g @taplo/cli
