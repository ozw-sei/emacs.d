(use-package elixir-mode
  :straight t)

(use-package alchemist
  :straight t)

(use-package flycheck-elixir
  :straight t)

(use-package lsp-mode
  :config
  (add-to-list 'exec-path "~/bin/elixir-ls"))

(setq alchemist-key-command-prefix (kbd "C-c ,"))
