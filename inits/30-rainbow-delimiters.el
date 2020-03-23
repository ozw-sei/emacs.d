(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package auto-yasnippet
  :straight t
  :config
  (setq aya-create-with-newline t))
