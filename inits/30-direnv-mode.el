;; direnv-allow が便利
(use-package direnv
 :straight t
 :config
 (add-to-list 'warning-suppress-types '(direnv))
 (direnv-mode))
