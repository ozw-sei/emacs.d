(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  :init
  (add-hook 'python-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c C-p"))))

  )
