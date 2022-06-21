(use-package lsp-pyright
  :straight t
  :init
  (defun lsp-pyright-setup-when-pipenv ()
    (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
    (lsp-restart-workspace))
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(add-hook 'python-mode-hook
  :init
  (add-hook 'python-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c C-p"))))

  )
