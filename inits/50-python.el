;; Use Eglot with pyright for Python
(add-hook 'python-mode-hook 'eglot-ensure)

;; Configure pyright with pipenv support
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

;; Unset conflicting keybinding
(add-hook 'python-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c C-p"))))
