(use-package lsp-python-ms
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :init
  (add-hook 'hack-local-variables-hook
	    (lambda ()
	      (when (derived-mode-p 'python-mode)
		(require 'lsp-python-ms)
		(lsp)))))

(add-hook 'python-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c C-p"))))
