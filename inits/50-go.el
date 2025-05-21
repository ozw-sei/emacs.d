

(use-package go-mode
  :straight t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)

  :config
  (setq exec-path (parse-colon-path (getenv "GOROOT")))
  (setq exec-path (parse-colon-path (getenv "GOPATH")))
  (setq exec-path (parse-colon-path (getenv "PATH")))
  (setq eshell-path-env (getenv "GOPATH"))
  (setq eshell-path-env (getenv "PATH"))
  (setq eshell-path-env (getenv "GOROOT"))

  :bind
  ("M-." . 'godef-jump)
  ("M-," . 'pop-tag-mark)
  )

(use-package company-go
  :straight t
  :config
  (custom-set-variables
  '(company-go-insert-arguments nil))
  :after (go-mode company)
  )

(use-package direx
  :straight t
  :ensure t)

(use-package go-direx
  :straight t
  :after (go-mode direx))
