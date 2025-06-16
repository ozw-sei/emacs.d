

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

;; Removed company-go as we're no longer using company-mode
;; Eglot provides completion via its built-in LSP support

(use-package direx
  :straight t
  :config
  ;; Ensure direx is fully loaded before go-direx uses it
  (require 'direx))

(use-package go-direx
  :straight t
  :after (go-mode direx)
  :config
  ;; Ensure direx symbols are available
  (require 'direx)
  (require 'go-direx))
