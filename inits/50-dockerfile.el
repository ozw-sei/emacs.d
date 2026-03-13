;; Dockerfile support
(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))
;; lsp-mode auto-detects docker-langserver
;; Install: npm i -g dockerfile-language-server-nodejs
