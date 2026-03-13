;; YAML support - yaml-ts-mode via treesit-auto, yaml-mode as fallback
(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))
;; lsp-mode auto-detects yaml-language-server
;; Install: npm i -g yaml-language-server
