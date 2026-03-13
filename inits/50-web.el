;; Web-mode for HTML template files only
;; TypeScript/JSX/TSX are handled by tree-sitter modes + lsp-mode
(use-package web-mode
  :straight t
  :mode (("\\.html\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.jsp\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))
