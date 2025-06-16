;;(use-package tree-sitter-langs
;;  :config
;;  (setq tree-sitter-major-mode-language-alist
;;        (append tree-sitter-major-mode-language-alist
;;                '((typescript-mode . tsx)
;;                  (tsx-mode . tsx)))))
;;
;; TypeScript / TSX モードで tree-sitter を有効にする
;;(add-hook 'typescript-mode-hook #'tree-sitter-mode)
;;(add-hook 'tsx-mode-hook #'tree-sitter-mode)

(use-package tsx-ts-mode
  :mode (("\\.ts[x]?\\'" . tsx-ts-mode)
         ("\\.[m]ts\\'" . tsx-ts-mode)
         ("\\.js[x]?\\'" . tsx-ts-mode)
         ("\\.[mc]js\\'" . tsx-ts-mode)))


;; Pythonの設定
(use-package python-ts-mode
  :mode ("\\.py$" . python-ts-mode))
