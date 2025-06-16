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

;; TypeScript and TSX files
(use-package tsx-ts-mode
  :mode (("\\.tsx?\\'" . tsx-ts-mode)
         ("\\.[m]ts\\'" . tsx-ts-mode)))

;; JavaScript files
(use-package js-ts-mode
  :mode (("\\.js[x]?\\'" . js-ts-mode)
         ("\\.[mc]js\\'" . js-ts-mode)))


;; Pythonの設定
(use-package python-ts-mode
  :mode ("\\.py$" . python-ts-mode))
