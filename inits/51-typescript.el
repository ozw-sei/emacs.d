(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2))

(use-package tsx-mode
  :ensure t
  :mode ("\\.tsx\\'" . tsx-mode)
  :hook (tsx-mode . eglot-ensure) ; TSXファイルでeglotを有効化
  :config
  ;; 必要に応じて追加設定 (例: インデント)
  (setq tsx-indent-level 2))
