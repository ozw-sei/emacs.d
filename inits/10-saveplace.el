(use-package saveplace
  :straight t
  :config
  (save-place-mode 1))

;;; ファイルを開いた位置を保存する
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
