;; シェルの環境変数をEmacsに引き継ぐ
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize)
  ;; Go tools のパスを確実に追加
  (let ((gopath (or (getenv "GOPATH") (expand-file-name "~/go"))))
    (add-to-list 'exec-path (concat gopath "/bin"))
    (setenv "PATH" (concat gopath "/bin:" (getenv "PATH")))))
