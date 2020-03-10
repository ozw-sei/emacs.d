
;; 単語にカーソルを置くと同じ単語をハイライトしてくれる
(use-package highlight-symbol
  :straight t
  :diminish (highlight-symbol-mode)
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 0.7)
  (setq indent-guide-delay 0.5))
