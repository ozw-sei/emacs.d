;; mozc を入力に使用する.いつかSKKに挑戦しよう。
(use-package mozc
  :straight t
  :config
  (set-language-environment "Japanese")           ; 言語環境を"japanese"に
  (setq default-input-method "japanese-mozc")     ; IMEをjapanes-mozcに
  (prefer-coding-system 'utf-8))
