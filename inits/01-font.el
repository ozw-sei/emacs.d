;; font-size　ASCII
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 120
                    :weight 'normal
                    :width 'normal)


; 半角ｶﾅ設定
; japanese-jisx0208 fonts
(set-fontset-font nil
                  'japanese-jisx0208
                  (font-spec :family "Noto Sans Mono CJK JP Regular"))
