(if window-system  (progn
    (setq default-frame-alist
          (append (list
                   '(font . "Consolas"))
                  default-frame-alist))

  ;; font-size　ASCII
  (set-face-attribute 'default nil :height 120)


                                        ; japanese-jisx0208 fonts
  (set-fontset-font nil
                    'japanese-jisx0208
                    (font-spec :family "Noto Sans Mono CJK JP"))
  )
)
