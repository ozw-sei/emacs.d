
(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                          :color pink
                          :post (deactivate-mark))
  ""
  ("h" backward-char "left")
  ("l" forward-char "right")
  ("k" previous-line "up")
  ("j" next-line "down")
  ("e" exchange-point-and-mark "exchange")
  ("n" copy-rectangle-as-kill "copy-rectangle")
  ("d" delete-rectangle "delete")
  ("r" (if (region-active-p)
          (deactivate-mark)
        (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle "yank")
  ("u" undo "undo")
  ("s" string-rectangle "string-rectangle")
  ("p" kill-rectangle "kill-rectangle")
  ("q" nil "exit")
  )

(bind-key "C-x SPC" 'hydra-rectangle/body)
