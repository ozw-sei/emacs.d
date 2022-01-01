;; hydra window 操作
(defhydra hydra-buffer-split nil
  "hydra-buffer-split"
  ("s" (lambda ()
        (interactive)
        (split-window-vertically)
        (windmove-down)) "split-horizontally")

  ("v" (lambda()
        (interactive)
        (split-window-horizontally)
        (balance-windows)
        (windmove-right)) "split-vertically")
  ("C-k" delete-window "delete")
  ("h" windmove-left "move-left")
  ("l" windmove-right "move-right")
  ("k" windmove-up "move-up")
  ("j" windmove-down "move-down")
  ("u" winner-undo "winner-undo")
  ("r" winner-redo "winner-redo")
  )
