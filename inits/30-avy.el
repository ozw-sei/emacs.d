
(use-package avy
  :config
  (defhydra hydra-avy (:exit t :hint nil)
    "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char
 [_m_] move   [_M_] move      [_w_] word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("c" avy-goto-char)
    ("w" avy-goto-word-1)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region))

  :bind
  ("C-]" . hydra-avy/body)
  ("C-l" . avy-goto-line)
  :straight t)
