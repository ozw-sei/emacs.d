(use-package origami
  :straight t
  :config
  (global-origami-mode 1)
  (defhydra hydra-folding (:color red)
    "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes))

(bind-key "<tab>" 'hydra-folding/body))
