(use-package iflipb
  :straight t
  :config
  (setq iflipb-ignore-buffers (list "^[*]" "^magit" "]$"))
  (setq iflipb-wrap-around t)
  :bind

  ;; pg up
  ("M-<prior>" . iflipb-previous-buffer)
  ;; pg down
  ("M-<next>" . iflipb-next-buffer)
)
