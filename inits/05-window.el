(use-package ace-window
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key (kbd "C-o") 'ace-window)
  (global-set-key (kbd "C--") 'ace-swap-window)
  )
