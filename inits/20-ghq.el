;; ido-ghq
(require 'ido-ghq)
(setq ido-ghq-short-list t)
(bind-key "M-p" 'ido-ghq-open)

(use-package ido-vertical-mode
  :straight t
  :after ido
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-max-window-height 0.75)
  )
