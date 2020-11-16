;; ido-ghq
(require 'ido-ghq)
(setq ido-ghq-short-list t)

(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

(use-package ido-vertical-mode
  :straight t
  :after ido
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-max-window-height 0.75)
  )
