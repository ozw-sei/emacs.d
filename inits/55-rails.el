
(use-package projectile-rails
  :straight t
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'hydra-projectile-rails/body)
)
