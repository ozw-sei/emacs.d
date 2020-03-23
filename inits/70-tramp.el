(use-package docker-tramp
  :if (executable-find "docker")
  :straight t)

(use-package vagrant-tramp
  :if (executable-find "vagrant")
  :straight t)

(use-package counsel-tramp
  :straight t)
