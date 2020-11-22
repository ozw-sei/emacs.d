(use-package restclient
  :straight t)


(use-package company-restclient
  :straight t
  :after restclient
  :config (add-to-list 'company-backends 'company-restclient))
