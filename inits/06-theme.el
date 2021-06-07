;; (use-package solarized-theme
;;   :straight t
;;   :config
;;   (load-theme 'solarized-dark t))

(use-package monokai-theme
  :straight t
  :if (window-system)
  :config
  (load-theme 'monokai t))


(use-package srcery-theme
  :straight t
  :if (not window-system)
  :config
  (load-theme 'srcery t))


;; (use-package zenburn-theme
;;   :straight t
;;   :config
;;   (load-theme 'zenburn t))
