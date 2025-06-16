;; yasnippet
(use-package yasnippet
  :straight t
  :init
  (yas-global-mode 1)
  :commands yas-reload-all
  :delight yas-minor-mode
  :hook ((prog-mode). yas-minor-mode)
  :config (yas-reload-all)
  :diminish yas-minor-mode
  ;; Removed company-yasnippet binding as we're no longer using company-mode
  ;; Use C-t for consult-yasnippet instead
  )

(use-package yasnippet-snippets
  :straight t)

(use-package consult-yasnippet
  :straight t
  :ensure t
  :after (yasnippet consult) ;; Ensure yasnippet and consult are loaded first
  :config
  ;; Bind C-t to consult-yasnippet or its main interactive command
  (global-set-key (kbd "C-t") 'consult-yasnippet))
