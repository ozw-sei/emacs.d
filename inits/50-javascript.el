(use-package typescript-mode
  :straight t
  :custom
  (typescript-indent-level 2)
  )

(use-package add-node-modules-path
  :straight t
  :hook ((javascript-mode typescript-mode))
  )


(use-package npm-mode
  :straight t
  :hook (javascript-mode typescript-mode))
