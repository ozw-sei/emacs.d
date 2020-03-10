(use-package typescript-mode
  :straight t)

(use-package flow-minor-mode
  :straight t)

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode typescript-mode))
  )

(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)

(use-package js2-refactor
  :straight t
  :config
  (js2r-add-keybindings-with-prefix "C-c m"))

(use-package prettier-js
  :straight t)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)



;; javascript
(eval-after-load 'js-mode
  '(add-hook 'js-mode-check #'add-node-modules-path))

(eval-after-load 'typescript-mode
  '(add-hook 'typescript-mode-check #'add-node-modules-path))
