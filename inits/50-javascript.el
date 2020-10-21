(use-package typescript-mode
  :straight t)

(use-package add-node-modules-path
  :straight t
  :hook ((javascript-mode typescript-mode))
  )

;; (use-package prettier-js
;;   :straight t)

(use-package npm-mode
  :straight t
  :hook (
         (javascript-mode . npm-mode)
         (typescript-mode . npm-mode)
         )
  )

;; javascript
(eval-after-load 'javascript-mode
  '(progn (add-hook 'javascript-mode #'add-node-modules-path)
          (add-hook 'javascript-mode #'npm-mode)
          (add-hook 'web-mode-hook #'prettier-js-mode)
          )
  )

(eval-after-load 'typescript-mode
  '(progn
     (add-hook 'typescript-mode-check #'add-node-modules-path)
     (add-hook 'web-mode-hook #'prettier-js-mode)

     )
  )
