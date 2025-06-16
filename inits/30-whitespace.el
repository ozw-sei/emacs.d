(use-package whitespace
  :straight t
  :config
  (progn
    (add-hook 'ruby-mode-hook (lambda () (whitespace-mode 1)))
    (add-hook 'c-mode-common-hook (lambda () (whitespace-mode 1)))
    (setq whitespace-style '(face
                            trailing
                            tabs
                            spaces
                            lines-tail
                            newline
                            empty
                            indentation
                            space-after-tab
                            space-before-tab
                            space-mark
                            tab-mark
                            ))
    (set-face-background 'whitespace-newline 'nil)
    (set-face-background 'whitespace-space 'nil)
    (set-face-foreground 'whitespace-space "RGB:44/44/44")
    )
  )


;; Dependencies for unicode-whitespace
(use-package list-utils
  :straight t)

(use-package ucs-utils
  :straight t)

(use-package persistent-soft
  :straight t)

(use-package unicode-whitespace
  :straight t
  :after (list-utils ucs-utils persistent-soft)
  :config
  (unicode-whitespace-setup)
  )
