(use-package consult-ghq
  :straight t
  :ensure t
  :after (consult ghq) ;; Assuming ghq command-line tool integration might be separate
  ;; Add any necessary keybindings or configuration for consult-ghq
  ;; For example, if there's a command like consult-ghq-open-project:
  ;; :bind ("C-c g p" . consult-ghq-open-project) 
  )
