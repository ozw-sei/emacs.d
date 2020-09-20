(use-package mwim
  :straight t
  :bind
  ("C-a" . 'mwim-beginning-of-code-or-line)
  ("C-e" . 'mwim-end-of-code-or-line))

(require 'mwe-log-commands)
