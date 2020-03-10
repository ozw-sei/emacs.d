
;;; Ruby-mode
(use-package ruby-mode
  :straight t
  :mode (
        ("\\Vagrantfile$" . ruby-mode)

        ("\\.rb$" . ruby-mode))
  )


(use-package ruby-hash-syntax
  :straight t)

(setq-default
 ruby-use-encoding-map nil
 ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook 'subword-mode)

(use-package rspec-mode
  :straight t)

(use-package inf-ruby
  :straight t)

(use-package bundler
  :straight t)

(use-package ruby-electric
  :straight t)

(use-package inflections
  :straight t)

(use-package rake
  :straight t)

(require 'rbenv)
(global-rbenv-mode)
(setq rbenv-installation-dir "~/.rbenv")

(setq ruby-insert-encoding-magic-comment nil)
(custom-set-variables '(ruby-insert-encoding-magic-comment nil))
