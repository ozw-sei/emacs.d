;; Enhanced Scala development with Tree-sitter and Metals LSP
(use-package scala-ts-mode
  :straight t
  :mode (("\\.scala\\'" . scala-ts-mode)
         ("\\.sc\\'" . scala-ts-mode))
  :hook (scala-ts-mode . eglot-ensure)
  :config
  ;; Scala formatting and indentation
  (setq scala-ts-mode-indent-offset 2))

;; Fallback to regular scala-mode if tree-sitter not available
(use-package scala-mode
  :straight t
  :mode "\\.s\\(cala\\|bt\\)$"
  :hook (scala-mode . eglot-ensure)
  :config
  ;; Scala import indentation settings
  (setq scala-indent:align-parameters nil
        scala-indent:align-forms t
        scala-indent:use-javadoc-style t))

(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
  'minibuffer-complete-word
  'self-insert-command
  minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

;; Optional: Add Metals-specific keybindings
(with-eval-after-load 'scala-mode
  (with-eval-after-load 'eglot
    (define-key scala-mode-map (kbd "C-c m b") 'sbt-command)
    (define-key scala-mode-map (kbd "C-c m s") 'sbt-start)
    (define-key scala-mode-map (kbd "C-c m i") 'eglot-find-implementation)
    (define-key scala-mode-map (kbd "C-c m t") 'eglot-find-typeDefinition)))

;; Metals installation note:
;; To install Metals, run:
;; coursier install metals
;; or
;; cs install metals
