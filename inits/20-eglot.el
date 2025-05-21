;; Eglot configuration
(use-package eglot
  :straight t
  :ensure t
  :hook ((js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (scala-mode . eglot-ensure)
         (dart-mode . eglot-ensure))
  :config
  (setq eglot-events-buffer-size 0) ; Disable event logging buffer
  (setq eglot-confirm-server-initiated-edits nil) ; Don't ask for confirmation for server edits
  ;; Add any other specific eglot configurations you deem necessary.
  (add-to-list 'eglot-server-programs '(scala-mode . ("sbt" "--client" "runMain" "org.jetbrains.sbt.ogliamo.Main"))) ; Example for Scala with Metals, adjust if needed
  (add-to-list 'eglot-server-programs '(dart-mode . ("dart" "language-server" "--protocol=lsp"))) ; Example for Dart, adjust if needed
)
