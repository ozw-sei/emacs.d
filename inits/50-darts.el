(use-package dart-mode
  :straight t
  :custom
  (dart-format-on-save t)
  (dart-sdk-path "~/dev-tools/flutter/bin/cache/dart-sdk/"))

;; Use Eglot for Dart instead of lsp-dart
(add-hook 'dart-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(dart-mode . ("dart" "language-server" "--protocol=lsp"))))

(use-package hover
  :straight t)


(use-package flutter
  :straight t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "~/dev-tools/flutter/"))

(use-package flutter-l10n-flycheck
  :straight t
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))
