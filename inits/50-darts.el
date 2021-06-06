(use-package dart-mode
  :straight t
  :custom
  (dart-format-on-save t)
  (dart-sdk-path "~/dev-tools/flutter/bin/cache/dart-sdk/"))

(use-package lsp-dart
  :straight t
  :custom
  (lsp-dart-flutter-sdk-dir "~/dev-tools/flutter")
  :hook (dart-mode . lsp))

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
