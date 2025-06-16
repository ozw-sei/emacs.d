;;(use-package tree-sitter-langs
;;  :config
;;  (setq tree-sitter-major-mode-language-alist
;;        (append tree-sitter-major-mode-language-alist
;;                '((typescript-mode . tsx)
;;                  (tsx-mode . tsx)))))
;;
;; TypeScript / TSX モードで tree-sitter を有効にする
;;(add-hook 'typescript-mode-hook #'tree-sitter-mode)
;;(add-hook 'tsx-mode-hook #'tree-sitter-mode)

;; TypeScript and TSX files
(use-package tsx-ts-mode
  :mode (("\\.tsx?\\'" . tsx-ts-mode)
         ("\\.[m]ts\\'" . tsx-ts-mode)))

;; JavaScript files
(use-package js-ts-mode
  :mode (("\\.js[x]?\\'" . js-ts-mode)
         ("\\.[mc]js\\'" . js-ts-mode)))

;; Python files
(use-package python-ts-mode
  :mode ("\\.py$" . python-ts-mode))

;; C files
(use-package c-ts-mode
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)))

;; C++ files
(use-package c++-ts-mode
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hxx\\'" . c++-ts-mode)
         ("\\.hh\\'" . c++-ts-mode)))

;; Java files
(use-package java-ts-mode
  :mode ("\\.java\\'" . java-ts-mode))

;; Ruby files
(use-package ruby-ts-mode
  :mode (("\\.rb\\'" . ruby-ts-mode)
         ("\\.rake\\'" . ruby-ts-mode)
         ("\\.gemspec\\'" . ruby-ts-mode)
         ("Rakefile\\'" . ruby-ts-mode)
         ("Gemfile\\'" . ruby-ts-mode)))

;; Scala files (if tree-sitter grammar is available)
(use-package scala-ts-mode
  :mode (("\\.scala\\'" . scala-ts-mode)
         ("\\.sc\\'" . scala-ts-mode)))
