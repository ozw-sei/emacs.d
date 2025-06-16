;;; Imenu configuration
(use-package imenu
  :straight nil  ;; Built-in package
  :custom
  (imenu-auto-rescan t)
  (imenu-use-popup-menu nil)
  :config
  ;; Better imenu presentation
  (setq imenu-max-item-length 100))

;; Consult integration for imenu
(use-package consult-imenu
  :straight nil  ;; Part of consult
  :after consult
  :bind (("C-c i" . consult-imenu)
         ("C-c I" . consult-imenu-multi)))

;; Enhanced imenu support for various languages
(use-package imenu-list
  :straight t
  :bind (("C-c C-i" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

;; Better imenu support for specific modes
(use-package imenu-anywhere
  :straight t
  :bind (("C-c j" . imenu-anywhere)))

;; Add imenu support for use-package declarations
(with-eval-after-load 'imenu
  (add-to-list 'imenu-generic-expression
               '("use-package"
                 "\\(^\\s-*(use-package\\s-+\\)\\(\\(?:\\sw\\|\\s_\\|\\s.\\)+\\)"
                 2)))

;; Treemacs integration
(with-eval-after-load 'treemacs
  (define-key global-map (kbd "C-c t i") #'treemacs-imenu))