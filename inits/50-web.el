(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode)))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
                                        ;(add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
                                        ;(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
                                        ;(setq web-mode-content-types-alist
                                        ;'(("jsx" . "\\.js[x]?\\'")))
