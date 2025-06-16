(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode)))

(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-indentation nil)
 (let ((case-fold-search nil))
    (highlight-regexp "\\_<number\\|string\\|boolean\\|enum\\|unknown\\|any\\|void\\|null\\|undefined\\|never\\|object\\|symbol\\_>" 'font-lock-type-face))


                                        ;(add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
                                        ;(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
                                        ;(setq web-mode-content-types-alist
                                        ;'(("jsx" . "\\.js[x]?\\'")))
