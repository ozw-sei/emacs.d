;; Prescient - Smart sorting and filtering
(use-package prescient
  :straight t
  :custom
  (prescient-history-length 200)
  (prescient-save-file "~/.emacs.d/prescient-save.el")
  (prescient-filter-method '(literal regexp initialism fuzzy))
  :config
  (prescient-persist-mode +1))

;; Vertico integration with prescient
(use-package vertico-prescient
  :straight t
  :after (vertico prescient)
  :custom
  (vertico-prescient-enable-filtering nil) ;; We use orderless for filtering
  :config
  (vertico-prescient-mode +1))