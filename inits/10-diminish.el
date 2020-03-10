
(use-package diminish
  :straight t
  :config
  (progn
    (add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "Lisp")))
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "elisp")))
    (add-hook 'texinfo-mode-hook (lambda () (setq mode-name "texi")))
    (add-hook 'change-log-mode-hook (lambda () (setq mode-name "CL")))
    (diminish 'isearch-mode)))
