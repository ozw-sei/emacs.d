;; flycheck
(use-package flycheck
  :straight t
  :config

  (global-flycheck-mode 1)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  )

(use-package flycheck-color-mode-line
  :straight t
  :config
  (flycheck-color-mode-line-mode 1))

(use-package flycheck-pos-tip
  :straight t
  :config
  (flycheck-pos-tip-mode 1))

(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

;; hydra flcheck 操作
(defhydra hydra-flycheck nil
  "hydra-flycheck"
  ("j" flycheck-next-error     "next-error")
  ("k" flycheck-previous-error "prev-error")
  ("h" flycheck-first-error    "first-error")
  ("l" (lambda ()
        (interactive)
        (flycheck-list-errors)
        (windmove-right)) "list-errors" :exit t)
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil "exit" :color blue))

(bind-key "C-;" 'hydra-flycheck/body)
