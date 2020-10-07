
(use-package lsp-mode
  :straight t
  :custom
  (lsp-auto-guess-root t)
  (lsp-response-timeout 5)
  (lsp-document-sync-method 'incremental)
  (lsp-enable-file-watchers 1)

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js-mode . lsp)
         (typescript-mode . lsp)
         (elixir-mode . lsp)
         (scala-mode . lsp)
         (lsp-managed-mode . (lambda () (setq-local company-backends '(company-capf)))))
  :config
  (setq lsp-diagnostics-modeline-scope :project)
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode)

  :bind
  ("C-c l" . 'hydra-lsp/body)
  ("C-<return>" . 'lsp-execute-code-action)

  :commands (lsp))

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-ui
  :after lsp-mode
  :straight t
  :if (eq system-type 'darwin)

  :custom
  (scroll-margin 0)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-flycheck-enable 1)
  (lsp-ui-sideline-enable 1)
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)

  :hook   (lsp-mode . lsp-ui-mode))

(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
                        [_M-r_] restart            [_D_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_d_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("D" lsp-find-declaration)
  ("d" lsp-ui-peek-find-definitions)
  ("u" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ;; ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(use-package lsp-python-ms
  :straight t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))
