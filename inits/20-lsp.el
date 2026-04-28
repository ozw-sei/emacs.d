;; LSP configuration with lsp-mode
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((js-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (scala-ts-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (ruby-ts-mode . lsp-deferred)
         (csharp-ts-mode . lsp-deferred)
         (dockerfile-ts-mode . lsp-deferred)
         (yaml-ts-mode . lsp-deferred)
         (bash-ts-mode . lsp-deferred)
         (json-ts-mode . lsp-deferred)
         (css-ts-mode . lsp-deferred)
         (html-ts-mode . lsp-deferred)
         (toml-ts-mode . lsp-deferred)
         (perl-mode . lsp-deferred)
         (cperl-mode . lsp-deferred))
  :custom
  ;; Use corfu for completion (not company)
  (lsp-completion-provider :none)
  ;; Use flycheck (not flymake) for diagnostics
  (lsp-diagnostics-provider :flycheck)
  ;; Performance tuning
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-enable-file-watchers nil)
  ;; UI preferences
  (lsp-headerline-breadcrumb-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable t)
  ;; Auto shutdown
  (lsp-keep-workspace-alive nil)
  :config
  ;; Ensure corfu capf integration
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless flex)))
  (add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)

  ;; Function to check if current file is a library file
  (defun my/is-library-file-p ()
    "Check if current file is in a library/external directory."
    (and buffer-file-name
         (or
          (string-match-p "site-packages" buffer-file-name)
          (string-match-p "node_modules" buffer-file-name)
          (string-match-p "pkg/mod" buffer-file-name)
          (string-match-p "\\.cargo/registry" buffer-file-name)
          (string-match-p "/gems/" buffer-file-name)
          (string-match-p "straight/\\(repos\\|build\\)" buffer-file-name)
          (string-match-p "^/usr/\\(local/\\)?\\(lib\\|share\\)" buffer-file-name)
          (string-match-p "^/opt/homebrew" buffer-file-name)
          (string-match-p "^/usr/local/Cellar" buffer-file-name))))

  ;; Make library/external files read-only when jumping to definitions
  (defun my/make-library-files-read-only ()
    "Make files read-only if they are in library/external directories."
    (when (my/is-library-file-p)
      (read-only-mode 1)
      (message "Library file (read-only): %s (C-x C-q to toggle)" (file-name-nondirectory buffer-file-name))))

  (add-hook 'find-file-hook #'my/make-library-files-read-only)

  ;; Add visual indicator to mode line for library files
  (defun my/library-file-mode-line-indicator ()
    "Return a mode line indicator for library files."
    (when (my/is-library-file-p)
      (propertize " [LIB]"
                  'face '(:foreground "#ff6c6b" :weight bold)
                  'help-echo "This is a library/external file (read-only)")))

  (add-to-list 'mode-line-misc-info '(:eval (my/library-file-mode-line-indicator)))

  ;; Change background color for library files
  (defface my/library-file-face
    '((t :background "#2a2a2a"))
    "Face for library file buffers")

  (defun my/highlight-library-files ()
    "Apply special face to library file buffers."
    (when (my/is-library-file-p)
      (face-remap-add-relative 'default 'my/library-file-face)))

  (add-hook 'find-file-hook #'my/highlight-library-files))

;; lsp-ui for enhanced UI
(use-package lsp-ui
  :straight t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-peek-enable t))

;; Consult integration for lsp-mode
(use-package consult-lsp
  :straight t
  :after (lsp-mode consult)
  :config
  ;; consult-lsp-diagnostics: browse all LSP diagnostics with filtering
  ;; consult-lsp-symbols: workspace-wide symbol search
  ;; consult-lsp-file-symbols: symbols in current file
  )

;; Consult integration for flycheck
(use-package consult-flycheck
  :straight t
  :after (consult flycheck))

;; Hydra for LSP operations
(defhydra hydra-lsp (:exit t :hint nil)
  "
 Navigate^^             Server^^                   Symbol^^                   Diagnostics
─────────────────────────────────────────────────────────────────────────────────────────
 [_d_] definition       [_M-r_] reconnect          [_s_] symbols (workspace)  [_e_] errors (consult)
 [_D_] declaration      [_S_]   shutdown           [_f_] symbols (file)       [_j_] next error
 [_u_] references       [_M-s_] describe session   [_r_] rename               [_k_] prev error
 [_i_] implementation   [_l_]   start lsp          [_x_] code action
 [_t_] type def         ^^                          [_o_] documentation
 [_m_] imenu"
  ("l" lsp)
  ("D" lsp-find-declaration)
  ("d" lsp-find-definition)
  ("u" lsp-find-references)
  ("i" lsp-find-implementation)
  ("t" lsp-find-type-definition)
  ("o" lsp-ui-doc-glance)
  ("s" consult-lsp-symbols)
  ("f" consult-lsp-file-symbols)
  ("r" lsp-rename)
  ("x" lsp-execute-code-action)
  ("m" consult-imenu)

  ;; Diagnostics
  ("e" consult-lsp-diagnostics)
  ("j" flycheck-next-error :exit nil)
  ("k" flycheck-previous-error :exit nil)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown)
  ("q" nil "quit"))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l") 'hydra-lsp/body)
  (define-key lsp-mode-map (kbd "C-c C-l") 'hydra-lsp/body)
  (define-key lsp-mode-map (kbd "M-<return>") 'lsp-execute-code-action))
