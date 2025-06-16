;; Eglot configuration
(use-package eglot
  :straight t
  :ensure t
  :hook ((js-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (scala-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (ruby-ts-mode . eglot-ensure)
         (perl-mode . eglot-ensure)
         (cperl-mode . eglot-ensure))
  :bind (:map eglot-mode-map
         ("C-c l" . 'hydra-eglot/body)
         ("C-c C-l" . 'hydra-eglot/body)  ;; Alternative binding
         ("M-<return>" . 'eglot-code-actions))
  :config
  (setq eglot-events-buffer-size 0) ; Disable event logging buffer
  (setq eglot-confirm-server-initiated-edits nil) ; Don't ask for confirmation for server edits
  (setq eglot-autoshutdown t) ; Automatically shutdown servers
  (setq eglot-sync-connect 5) ; Timeout for initial connection

  ;; Server configurations
  ;; Metals for Scala - using the recomm
  (add-to-list 'eglot-server-programs '(scala-mode . ("metals")))

  ;; Function to check if current file is a library file
  (defun my/is-library-file-p ()
    "Check if current file is in a library/external directory."
    (and buffer-file-name
         (or
          ;; Python site-packages
          (string-match-p "site-packages" buffer-file-name)
          ;; Node modules
          (string-match-p "node_modules" buffer-file-name)
          ;; Go pkg/mod
          (string-match-p "pkg/mod" buffer-file-name)
          ;; Rust cargo registry
          (string-match-p "\\.cargo/registry" buffer-file-name)
          ;; Ruby gems
          (string-match-p "/gems/" buffer-file-name)
          ;; Straight.el packages
          (string-match-p "straight/\\(repos\\|build\\)" buffer-file-name)
          ;; System libraries
          (string-match-p "^/usr/\\(local/\\)?\\(lib\\|share\\)" buffer-file-name)
          ;; Homebrew
          (string-match-p "^/opt/homebrew" buffer-file-name)
          (string-match-p "^/usr/local/Cellar" buffer-file-name))))

  ;; Make library/external files read-only when jumping to definitions
  (defun my/make-library-files-read-only ()
    "Make files read-only if they are in library/external directories."
    (when (my/is-library-file-p)
      (read-only-mode 1)
      (setq-local buffer-read-only-message "📚 Library file - Press C-x C-q to edit")
      (message "📚 Opened library file in read-only mode: %s (C-x C-q to toggle)" (file-name-nondirectory buffer-file-name))))

  ;; Apply to files opened via xref (which Eglot uses)
  (add-hook 'find-file-hook #'my/make-library-files-read-only)

  ;; Add visual indicator to mode line for library files
  (defun my/library-file-mode-line-indicator ()
    "Return a mode line indicator for library files."
    (when (my/is-library-file-p)
      (propertize " [LIB]"
                  'face '(:foreground "#ff6c6b" :weight bold)
                  'help-echo "This is a library/external file (read-only)")))

  ;; Add to mode-line
  (add-to-list 'mode-line-misc-info '(:eval (my/library-file-mode-line-indicator)))

  ;; Optional: Change background color for library files
  (defface my/library-file-face
    '((t :background "#2a2a2a"))
    "Face for library file buffers")

  (defun my/highlight-library-files ()
    "Apply special face to library file buffers."
    (when (my/is-library-file-p)
      (face-remap-add-relative 'default 'my/library-file-face)))

  (add-hook 'find-file-hook #'my/highlight-library-files))

;; For symbol search with vertico/consult
(use-package consult-eglot
  :straight t
  :after (eglot consult))

;; Hydra for Eglot
(defhydra hydra-eglot (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
                        [_M-r_] reconnect          [_D_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_d_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] show config        [_u_] references   [_s_] symbols"
  ("l" eglot)
  ("D" eglot-find-declaration)
  ("d" xref-find-definitions)
  ("u" xref-find-references)
  ("i" eglot-find-implementation)
  ("t" eglot-find-typeDefinition)
  ("o" eldoc-doc-buffer)
  ("s" consult-eglot-symbols)
  ("r" eglot-rename)

  ("m" consult-imenu)
  ("x" eglot-code-actions)

  ("M-s" eglot-show-workspace-configuration)
  ("M-r" eglot-reconnect)
  ("S" eglot-shutdown)
  ("q" nil "quit"))
