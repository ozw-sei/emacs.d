;; Eglot configuration
(use-package eglot
  :straight t
  :ensure t
  :hook ((js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (scala-mode . eglot-ensure)
         (dart-mode . eglot-ensure)
         (csharp-mode . eglot-ensure)) ;; Added line
  :config
  (setq eglot-events-buffer-size 0) ; Disable event logging buffer
  (setq eglot-confirm-server-initiated-edits nil) ; Don't ask for confirmation for server edits
  (setq eglot-autoshutdown t) ; Automatically shutdown servers
  (setq eglot-sync-connect 5) ; Timeout for initial connection
  
  ;; Server configurations
  (add-to-list 'eglot-server-programs '(scala-mode . ("sbt" "--client" "runMain" "org.jetbrains.sbt.ogliamo.Main"))) ; Example for Scala with Metals, adjust if needed
  (add-to-list 'eglot-server-programs '(dart-mode . ("dart" "language-server" "--protocol=lsp"))) ; Example for Dart, adjust if needed
  
  :bind*
  ("C-c l" . 'hydra-eglot/body)
  ("M-<return>" . 'eglot-code-actions))

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
  ("D" xref-find-declarations)
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
  ("S" eglot-shutdown))
