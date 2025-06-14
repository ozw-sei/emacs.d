(use-package claude-code
  :straight (:host github :repo "anthropics/claude-code" :files ("*.el"))
  :commands (claude-code-query
             claude-code-query-region
             claude-code-review-buffer
             claude-code-explain)
  :custom
  ;; Set your Claude API key here or use environment variable
  (claude-code-api-key (getenv "CLAUDE_API_KEY"))
  ;; Model selection
  (claude-code-model "claude-3-opus-20240229")
  ;; Optional: Set maximum tokens for responses
  (claude-code-max-tokens 4096)
  ;; Optional: Temperature setting for creativity
  (claude-code-temperature 0.7)
  
  :config
  ;; Create a hydra for claude-code commands
  (defhydra hydra-claude-code (:color blue :hint nil)
    "
Claude Code Commands:
_q_: Query Claude          _r_: Query region
_b_: Review buffer         _e_: Explain code
_d_: Document function     _i_: Improve code
_t_: Generate tests        _f_: Fix errors
_c_: Complete code         _h_: Get help
_g_: Generate code         _s_: Summarize
"
    ("q" claude-code-query)
    ("r" claude-code-query-region)
    ("b" claude-code-review-buffer)
    ("e" claude-code-explain)
    ("d" claude-code-document-function)
    ("i" claude-code-improve)
    ("t" claude-code-generate-tests)
    ("f" claude-code-fix-errors)
    ("c" claude-code-complete)
    ("h" claude-code-help)
    ("g" claude-code-generate)
    ("s" claude-code-summarize))
  
  ;; Optional: Setup mode-specific keybindings
  (defun claude-code-setup-keybindings ()
    "Setup claude-code keybindings for programming modes."
    (local-set-key (kbd "C-c c q") 'claude-code-query)
    (local-set-key (kbd "C-c c r") 'claude-code-query-region)
    (local-set-key (kbd "C-c c e") 'claude-code-explain)
    (local-set-key (kbd "C-c c d") 'claude-code-document-function)
    (local-set-key (kbd "C-c c i") 'claude-code-improve)
    (local-set-key (kbd "C-c c t") 'claude-code-generate-tests))
  
  ;; Add keybindings to programming modes
  (add-hook 'prog-mode-hook 'claude-code-setup-keybindings)
  
  :bind
  ;; Global keybinding for the hydra
  ("C-c C" . hydra-claude-code/body)
  ;; Direct global bindings
  ("C-c c q" . claude-code-query)
  ("C-c c r" . claude-code-query-region))

;; Optional: If claude-code supports company-mode backend
(use-package company-claude-code
  :after (company claude-code)
  :straight (:host github :repo "anthropics/company-claude-code")
  :config
  (add-to-list 'company-backends 'company-claude-code))