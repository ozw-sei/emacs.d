;; Vertico configuration
(use-package vertico
  :straight t
  :ensure t
  :init
  (vertico-mode)
  ;; You can add custom vertico settings here if needed, for example:
  ;; (setq vertico-scroll-margin 0)
  ;; (setq vertico-count 20)
  ;; (setq vertico-resize t)
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :ensure t
  :init
  (savehist-mode))

;; Configure Orderless completion style.
(use-package orderless
  :straight t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using Marginalia
(use-package marginalia
  :straight t
  :ensure t
  ;; Bind marginalia-cycle to M-A
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :ensure t
  ;; Optionally, add some common bindings or further configurations if desired.
  ;; For example, to make consult-buffer available:
  :bind (
         ("C-s" . consult-line)
         ("M-y" . 'consult-yank-from-kill-ring)
         )
  ;;        ("C-x C-b" . consult-buffer))
  ;; However, let's stick to just adding the package for now,
  ;; specific bindings will be handled in later steps or by user preference.
  )



;; (use-package consult-migemo
;;   :straight t
;;   :ensure t
;;   :after consultn
;;   :config
;;   ;; Path to Migemo dictionary - USER NEEDS TO VERIFY/ADJUST THIS PATH
;;   (setq consult-migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

;;   (when (and (executable-find "cmigemo") (file-exists-p consult-migemo-dictionary))
;;     (require 'migemo)
;;     (setq migemo-command "cmigemo")
;;     (setq migemo-dictionary consult-migemo-dictionary)
;;     ;; Add other migemo settings if needed, like migemo-options
;;     ;; Enable migemo for various consult commands
;;     (setq consult-regexp-hist-migemo t) ;; for history
;;     (setq consult-grep-migemo t)        ;; for consult-grep, consult-ripgrep
;;     (setq consult-line-migemo t)        ;; for consult-line and consult-recent-file
;;     (setq consult-imenu-migemo t)
;;     (setq consult-outline-migemo t)
;;     (setq consult-mark-migemo t)
;;     (setq consult-buffer-migemo t)
;;     ;; Activate consult-migemo integration globally
;;     (consult-migemo-mode 1)
;;     (message "Consult-Migemo enabled with dictionary: %s" consult-migemo-dictionary))
;;   (unless (and (executable-find "cmigemo") (file-exists-p consult-migemo-dictionary))
;;     (message "Consult-Migemo: cmigemo executable not found or dictionary path invalid: %s. Migemo support not enabled." consult-migemo-dictionary)))


;; Optional: Consider adding `consult` and `embark` later if the user wants them.

(use-package embark
  :straight t
  :ensure t
  ;; Add bindings or configurations as needed
  :bind (("C-." . embark-act)         ;; Act on current completion
         ("M-." . embark-dwim)))      ;; Act on thing at point/region
