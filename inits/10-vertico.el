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
  ;; :bind (("C-x b" . consult-buffer)
  ;;        ("C-x C-b" . consult-buffer)) 
  ;; However, let's stick to just adding the package for now, 
  ;; specific bindings will be handled in later steps or by user preference.
)

;; Optional: Consider adding `consult` and `embark` later if the user wants them.
;; (use-package embark
;;   :straight t
;;   :ensure t
;;   ;; Add bindings or configurations as needed
;;   :bind (("C-." . embark-act)         ;; Act on current completion
;;          ("M-." . embark-dwim)))      ;; Act on thing at point/region
