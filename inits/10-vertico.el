;; Vertico configuration
(use-package vertico
  :straight t
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-count 10)                    ;; Number of candidates to display
  (vertico-resize t)                    ;; Resize minibuffer
  (vertico-cycle t)                     ;; Enable cycling for `vertico-next/previous'
  :config
  ;; Make M-x use Vertico with enhanced completion
  (define-key global-map [remap execute-extended-command] #'execute-extended-command))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :ensure t
  :init
  (savehist-mode)
  :custom
  (history-length 25)
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-file "~/.emacs.d/savehist"))

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
  :bind (
         ("C-s" . consult-line)
         ("M-y" . 'consult-yank-from-kill-ring)
         ;; Make M-x smarter with consult
         ("C-x C-c" . consult-mode-command)
         ("C-x M-:" . consult-complex-command)
         )
  :custom
  (consult-narrow-key "<")  ;; Use < for narrowing
  :config
  ;; Configure preview for M-x
  (consult-customize
   consult-mode-command :preview-key '(:debounce 0.2 any)))



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
