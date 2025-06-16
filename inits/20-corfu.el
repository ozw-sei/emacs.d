;;; Corfu - Completion framework
(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :custom
  ;; Enable auto completion and configure quitting
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)               ;; Enable cycling for `corfu-next/previous'
  (corfu-on-exact-match nil)    ;; Don't auto commit on exact match
  ;; Enable indentation+completion using the TAB key.
  (tab-always-indent 'complete)
  :bind
  (:map corfu-map
        ("TAB" . corfu-insert)
        ([tab] . corfu-insert)
        ("RET" . nil)
        ([return] . nil))
  :init
  (global-corfu-mode)
  :config
  ;; Ensure corfu works in terminal
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  
  ;; Enable Corfu in minibuffer
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (or (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil) ;; Disable auto completion
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer 1))

;;; Cape - Completion At Point Extensions
(use-package cape
  :straight t
  :hook 
  ;; Add useful defaults completion sources
  ((prog-mode . cape-setup-capf)
   (text-mode . cape-setup-capf))
  :config
  (defun cape-setup-capf ()
    "Setup completion at point functions."
    ;; Add cape functions to completion-at-point-functions
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-keyword))
  
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  
  ;; Ensure that pcomplete does not write to the buffer
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;;; Corfu terminal support
(when (not (display-graphic-p))
  (use-package corfu-terminal
    :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
    :after corfu
    :config
    (corfu-terminal-mode +1)))

;;; Kind-icon - Icons for corfu
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Corfu popupinfo extension
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.0))
  (corfu-popupinfo-max-height 10))

;;; Integration with orderless (already configured in vertico.el)
(with-eval-after-load 'orderless
  (defun orderless-fast-dispatch (word index total)
    "Fast completion dispatch for orderless."
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-literal . ,word)))
  
  (defun orderless-corfu-setup ()
    (setq-local orderless-matching-styles '(orderless-flex)
                completion-styles '(orderless basic)))
  
  (add-hook 'corfu-mode-hook #'orderless-corfu-setup))

;;; Eglot integration
(with-eval-after-load 'eglot
  (setq completion-category-overrides '((eglot (styles orderless flex)))))