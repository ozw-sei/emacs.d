;;; beginend - Smart beginning/end of buffer movement
(use-package beginend
  :straight t
  :diminish beginend-global-mode
  :config
  (beginend-global-mode)
  
  ;; Additional modes that might not be covered by default
  (dolist (mode '(vterm-mode))
    (beginend-define-mode mode
      (progn
        (beginend-goto-beginning))
      (progn
        (beginend-goto-end)))))

;; beginend improves the behavior of C-a (beginning) and C-e (end) commands
;; For example:
;; - In prog-mode: C-a goes to beginning of code (after indentation)
;; - In message-mode: C-a goes after headers
;; - In occur-mode: C-a goes to beginning of match
;; - And many more smart behaviors for different modes