(use-package company
  :diminish (company-mode)
  :straight t
  :custom
  (company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (company-minimum-prefix-length 2) ; デフォルトは4
  (company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.5)

  :config
  (global-company-mode) ; 全バッファで有効にする

  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
  )

(use-package company-box
  :straight t
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-flow))

(use-package company-prescient
  :straight t
  :config
  (company-prescient-mode 1))
