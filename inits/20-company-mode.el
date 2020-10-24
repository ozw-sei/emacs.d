(use-package company
  :diminish (company-mode)
  :straight t
  :custom
  (company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (company-minimum-prefix-length 2) ; デフォルトは4
  (company-echo-delay 0)

  (company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.3)

  :config
  (global-company-mode) ; 全バッファで有効にする
  (company-mode 1)

  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "C-f") 'company-complete-selection)
  (define-key emacs-lisp-mode-map (kbd "M-/") 'company-complete)
  (define-key emacs-lisp-mode-map (kbd "C-<tab>") 'company-capf)

  (defvar company-mode/enable-yas t)
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (add-to-list 'company-backends '(company-bbdb
                                   company-nxml
                                   company-css
                                   company-eclim
                                   company-semantic
                                   company-clang
                                   company-xcode
                                   company-cmake
                                   company-capf
                                   company-dabbrev-code
                                   company-gtags
                                   company-etags
                                   company-keywords
                                   company-oddmuse
                                   company-files
                                   company-dabbrev
                                   mapcar
                                   company-mode/backend-with-yas
                                   company-irony
                                   company-c-headers
                                   company-auctex
                                   company-math-symbols-unicode
                                   company-lsp
                                   company-elisp
                                   company-yasnippet)
               )
  )



(use-package company-box
  :straight t
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :straight t
  :hook (company-mode-hook . company-quickhelp-mode)
  )
