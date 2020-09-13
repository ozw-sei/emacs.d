(use-package swiper
  :straight t
  :config
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "p")
    ;; (interactive "P") ;; 大文字のPだと，C-u C-sでないと効かない
    (let (current-prefix-arg)
      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
  )

(use-package selected
  :straight t
  :init
  (selected-minor-mode 1)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("s" . flyspell-word)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines)
              (";" . comment-dwim)
              ("w" . kill-ring-save)
              ("j" . json-pretty-print)))


(use-package expand-region
  :straight t)

(use-package helm
  :straight t
  :config
  (helm-mode 1)
  (find-file-read-only . ido)

  (global-set-key (kbd "C-x f") 'helm-find-files)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  )


(use-package helm-selected
  :straight t
  :bind (:map selected-keymap ("h" . helm-selected)))

(use-package helm-smex
  :straight t
  :config
  (global-set-key [remap execute-extended-command] #'helm-smex)
  (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands))
