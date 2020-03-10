(use-package counsel
  :straight t
  :diminish (ivy counsel-mode)
  :config
  (setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../" ".DS_Store" ".git" ".meta" )))

  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style t)
  (setq ivy-wrap t)
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1))

  (setq ivy-height 30)

  (custom-set-faces
  '(ivy-current-match
    ((((class color) (background light))
      :background "#FFF3F3" :distant-foreground "#000000")
      (((class color) (background dark))
      :background "#404040" :distant-foreground "#abb2bf")))
  '(ivy-minibuffer-match-face-1
    ((((class color) (background light)) :foreground "#666666")
      (((class color) (background dark)) :foreground "#999999")))
  '(ivy-minibuffer-match-face-2
    ((((class color) (background light)) :foreground "#c03333" :underline t)
      (((class color) (background dark)) :foreground "#e04444" :underline t)))
  '(ivy-minibuffer-match-face-3
    ((((class color) (background light)) :foreground "#8585ff" :underline t)
      (((class color) (background dark)) :foreground "#7777ff" :underline t)))
  '(ivy-minibuffer-match-face-4
    ((((class color) (background light)) :foreground "#439943" :underline t)
      (((class color) (background dark)) :foreground "#33bb33" :underline t))))

  (ivy-mode 1)
  (counsel-mode 1)
  ;; counsel-switch-to-buffer を読んでほしくない.
  (define-key counsel-mode-map [remap switch-to-buffer]  nil)
  (define-key ivy-mode-map [remap switch-to-buffer]  nil)

  (define-key counsel-mode-map [remap recentf]  nil)
  (define-key ivy-mode-map [remap recentf]  nil)

  :bind
  ("M-x" . 'counsel-M-x)
  ("M-o" . 'swiper-thing-at-point)
  ("M-O" . 'swiper)

  ;;("C-x C-r" . 'counsel-recentf)
  ("M-y" . 'counsel-yank-pop))

(use-package selected
  :straight t
  :init
  (setq selected-org-mode-map (make-sparse-keymap))
  (selected-global-mode 1)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("s" . flyspell-word)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines)
              (";" . comment-dwim)
              ("w" . kill-ring-save)
              ("j" . json-pretty-print)
              ("t" . org-table-convert-region)))

(require 'counsel-selected)
(define-key selected-keymap (kbd "h") 'counsel-selected)
