(use-package swiper
  :straight t
  :config
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "p")
    ;; (interactive "P") ;; 大文字のPだと，C-u C-sでないと効かない
    (let (current-prefix-arg)
      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper))

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
  :diminish (helm-mode)
  :config
  (helm-mode 1)
  :bind (
         ("C-x f" . 'helm-find-files)
         ("C-x C-f" . 'helm-find-files)
         ("M-y" . 'helm-show-kill-ring))
  )

(use-package shackle
  :straight t
  :config
  (setq shackle-rules
      '(;; *compilation*は下部に2割の大きさで表示
        (compilation-mode :align below :ratio 0.2)
        ;; ヘルプバッファは右側に表示
        ("*Help*" :align right)
        ;; 補完バッファは下部に3割の大きさで表示
        ("*Completions*" :align below :ratio 0.3)
        ;; M-x helm-miniは下部に7割の大きさで表示
        ("*helm mini*" :align below :ratio 0.7)
        ;; 他のhelmコマンドは右側に表示 (バッファ名の正規表現マッチ)
        ("\*helm" :regexp t :align bottom)
        ;; 上部に表示
        ("foo" :align above)
        ;; 別フレームで表示
        ("bar" :frame t)
        ;; 同じウィンドウで表示
        ("baz" :same t)
        ;; ポップアップで表示
        ("hoge" :popup t)
        ;; 選択する
        ("abc" :select t)
        ))
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  (shackle-mode 1)
  )

(use-package helm-selected
  :straight t
  :bind (:map selected-keymap ("h" . helm-selected)))


(use-package helm-smex
  :straight t
  :config
  (global-set-key [remap execute-extended-command] #'helm-smex)
  (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands))


(use-package helm-descbinds
  :straight t
  :config
  (helm-descbinds-mode)
  :bind ("C-c h" . 'helm-descbinds))

(use-package helm-make
  :straight t)


(defhydra hydra-helm (:hint nil :color pink)
        "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
        ("<tab>" helm-keyboard-quit "back" :exit t)
        ("<escape>" nil "quit")
        ("\\" (insert "\\") "\\" :color blue)
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("n" helm-next-source)
        ("p" helm-previous-source)
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ("c" helm-recenter-top-bottom-other-window)
        ("m" helm-toggle-visible-mark)
        ("t" helm-toggle-all-marks)
        ("u" helm-unmark-all)
        ("H" helm-help)
        ("s" helm-buffer-help)
        ("v" helm-execute-persistent-action)
        ("d" helm-persistent-delete-marked)
        ("y" helm-yank-selection)
        ("w" helm-toggle-resplit-and-swap-windows)
        ("f" helm-follow-mode))


(define-key helm-map (kbd "C-t") 'hydra-helm/body)
