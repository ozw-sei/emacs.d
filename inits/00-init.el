
; terminal にはtmuxがあるので使わない
(if window-system
  (progn (bind-key* "C-q" 'hydra-buffer-split/body))
  (bind-key* "C-q" nil)
)
;; Emacs 本体の設定
;;; ログはエラーが出た時のみ
(defvar display-warning-minimum-level :error)

;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-refresh-contents)

;;; symlinkは必ず追いかける
(setq vc-follow-symlinks t)

(set-face-attribute 'default nil :height 100)

;; 同時に二つEmacsが起動しないようにする（mac）
(setq ns-pop-up-frames nil)

;;; メニューバーとツールバーとスクロールバーを消す
(if window-system (progn
                    ;; ←GUI用設定を、ここに記述
                    (menu-bar-mode -1)
                    (tool-bar-mode -1)
                    (scroll-bar-mode -1)
                    (bind-key "C-x C-c" 'counsel-M-x)
                    )
  )

;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;;; 現在行に色をつける
(global-hl-line-mode 1)

;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; シェルに合わせるため、C-hはに割り当てる
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
;;(global-set-key "\C-h" 'delete-backward-char)

;;; モードラインに時刻を表示する
(display-time)

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; 履歴をたくさん保存する
(setq history-length 1000)


;; I never use C-x C-c
;; exit で抜けられます
(defalias 'exit 'save-buffers-kill-emacs)

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

(global-auto-revert-mode 1)

;; 大文字小文字を区別しない
(setq completion-ignore-case t)


;; common-setting
;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;; splash screenを無効にする
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-u C-SPC C-SPC …でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;; https://www.emacswiki.org/emacs/AnsiColor
(use-package ansi-color
  :straight t
  :mode
  (("\\.log$" . display-ansi-colors))
  :config
  (progn
    ;; http://stackoverflow.com/a/23382008
    (defun display-ansi-colors ()
      (interactive)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))))

; ビープ音禁止
(setq ring-bell-function 'ignore)

;; 自動改行しない
(setq auto-fill-mode 0)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)

(setq garbage-collection-messages t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq set-mark-command-repeat-pop t)


; terminal にはtmuxがあるので使わない
(if window-system
  (progn (bind-key* "C-q" 'hydra-buffer-split/body))
  (bind-key* "C-q" nil)
)


;; 行頭の kill-line (C-k) で行ごと削除
(setq kill-whole-line t)

;; *scratch* バッファの初期メッセージを消す
(setq initial-scratch-message "")
