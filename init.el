;; package configuration
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; elisp read config
(add-to-list 'load-path "~/.emacs.d/elisp")

(setq mac-command-modifier 'control)

(package-initialize)

(package-refresh-contents)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defvar favorite-packages
  '(
    ;; package
    use-package
    ;; ido-mode\
    ido-vertical-mode
    ;; smex
    smex 
    ;; cursor position
    saveplace
    ;; git
    magit git-gutter
    ;; powerline
    powerline
    ;; company
    company

    ;; yasnippet
    yasnippet
    ;; projectile
    projectile
 
   ;; ido-ubiquitous
    ido-ubiquitous

    ;; avy / ace-jump
    avy
    
    ;; javascript / typescript
    typescript-mode
    ;; lsp
    lsp-mode
    company-lsp
    lsp-ui
    
    ;; go
    go-mode
    company-go

    ;; theme
    solarized-theme

    ;; migemo
    migemo

    ;; elixir-mode
    elixir-mode
    alchemist
    flycheck-elixir

    ;; ag
    ag
    ))

(require 'company-lsp)

(push 'company-lsp company-backends)


(dolist (package favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'company)
(global-company-mode) ; 全バッファで有効にする 
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

(require 'saveplace)
(save-place-mode 1)

;; git
(git-gutter-mode 1)

(defun turn-on-flycheck-mode ()
  (flycheck-mode 1))

 ;; ビープ音禁止
 (setq ring-bell-function 'ignore)

;; 自動改行しない
(setq auto-fill-mode 0)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

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


(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(set-face-attribute 'default nil :height 100)

(setq ido-enable-flex-matching t)

(require 'powerline)

(powerline-default-theme)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ag typescript-mode flycheck-elixir alchemist elixir-mode avy ido-ubiquitous projectile company migemo ido-vertical-mode package-utils use-package undohist smex powerline magit-stgit magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'avy)
(global-set-key (kbd "C-]") 'avy-goto-char)
(global-set-key (kbd "M-g f") 'avy-goto-line)


(require 'elixir-mode)
(require 'alchemist)
(require 'flycheck-elixir)

(setq alchemist-key-command-prefix (kbd "C-c ,"))

;; typescript
(add-hook 'javascript-mode-hook #'lsp)
(require 'company-lsp)

(load-theme 'solarized-light t)

;; migemo
(require 'migemo)
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; git-gutter
(require 'git-gutter)
(git-gutter-mode 1)

;; point-undo
(require 'point-undo)
(global-set-key (kbd "C--") 'point-undo)
(global-set-key (kbd "C-=") 'point-redo)


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
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "[^*]+")

;; mac-key-modifier
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'command)

;; font-size
(set-face-attribute 'default nil :height 150)

;;; ファイルを開いた位置を保存する
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;;; 釣合う括弧をハイライトする
(show-paren-mode 1)

;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;;; 現在行に色をつける
(global-hl-line-mode 1)

;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; シェルに合わせるため、C-hは後退に割り当てる
(global-set-key (kbd "C-h") 'delete-backward-char)

;;; モードラインに時刻を表示する
(display-time)

;;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; 履歴をたくさん保存する
(setq history-length 1000)

;;; メニューバーとツールバーとスクロールバーを消す

(if window-system (progn
                    ;; ←GUI用設定を、ここに記述
                    (menu-bar-mode -1)
                    (tool-bar-mode -1)
                    (scroll-bar-mode -1)
                    )
  )

;; C-x C-c で停止しない
(global-set-key (kbd "C-x C-c") 'smex)

;; I never use C-x C-c
;; exit で抜けられます
(defalias 'exit 'save-buffers-kill-emacs)

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)


(global-set-key (kbd "C-t") 'other-window)

(global-set-key (kbd "C-M-n") 'switch-to-next-buffer)
(global-set-key (kbd "C-M-p") 'switch-to-prev-buffer)


(global-set-key [f12] 'eval-buffer)


;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)

;; 最近使ったファイルに加えないファイルを
;; 正規表現で指定する
(setq recentf-exclude
      '("/TAGS$" "/var/tmp/"))
;; 
(global-auto-revert-mode 1)


;; 大文字小文字を区別しない
(setq completion-ignore-case t)

(global-set-key (kbd "M-o") 'occur)

;;; ido smex
(use-package ido
  :bind
  (("C-x C-r" . recentf-ido-find-file)
   ("C-x C-f" . ido-find-file)
   ("C-x C-d" . ido-dired)
   ("C-x b" . ido-switch-buffer)
   ("C-x C-b" . ido-switch-buffer)
 ;("M-x" . smex)
   )
  :init
  (recentf-mode 1)
  
  (defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (setq ido-max-window-height 0.75)
  (when (fboundp 'skk-mode)
    (fset 'ido-select-text 'skk-mode))
  )


(use-package smex
  :bind
  (("M-x" . smex))
  :init
  (setq smex-save-file "~/.emacs.d/cache/.smex-items")
  :config
  (smex-initialize)
  )


;; ido-vertical
;;; このときidoが使うwindowの高さは大きくした方がいい
(setq ido-max-window-height 0.75)
;;; あいまいマッチは入れておこう
(setq ido-enable-flex-matching t)
(ido-mode 1)
(ido-vertical-mode 1)
;;; [2015-07-07 Tue]new: C-n/C-pで選択
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
;;; 他の選択肢: ↑と↓でも選択できるようにする
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
;;; ←と→で履歴も辿れるようにする
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;;----------------------
;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))


;; undo-tree
;; undo-treeモードの設定
(require 'undo-tree)
(global-set-key (kbd "C-c u") 'undo-tree-visualize)

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)


;; projectile
(require 'projectile)
(projectile-mode +1)

(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-c C-g") 'projectile-ag)

(setq projectile-project-search-path '("~/sources/repos/"))

