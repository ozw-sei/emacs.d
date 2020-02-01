;; straight.el setting by myself

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)

;; use-packageをstraight.elにフォールバックする
(defvar straight-use-package-by-default t)

;; elisp read config
(add-to-list 'load-path "~/.emacs.d/elisp")

;(package-initialize)

;;; ログはエラーが出た時のみ
(defvar display-warning-minimum-level :error)

;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-refresh-contents)

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :init   (setenv "SHELL" "/usr/local/bin/zsh")
  :config
  (exec-path-from-shell-initialize)
  )

(use-package diminish
  :straight t
  :config
  (progn
    (add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "Lisp")))
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "elisp")))
    (add-hook 'texinfo-mode-hook (lambda () (setq mode-name "texi")))
    (add-hook 'change-log-mode-hook (lambda () (setq mode-name "CL")))
    (diminish 'isearch-mode)))

(use-package magit-lfs
  :straight t)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(use-package aggressive-indent
  :straight t
  :config (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package origami
  :straight t)

;; Or if you use use-package
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '(
                          (recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (delete-other-windows)
    ;; Refresh dashboard buffer
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    ;; Jump to the first section
    (goto-char (point-min))
    (dashboard-goto-recent-files))

  :bind
  ("<f9>" . 'open-dashboard)
  )

(setq initail-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; ido-uniquitous
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

;; ido-ghq
(require 'ido-ghq)
(setq ido-ghq-short-list t)
(bind-key "M-p" 'ido-ghq-open)

(use-package company
  :straight t
  :config

  (global-company-mode) ; 全バッファで有効にする
  (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)

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

(use-package expand-region
  :straight t
  :bind* ("C-c SPC" . er/expand-region))

(use-package typescript-mode
  :straight t)


(use-package eglot
  :straight t
  :bind (
         ("M-r" . 'xref-find-references )
         ("C-." . (lambda ()
                    (interactive)
                    (eglot-help-at-point)
                    (windmove-right)))
         ("C-," . (lambda ()
                    (interactive)
                    (eglot-code-actions)
                    (windmove-right)))
         )
  :hook ((c-mode c++-mode ruby-mode) . eglot-ensure)
  )

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode typescript-mode))
  )

(use-package direnv
 :straight t
 :config
 (direnv-mode))

;; 単語にカーソルを置くと同じ単語をハイライトしてくれる
(use-package highlight-symbol
  :straight t
  :diminish (highlight-symbol-mode)
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 0.7))


(use-package saveplace
  :straight t
  :config
  (save-place-mode 1))

(defun turn-on-flycheck-mode ()
  (flycheck-mode 1))

 ;; ビープ音禁止
 (setq ring-bell-function 'ignore)

;; 自動改行しない
(setq auto-fill-mode 0)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* gc-cons-threshold 10))
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


(use-package magit
  :straight t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
(use-package magit-lfs
  :straight t
  :after magit)
(use-package magit-todos
  :straight t
  :after magit)
(use-package magit-gitflow
  :straight t
  :after magit)

(use-package indent-guide
  :straight t
  :diminish (indent-guide-mode)
  :config
  (indent-guide-global-mode)
                                        ; for performance
  (setq indent-guide-delay 0.5)
  )

(use-package forge
  :straight t
  :after magit)


(set-face-attribute 'default nil :height 100)

(setq ido-enable-flex-matching t)

(use-package powerline
  :straight t
  :config (powerline-default-theme))

(use-package terraform-mode
  :straight t)


(setq ns-pop-up-frames nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:ask-p nil)
 '(package-selected-packages
   (quote
    (highlight-symbol exec-path-from-shell pip-requirements bundler solidity-mode smartparens smartparens-config smart-parens inf-ruby rspec-mode ruby-hash-syntax ibuffer-projectile shader-mode web-mode glsl-mode yaml-mode back-button omnisharp mwim zop-to-char dashboard editorconfig smart-jump ag typescript-mode flycheck-elixir alchemist elixir-mode avy ido-ubiquitous projectile company migemo ido-vertical-mode package-utils use-package undohist smex powerline magit-stgit magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(use-package avy
  :bind
  ("C-]" . avy-goto-char)
  ("C-c l" . avy-goto-line)
  :straight t)

(use-package elixir-mode
  :straight t)

(use-package alchemist
  :straight t)

(use-package flycheck-elixir
  :straight t)

(setq alchemist-key-command-prefix (kbd "C-c ,"))

(use-package solarized-theme
  :straight t
  :config
  (load-theme 'solarized-light t))

                                        ;(use-package monokai-theme
                                        ;:straight t
                                        ;:config
                                        ;(load-theme 'monokai t)
                                        ;)

(use-package anzu
  :straight t
  :init
  (anzu-mode +1)
  )

;; migemo
(use-package migemo
  :if (executable-find "/usr/local/bin/cmigemo")

  :init
  (load-library "migemo")

  :config
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-regex-dictionary nil)
  (migemo-init)
  (setq anzu-use-migemo t)
  :straight t
  )

;; yasnippet
(use-package yasnippet
  :straight t
  :commands yas-reload-all
  :delight yas-minor-mode
  :hook ((prog-mode). yas-minor-mode)
  :config (yas-reload-all)
  :diminish yas-minor-mode
  :bind
  ("C-j" . company-yasnippet)
  )

(use-package yasnippet-snippets
  :straight t)

;; point-undo
(require 'point-undo)
(bind-key "C--" 'point-undo)
(bind-key "C-=" 'point-redo)

(use-package iflipb
  :straight t
  :config
  (setq iflipb-ignore-buffers (list "^[*]" "^magit" "]$"))
  (setq iflipb-wrap-around t)
  :bind
  ("M-[" . iflipb-previous-buffer)
  ("M-]" . iflipb-next-buffer)
)

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

;; M-x woman
(use-package woman
  :config (setq woman-cache-filename (expand-file-name "~/.emacs.d/woman-cache")))

;; font-size
(set-face-attribute 'default nil :height 140)
(set-default-font "Consolas")

(setq default-frame-alist
      (append (list
               '(font . "Consolas"))
              default-frame-alist))


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

;;; シェルに合わせるため、C-hはに割り当てる
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
;;(global-set-key "\C-h" 'delete-backward-char)

;;; モードラインに時刻を表示する
(display-time)

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 30 gc-cons-threshold))

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
                    (bind-key "C-x C-c" 'smex)
                    )
  )
;; I never use C-x C-c
;; exit で抜けられます
(defalias 'exit 'save-buffers-kill-emacs)

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)

(global-auto-revert-mode 1)

;; 大文字小文字を区別しない
(setq completion-ignore-case t)

(bind-key "M-o" 'occur)

(use-package goto-line-preview
  :straight t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview)
)

;;; ido smex
(use-package ido
  :bind*
  (("C-x C-r" . recentf-ido-find-file)
   ("C-x C-f" . ido-find-file)
   ("C-c d" . ido-dired)
   ("C-x b" . ido-switch-buffer)
   ("C-x d" . dired)
   ("C-x C-b" . ido-switch-buffer)
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
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t)
  (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
  (setq ido-max-window-height 0.75)
  )

                                        ; Org-captureを呼び出すキーシーケンス
(define-key global-map "\C-cc" 'org-capture)
                                        ; Org-captureのテンプレート（メニュー）の設定
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %?\nEntered on %U\n %i\n %a")
        ))

(use-package flow-minor-mode
  :straight t)

(add-hook 'js2-mode-hook 'flow-minor-mode)
(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-flow))


(setq set-mark-command-repeat-pop t)

(use-package goto-chg
  :straight t)
(global-set-key (kbd "<f7>") 'goto-last-change)
(global-set-key (kbd "<f8>") 'goto-last-change-reverse)

(use-package ido-vertical-mode
  :straight t
  :after ido
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-max-window-height 0.75)
  )

(use-package smex
  :straight t
  :bind
  (("M-x" . smex))
  :config
  (setq smex-save-file "~/.emacs.d/cache/.smex-items")
  (smex-initialize)
  )

;;----------------------
;; undohistの設定
(use-package undohist
  :straight t
  :config
  (undohist-initialize))

;; undo-tree
;; undo-treeモードの設定
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  :diminish undo-tree-mode
  :bind
  ("C-c u" . 'undo-tree-visualize))

;; projectile
(use-package projectile
  :straight t
  :diminish (projectile-mode)
  :config
  (projectile-mode +1)
  :bind
  ("C-c C-f" . 'projectile-find-file)
  ("C-c f" . 'projectile-find-file)
  )

(use-package hydra
  :straight t)

(defhydra hydra-projectile (:color teal
			           :columns 4)
  "Projectile"
  ("f"   projectile-find-file                "Find File")
  ("a"   projectile-ag                "ag" :exit t)
  ("r"   projectile-recentf                  "Recent Files")
  ("z"   projectile-cache-current-file       "Cache Current File")
  ("x"   projectile-remove-known-project     "Remove Known Project")

  ("d"   projectile-find-dir                 "Find Directory")
  ("b"   projectile-switch-to-buffer         "Switch to Buffer")
  ("c"   projectile-invalidate-cache         "Clear Cache")
  ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")

  ("s"   projectile-switch-project           "Switch Project")
  ("k"   projectile-kill-buffers             "Kill Buffers"))

(bind-key "C-c p" 'hydra-projectile/body)
(bind-key "C-c C-p" 'hydra-projectile/body)

;; elscreen
(use-package elscreen
  :straight t
  :init
  (elscreen-start))

;; flycheck
(use-package flycheck
  :straight t
  :config
  (setq flycheck-check-syntax-automatically
        '(save idle-change mode-enabled))

  (setq flycheck-idle-change-delay 1)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

;; javascript
(eval-after-load 'js-mode
  '(add-hook 'js-mode-check #'add-node-modules-path))
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(eval-after-load 'typescript-mode
  '(add-hook 'typescript-mode-check #'add-node-modules-path))

;; flyspell
(add-hook 'prog-mode-hook 'flyspell-mode)

;; ispell の後継である aspell を使う。
;; CamelCase でもいい感じに spellcheck してくれる設定を追加
;; See: https://stackoverflow.com/a/24878128/8888451
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
(setq ispell-program-name "aspell"
      ispell-extra-args
      '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=5" "--run-together-min=2"))

(use-package git-gutter
  :straight t
  :diminish (git-gutter-mode)
  :custom
  (global-git-gutter-mode +1)
  ;; stage, revertで確認を出さないようにする
  ;; (undoでもどせるからいいや、という気持ち)
  (git-gutter:ask-p nil)

  :bind
  ;; hydra-git-gutter起動のキーバインド
  ("C-c g" . hydra-git-gutter/body))

(use-package quickrun
  :straight t
  :bind
  ("<f5>" . quickrun)
  )

(use-package git-timemachine
  :straight t)

(use-package js2-refactor
  :straight t
  :config
  (js2r-add-keybindings-with-prefix "C-c m"))

(use-package prettier-js
  :straight t)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

;; git-gutter:popup-hunkをそのまま割り当てるとdiffウィンドウを閉じれないので
;; トグルできる関数を定義
(defun git-gutter:toggle-popup-hunk ()
  "Toggle 'git-gutter' hunk window."
  (interactive)
  (git-gutter:popup-hunk)
  (other-window 1)
  )

;; git-gutterのhydra定義
(defhydra hydra-git-gutter nil
  "git hunk"
  ("p" git-gutter:previous-hunk "previous" :exit nil)
  ("n" git-gutter:next-hunk "next" :exit nil)
  ("s" git-gutter:stage-hunk "stage")
  ("r" git-gutter:revert-hunk "revert")
  ("m" magit-status "magit-status" :exit t)
  ("d" magit-status-here "status-here" :exit t)
  ("c" magit-commit-create "commit" :exit t)
  ("b" magit-blame-addition "blame" :exit t)
  ("P" magit-push "push" :exit t)
  ("x" magit-dispatch "dispatch" :exit t)
  ("t" git-timemachine "time-machine" :exit t)
  ("SPC" git-gutter:toggle-popup-hunk "toggle diffinfo" :exit nil))

;; hydra window 操作
(defhydra hydra-buffer-split nil
  "hydra-buffer-split"
  ("s" (lambda ()
         (interactive)
         (split-window-vertically)
         (windmove-down)) "split-horizontally")
  ("v" (lambda()
         (interactive)
         (split-window-horizontally)
         (windmove-right)) "split-vertically")
  ("C-k" delete-window "delete")
  ("w" enlarge-window-horizontally "enrage-horizontally")
  ("W" shrink-window-horizontally "shrink-horizontally")
  ("t" enlarge-window "enrage-vertically")
  ("T" shrink-window "shrink-vertically")
  ("b" balance-windows "balance")
  ("C-M-r" delete-other-windows "reset-window")
  ("h" windmove-left "move-left")
  ("j" windmove-down "move-down")
  ("k" windmove-up "move-up")
  ("l" windmove-right "move-right")
  )

; terminal にはtmuxがあるので使わない
(if window-system
  (progn (bind-key* "C-q" 'hydra-buffer-split/body))
  (bind-key* "C-q" nil)
)

;; hydra flycheck 操作
(defhydra hydra-flycheck nil
  "hydra-flycheck"
  ("j" flymake-goto-next-error     "next-error")
  ("k" flymake-goto-previous-error "prev-error")
  ("h" flymake-goto-first-error    "first-error")
  ("l" (condition-case err
           (while t
             (next-error))
         (user-error nil))
   nil :bind nil)
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil "exit" :color blue))

(bind-key "C-'" 'hydra-flycheck/body)

;; editor-config
(use-package editorconfig
  :straight t
  :diminish (editorconfig-mode)
  :config
  (editorconfig-mode 1))

;; 最近使ったファイルに加えないファイルを
;; 正規表現で指定する
(setq recentf-exclude
      '("/TAGS$"
        "/var/tmp/"
        "ido.last"
        "smex-items"
        "COMMIT_EDITMSG"
        "./straight/"
        "persp-confs"
        ".breadcrumb"
        "*.sqlite"
        "./server/"
        )
      )

(add-to-list 'recentf-exclude "ido.last")
;; 最近使ったファイルに加えないファイルをg
(add-to-list 'recentf-exclude "smex-items")
(add-to-list 'recentf-exclude "woman-cache")
;; 正規表現で指定する
(add-to-list 'recentf-exclude "COMMIT_EDITMSG")

(add-to-list 'recentf-exclude
             (expand-file-name "~/.emacs.d/elsp/*"))
(add-to-list 'recentf-exclude
             (expand-file-name "~/.emacs.d/elpa/*"))
(add-to-list 'recentf-exclude
             (expand-file-name "~/.emacs.d/cache/*"))

(add-to-list 'recentf-exclude
             (expand-file-name "/usr/local/Cellar/*"))

(setq recentf-max-saved-items 2000)

(bind-key "M-g" 'goto-line)

(use-package smart-jump
  :straight t
  :config
  (smart-jump-setup-default-registers))

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode)
)

(use-package zop-to-char
  :straight t
  :bind
  ("M-z" . 'zop-up-to-char))


(use-package mwim
  :straight t
  :bind
  ("C-a" . 'mwim-beginning-of-code-or-line)
  ("C-e" . 'mwim-end-of-code-or-line))



(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           open other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)


(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
  ""
  ("h" backward-char "left")
  ("l" forward-char "right")
  ("k" previous-line "up")
  ("j" next-line "down")
  ("e" exchange-point-and-mark "exchange")
  ("n" copy-rectangle-as-kill "copy-rectangle")
  ("d" delete-rectangle "delete")
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle "yank")
  ("u" undo "undo")
  ("s" string-rectangle "string-rectangle")
  ("p" kill-rectangle "kill-rectangle")
  ("q" nil "exit")
  )

(bind-key "C-x SPC" 'hydra-rectangle/body)

; csharp
(use-package omnisharp
  :straight t)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)
(add-hook 'csharp-mode-hook #'company-mode)
(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

                                        ;csharp-mode README.md recommends this too
                                        ;(electric-pair-mode 1)       ;; Emacs 24
                                        ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'mnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")

(use-package glsl-mode
  :straight t
  :mode (("\\.effect\\'" . glsl-mode)
         ("\\.fsh\\'" . glsl-mode)
         ("\\.vsh\\'" . glsl-mode)
         ("\\.shader\\'" . glsl-mode)))

(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
					;(add-to-list 'auto-mode-alist '("\\.js$" . web-mode)) ;
					;(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
					;(add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
					;(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
					;(setq web-mode-content-types-alist
					;'(("jsx" . "\\.js[x]?\\'")))
  )

(use-package csv-mode
  :straight t
  :config
  (setq indent-tabs-mode t))


;; 正規表現検索をビジュアル的に
(use-package visual-regexp
  :straight t)

(use-package shader-mode :straight t)

;;; Ruby-mode
(use-package ruby-mode
  :straight t
  :mode (
         ("\\Vagrantfile$" . ruby-mode)

         ("\\.rb$" . ruby-mode))
  )


;; json-mode
(use-package json-mode
  :straight t)

(use-package ruby-hash-syntax
  :straight t)

(setq-default
 ruby-use-encoding-map nil
 ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook 'subword-mode)


(use-package rspec-mode
  :straight t)

(use-package inf-ruby
  :straight t)

(use-package bundler
  :straight t)

(use-package ruby-electric
  :straight t)

                                        ;(use-package projectile-rails
                                        ;:straight t
                                        ;:after projectile)

(require 'rbenv)
(global-rbenv-mode)
(setq rbenv-installation-dir "~/.rbenv")

(setq ruby-insert-encoding-magic-comment nil)
(custom-set-variables '(ruby-insert-encoding-magic-comment nil))

(use-package which-key
  :straight t
  :diminish (which-key-mode)
  :config (which-key-mode))

(use-package smartparens
  :straight t)

(use-package pip-requirements
  :straight t)

(use-package solidity-mode
  :straight t)


(use-package ag
  :straight t
  :config
  (setq ag-executable "ag")
  (setq ag-arguments (list "--path-to-ignore" "--skip-vcs-ignores")))

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

(smartparens-mode 1)

(setq make-backup-files nil)

(use-package viewer
  :straight t)


(defun other-window-or-split ()
  "If there is one window, open split window.
If there are two or more windows, it will go to another window."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(bind-key* "C-t" 'other-window-or-split)

(use-package zoom
  :straight t
  :diminish (zoom-mode)
  :config
  (custom-set-variables
   '(zoom-mode t))
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  )

(use-package go-mode
  :straight t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config

  (setq exec-path (parse-colon-path (getenv "GOROOT")))
  (setq exec-path (parse-colon-path (getenv "GOPATH")))
  (setq exec-path (parse-colon-path (getenv "PATH")))

  (setq eshell-path-env (getenv "GOPATH"))
  (setq eshell-path-env (getenv "PATH"))
  (setq eshell-path-env (getenv "GOROOT"))
  :bind
  ("M-." . 'godef-jump)
  ("M-," . 'pop-tag-mark)
  )

(use-package company-go
  :straight t
  :config
  (custom-set-variables
   '(company-go-insert-arguments nil))
  :after (go-mode company)
  )

(use-package go-direx
  :straight t)

(use-package real-auto-save
  :straight t
  :diminish (real-auto-save-mode)
  :config
  (setq real-auto-save-interval 3)        ;3秒後に自動保存
  (add-hook 'find-file-hook 'real-auto-save-mode))

(use-package unicode-whitespace
  :requires (list-utils
	     ucs-utils
	     unicode-whitespace
	     persistent-soft)
  :config
  (whitespace-mode 1)
  (unicode-whitespace-setup)
  )

(use-package whitespace
  :straight t
  :config
  (progn
    (add-hook 'ruby-mode-hook (lambda () (whitespace-mode 1)))
    (add-hook 'c-mode-common-hook (lambda () (whitespace-mode 1)))
    (setq whitespace-style '(face
                             trailing
                             tabs
                             spaces
                             lines-tail
                             newline
                             empty
                             indentation
                             space-after-tab
                             space-before-tab
                             space-mark
                             tab-mark
                             ))
    (set-face-background 'whitespace-newline 'nil)
    (set-face-background 'whitespace-space 'nil)
    (set-face-foreground 'whitespace-space "RGB:44/44/44")
    )
  )


(require 'breadcrumb)
(defhydra hydra-breadcrumb
  (:exit t)
  "
Breadcrumb bookmarks:
  _<up>_:   prev   _S-<up>_:   local prev
  _<down>_: next   _S-<down>_: local next
  _s_: set  _c_: clear  _l_: list  _q_: quit
"
  ("<down>" bc-next nil :exit nil)
  ("<up>" bc-previous nil :exit nil)
  ("S-<down>" bc-local-next nil :exit nil)
  ("S-<up>" bc-local-previous nil :exit nil)
  ("l" bc-list nil)
  ("s" bc-set nil)
  ("c" bc-clear nil)
  ("q" nil nil))

(bind-key "C-x o" 'hydra-breadcrumb/body)

(require 'browse-kill-ring)
(bind-key "M-y" 'browse-kill-ring)

(bind-key
 [f2]
 (defhydra hydra-compile (:color red :hint nil)
   "
    Compile: make _k_  _a_ll  _u_pftp  _m_ove  _b_klog  _g_it  _c_lean   🐾 "
   ("k" my:make-k :exit t)
   ("a" my:make-all :exit t)
   ("u" my:make-upftp :exit t)
   ("m" my:make-move :exit t)
   ("g" my:make-git :exit t)
   ("b" my:make-bklog :exit t)
   ("c" my:make-clean)))2
