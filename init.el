(eval-when-compile
  (let ((minver "26.0"))
    (when (version< emacs-version minver)
      (error "Your Emacs don't support this config, use Emacs %s or above" minver))))

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

;;; ログはエラーが出た時のみ
(defvar display-warning-minimum-level :error)

;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-refresh-contents)

;;; symlinkは必ず追いかける
(setq vc-follow-symlinks t)

(use-package init-loader
  :straight t
  :config
  (init-loader-load "~/.emacs.d/inits"))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :init   (setenv "SHELL" "/opt/local/bin/zsh" "/usr/local/bin/zsh")
  :config
  (exec-path-from-shell-initialize))

(use-package hydra
  :straight t)

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

;; (use-package aggressive-indent
;;   :straight t
;;   :config (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))


;; Or if you use use-package
;; (use-package dashboard
;;   :straight t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-items '((projects . 5)
;;                           (recents  . 20)))
;;   (defun open-dashboard ()
;;     "Open the *dashboard* buffer and jump to the first widget."
;;     (interactive)
;;     (delete-other-windows)
;;     ;; Refresh dashboard buffer
;;     (if (get-buffer dashboard-buffer-name)
;;         (kill-buffer dashboard-buffer-name))
;;     (dashboard-insert-startupify-lists)
;;     (switch-to-buffer dashboard-buffer-name)
;;     ;; Jump to the first section
;;     (goto-char (point-min))
;;     (dashboard-goto-recent-files))

;;   :bind
;;   ("<f9>" . 'open-dashboard)
;;   )

;(setq initail-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; ido-uniquitous
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
;; (defvar ido-cur-item nil)
;; (defvar ido-default-item nil)
;; (defvar ido-cur-list nil)

;; ido-ghq
(require 'ido-ghq)
(setq ido-ghq-short-list t)
(bind-key "M-p" 'ido-ghq-open)

(use-package company
  :diminish (company-mode)
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

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode typescript-mode))
  )

;; (use-package direnv
;;  :straight t
;;  :config
;;  (direnv-mode))

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

 ;; ビープ音禁止
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
  (setq indent-guide-delay 0.5))

(set-face-attribute 'default nil :height 100)

;(setq ido-enable-flex-matching t)

(use-package powerline
  :straight t
  :config (powerline-default-theme))

(use-package terraform-mode
  :straight t
  :config
  (custom-set-variables
 '(terraform-indent-level 4)))



(setq ns-pop-up-frames nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:ask-p nil)
 '(package-selvc-mergeected-packages
  (quote
    (highlight-symbol exec-path-from-shell pip-requirements bundler solidity-mode smartparens smartparens-config smart-parens inf-ruby rspec-mode ruby-hash-syntax ibuffer-projectile shader-mode web-mode glsl-mode yaml-mode back-button omnisharp mwim zop-to-char dashboard editorconfig smart-jump ag typescript-mode flycheck-elixir alchemist elixir-mode avy ido-ubiquitous projectile company migemo ido-vertical-mode package-utils use-package undohist smex powerline magit-stgit magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


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
  :diminish (anzu-mode)
  :init
  (anzu-mode +1)
  )

;; migemo
(use-package migemo
  :if (executable-find "cmigemo")

  :init
  (load-library "migemo")

  :config
  (setq migemo-command "cmigemo")
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
  :init
  (yas-global-mode 1)
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


(use-package smart-jump
  :straight t
  :config

  :bind
  ("C-." . smart-jump-go)
  ("C-," . smart-jump-back)
  ("C->" . smart-jump-peek))

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

  ;; pg up
  ("M-<prior>" . iflipb-previous-buffer)
  ;; pg down
  ("M-<next>" . iflipb-next-buffer)
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


(setq default-frame-alist
      (append (list
              '(font . "Consolas"))
              default-frame-alist))

;; font-size　ASCII
(set-face-attribute 'default nil :height 120)


; japanese-jisx0208 fonts
(set-fontset-font nil
                  'japanese-jisx0208
                  (font-spec :family "Noto Sans Mono CJK JP Bold"))

;;; ファイルを開いた位置を保存する
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

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

;;; メニューバーとツールバーとスクロールバーを消す
(if window-system (progn
                    ;; ←GUI用設定を、ここに記述
                    (menu-bar-mode -1)
                    (tool-bar-mode -1)
                    (scroll-bar-mode -1)
                    (bind-key "C-x C-c" 'counsel-M-x)
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

(use-package goto-line-preview
  :straight t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview)
)

(use-package flow-minor-mode
  :straight t)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-flow))
(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)


(setq set-mark-command-repeat-pop t)

(use-package goto-chg
  :straight t
  :bind
  ("<f7>" . 'goto-last-change)
  ("<f8>" . 'goto-last-change-reverse))

 (use-package ido-vertical-mode
  :straight t
  :after ido
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-max-window-height 0.75)
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
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'hybrid))


(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode 1)
  (setq counsel-projectile-sort-files t) ;; 当該プロジェクト内リストをソート
  (setq counsel-projectile-sort-projects t) ;; プロジェクトリストをソート
  :after (projectile))


(use-package ivy-hydra
  :straight t)

(use-package origami
  :straight t
  :config
  (global-origami-mode 1)
  (defhydra hydra-folding (:color red)
    "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes))

(bind-key "<tab>" 'hydra-folding/body))

(use-package avy
  :config
  (defhydra hydra-avy (:exit t :hint nil)
    "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char
 [_m_] move   [_M_] move      [_w_] word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("c" avy-goto-char)
    ("w" avy-goto-word-1)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region))

  :bind
  ("C-]" . hydra-avy/body)
  ("C-l" . avy-goto-line)
  :straight t)

(defhydra hydra-projectile nil
  "Projectile"
  ("f"   counsel-projectile-find-file         "Find File" :exit t)
  ("A"   projectile-ag                "ag" :exit t)
  ("a"   counsel-projectile-ag                "ag in counsel" :exit t)
  ("r"   projectile-recentf                  "Recent Files" :exit t)
  ("d"   counsel-projectile-find-dir                 "Find Directory" :exit t)
  ("b"   counsel-projectile-switch-to-buffer         "Switch to Buffer")
  ("c" counsel-compile "compile-project" :exit t)
  ("s"   projectile-switch-project           "Switch Project" :exit t)
  ("l"   persp-switch           "Switch Project" :exit t)
  ("k"   projectile-kill-buffers             "Kill Buffers" :exit t))

;; elscreen
(use-package elscreen
  :straight t
  :init
  (elscreen-start))

;; flycheck
(use-package flycheck
  :straight t
  :config

  (global-flycheck-mode 1)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  )

(use-package flycheck-color-mode-line
  :straight t
  :config
  (flycheck-color-mode-line-mode 1))

(use-package flycheck-pos-tip
  :straight t
  :config
  (flycheck-pos-tip-mode 1))

(use-package flyspell
  :straight t
  :config
  (when (string-equal system-type "darwin") ; There is no problem on Linux
    ;; Dictionary file name
    (setenv "DICTIONARY" "en_US"))
  (when (eq system-type 'windows-nt) ; Windows
    (add-to-list 'exec-path "~/.emacs.d/hunspell/bin"))
  (setq ispell-program-name "hunspell")
  (setq exec-path (parse-colon-path (getenv "PATH")))
  (setq exec-path (parse-colon-path (getenv "DICTIONARY")))
  (setq exec-path (parse-colon-path (getenv "DICPATH")))

  (setq exec-path (parse-colon-path (getenv "PATH")))
  (setq eshell-path-env (getenv "DICTIONARY"))
  (setq eshell-path-env (getenv "DICPATH"))
  (setq flyspell-mode 1))

(use-package flyspell-correct
  :straight t)

(use-package flyspell-correct-ivy
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (require 'flyspell-correct-ivy)
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; javascript
(eval-after-load 'js-mode
  '(add-hook 'js-mode-check #'add-node-modules-path))
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(eval-after-load 'typescript-mode
  '(add-hook 'typescript-mode-check #'add-node-modules-path))


(use-package git-gutter
  :straight t
  :diminish (git-gutter-mode)
  :custom
  (global-git-gutter-mode +1)

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

;; git-gutterのhydra定義
(defhydra hydra-git-gutter nil
  "git hunk"
  ("p" git-gutter:previous-hunk "previous" :exit nil)
  ("n" git-gutter:next-hunk "next" :exit nil)
  ("s" git-gutter:stage-hunk "stage")

  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" magit-log-buffer-file "log")
  ("L" magit-log "log")
  ("r" git-gutter:revert-hunk "revert")

  ("m" magit-status "magit-status" :exit t)
  ("i" git-gutter:popup-hunk)
  ("d" magit-status-here "status-here" :exit t)
  ("b" magit-blame-addition "blame" :exit t)
  ("P" magit-push "push" :exit t)
  ("c" magit-commit-create "commit" :exit t)
  ("x" magit-dispatch "dispatch" :exit t)
  ("t" git-timemachine "time-machine" :exit t)
  ; このタイミングでpopup-hunkをクリアしたい
  ("q" nil :color blue))

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
  ("h" windmove-left "move-left")
  ("l" windmove-right "move-right")
  ("k" windmove-up "move-up")
  ("j" windmove-down "move-down")
  ("c" elscreen-create "screen-create")
  ("n" elscreen-next "screen-next")
  ("p" elscreen-previous "screen-prev")
  ("x" elscreen-kill "screen-kill")
  )

; terminal にはtmuxがあるので使わない
(if window-system
  (progn (bind-key* "C-q" 'hydra-buffer-split/body))
  (bind-key* "C-q" nil)
)

;; hydra flcheck 操作
(defhydra hydra-flycheck nil
  "hydra-flycheck"
  ("j" flycheck-next-error     "next-error")
  ("k" flycheck-previous-error "prev-error")
  ("h" flycheck-first-error    "first-error")
  ("l" (lambda ()
        (interactive)
        (flycheck-list-errors)
        (windmove-right)) "list-errors" :exit t)
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil "exit" :color blue))

(bind-key "C-;" 'hydra-flycheck/body)

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



;; 2画面ファイラ
(setq dired-dwim-target t)

;; wdired
(require 'wdired)

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
  (setq csv-separators '("," ";" "|" " "))
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

(use-package inflections
  :straight t)

(use-package rake
  :straight t)

(use-package projectile-rails
  :straight t
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'hydra-projectile-rails/body)
)

(require 'rbenv)
(global-rbenv-mode)
(setq rbenv-installation-dir "~/.rbenv")

(setq ruby-insert-encoding-magic-comment nil)
(custom-set-variables '(ruby-insert-encoding-magic-comment nil))

(use-package which-key
  :straight t
  :diminish (which-key-mode)
  :config (which-key-mode))

(use-package paredit
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
  :straight t
  :after go-mode)

(use-package real-auto-save
  :straight t
  :diminish (real-auto-save-mode)
  :config
  (setq real-auto-save-interval 30)        ;30秒後に自動保存
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
(defhydra hydra-breadcrumb nil
  "
Breadcrumb bookmarks:
  _p_:   prev   _M-p_:   local prev
  _n_: next   _M-n_: local next
  _s_: set  _c_: clear  _l_: list
"
  ("n" bc-next nil :exit nil)
  ("p" bc-previous nil :exit nil)
  ("M-n" bc-local-next nil :exit nil)
  ("M-p" bc-local-previous nil :exit nil)
  ("l" bc-list nil)
  ("s" bc-set nil)
  ("c" bc-clear nil))

(bind-key "C-x o" 'hydra-breadcrumb/body)

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
  (require 'ivy-hydra)
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

(require 'emacs-surround)
(bind-key* "M-s" 'emacs-surround)

(use-package prescient
  :straight t)

(use-package ivy-prescient
  :straight t
  :config
  (ivy-prescient-mode 1))

(use-package company-prescient
  :straight t
  :config
  (company-prescient-mode 1))

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :straight t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
  'minibuffer-complete-word
  'self-insert-command
  minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(use-package lsp-mode
  :straight t
  :custom
  (lsp-auto-guess-root t)
  (lsp-response-timeout 5)
  (lsp-document-sync-method 'incremental)
  ;; document
  (lsp-ui-doc-use-childframe t)

  (lsp-ui-flycheck-enable 1)

  (lsp-ui-sideline-enable 1)

  :config
  (require 'lsp-clients)
  :bind
  ("C-c l" . 'hydra-lsp/body)

  :commands (lsp))


(use-package lsp-ui
  :after lsp-mode
  :straight t
  :custom
  (scroll-margin 0)
  (lsp-ui-doc-position 'top)
  :hook   (lsp-mode . lsp-ui-mode))

(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))




;; (use-package ddskk
;;   :straight t)
;; (setq skk-jisyo-code 'utf-8)
;; (global-set-key (kbd "C-x C-j") 'skk-mode)
;; (setq skk-server-prog "google-ime-skk") ; google-ime-skkの場所
;; (setq skk-server-inhibit-startup-server nil) ; 辞書サーバが起動していなかったときに Emacs からプロセスを立ち上げる
;; (setq skk-server-host "localhost") ; サーバー機能を利用
;; (setq skk-server-portnum 55100)     ; ポートはgoogle-ime-skk
;; (setq skk-share-private-jisyo t)   ; 複数 skk 辞書を共有

(use-package restclient
  :straight t)

;; CTRL とホイールで拡大縮小をやめたい
(global-set-key (kbd "<C-wheel-down>") nil)
(global-set-key (kbd "<C-wheel-up>") nil)

;; *scratch* バッファの初期メッセージを消す
;(setq initial-scratch-message "")

;; 行頭の kill-line (C-k) で行ごと削除
(setq kill-whole-line t)


;;;--------------- org-mode ---------------------
(setq system-time-locale "C")
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files
      '("~/org/notes.org" "~/org/calendar.org"))

(setq org-refile-targets
      '(("~/org/notes.org" :level . 2 )
        ("~/org/calendar.org" :level . 2 )))

(use-package org-journal
  :straight t
  :custom
  (org-journal-date-format "%x")
  (org-journal-time-format "<%Y-%m-%d %R> ")
  (org-journal-file-format "journal.org")
  (org-journal-dir "~/org/journal/"))

(setq org-capture-templates
      `(
        ("t"
         "task"
         entry
         (file+headline "~/org/tasks.org" "TASK")
         "** TODO %?\n   Entered on %U    %i\n"
         :empty-lines 1)))
(setq org-agenda-files (list "~/org/"))
(setq org-default-notes-file "~/org/notes.org")
(setq org-return-follows-link t)
(define-key global-map "\C-cc" 'org-capture)

(bind-key* "C-x d" 'dired-jump)
(bind-key* "C-x b" 'switch-to-buffer)
(bind-key* "C-x C-b" 'switch-to-buffer)

(use-package persp-mode
  :straight t
  :config
  (persp-mode))

(setq persp-add-on-switch-or-display t)

(with-eval-after-load "persp-mode"
    (defvar persp-mode-projectile-bridge-before-switch-selected-window-buffer nil)

    ;; (setq persp-add-buffer-on-find-file 'if-not-autopersp)

    (persp-def-auto-persp "projectile"
      :parameters '((dont-save-to-file . t)
                    (persp-mode-projectile-bridge . t))
      :hooks '(projectile-before-switch-project-hook
               projectile-after-switch-project-hook
               projectile-find-file-hook
               find-file-hook)
      :dyn-env '((after-switch-to-buffer-adv-suspend t))
      :switch 'frame
      :predicate
      #'(lambda (buffer &optional state)
          (if (eq 'projectile-before-switch-project-hook
                  (alist-get 'hook state))
              state
            (and
             projectile-mode
             (buffer-live-p buffer)
             (buffer-file-name buffer)
             ;; (not git-commit-mode)
             (projectile-project-p)
             (or state t))))
      :get-name
      #'(lambda (state)
          (if (eq 'projectile-before-switch-project-hook
                  (alist-get 'hook state))
              state
            (push (cons 'persp-name
                        (concat "p) "
                                (with-current-buffer (alist-get 'buffer state)
                                  (projectile-project-name))))
                  state)
            state))
      :on-match
      #'(lambda (state)
          (let ((hook (alist-get 'hook state))
                (persp (alist-get 'persp state))
                (buffer (alist-get 'buffer state)))
            (case hook
              (projectile-before-switch-project-hook
               (let ((win (if (minibuffer-window-active-p (selected-window))
                              (minibuffer-selected-window)
                            (selected-window))))
                 (when (window-live-p win)
                   (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer
                         (window-buffer win)))))

              (projectile-after-switch-project-hook
               (when (buffer-live-p
                      persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                 (let ((win (selected-window)))
                   (unless (eq (window-buffer win)
                               persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                     (set-window-buffer
                      win persp-mode-projectile-bridge-before-switch-selected-window-buffer)))))

              (find-file-hook
               (setcdr (assq :switch state) nil)))
            (if (case hook
                  (projectile-before-switch-project-hook nil)
                  (t t))
                (persp--auto-persp-default-on-match state)
              (setcdr (assq :after-match state) nil)))
          state)
      :after-match
      #'(lambda (state)
          (when (eq 'find-file-hook (alist-get 'hook state))
            (run-at-time 0.5 nil
                         #'(lambda (buf persp)
                             (when (and (eq persp (get-current-persp))
                                        (not (eq buf (window-buffer (selected-window)))))
                               ;; (switch-to-buffer buf)
                               (persp-add-buffer buf persp t nil)))
                         (alist-get 'buffer state)
                         (get-current-persp)))
          (persp--auto-persp-default-after-match state)))

    ;; (add-hook 'persp-after-load-state-functions
    ;;           #'(lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)
    )

(bind-key* "C-c p" 'hydra-projectile/body)

(projectile-relevant-known-projects)

(winner-mode)
(bind-key "C-z" 'winner-undo)
(bind-key "C-Z" 'winner-redo)

(use-package git-modes
  :straight t)

(add-to-list 'auto-mode-alist
             (cons "/.dockerignore\\'" 'gitignore-mode))

(use-package git-commit
  :straight t)

(use-package org-pomodoro
  :straight t
  :custom
  (org-pomodoro-length 20)
  (org-pomodoro-manual-break 1))

(bind-key "C-c w" 'hydra-org-clock/body)
(defhydra hydra-org-clock (:color blue :hint nil)
   "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
   ("i" org-clock-in)
   ("o" org-clock-out)
   ("c" org-clock-in-last)
   ("e" org-clock-modify-effort-estimate)
   ("q" org-clock-cancel)
   ("g" org-clock-goto)
   ("d" org-clock-display)
   ("r" org-clock-report)
   ("?" (org-info "Clocking commands")))

(setq org-enforce-todo-dependencies t)

(setq org-log-done 'time)

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(add-to-list 'org-src-lang-modes '("js" . js2))
(add-to-list 'org-src-lang-modes '("json" . json))
