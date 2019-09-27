;; package configuration
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; elisp read config
(add-to-list 'load-path "~/.emacs.d/elisp")

(package-initialize)

;;; ログはエラーが出た時のみ
(setq display-warning-minimum-level :error)

;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3");
(package-refresh-contents)


(defvar favorite-packages
  '(
    ;; package
    use-package

    ; flx-ido
    flx-ido
    ;; ido-mode\
    ido-vertical-mode
    ;; smex
    smex

    ;; mwim
    mwim

    ;; zop-to-char
    zop-to-char

    ;; hydra
    hydra
    ;; cursor position
    saveplace
    ;; git
    magit
    git-gutter
    ;; powerline
    powerline
    ;; company
    company

    ;; editor-config
    editorconfig
    
    ;; yasnippet
    yasnippet
    yasnippet-snippets
    ;; projectile
    projectile
    
    ;; ido-ubiquitous
    ido-ubiquitous

    ;; avy / ace-jump
    avy
    
    ;; javascript / typescript
    typescript-mode
    add-node-modules-path
    
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

    ;; ag
    ag
    ;; egot
    eglot

    ;; flycheck
    flycheck
    flycheck-elixir

    ;; flyspell
    flyspell

    ;; jump
    dumb-jump

    ;; dash-board
    dashboard

    ;; docker-mode
    dockerfile-mode

    ;; omnisharp
    omnisharp

    ;; glsl-mode
    glsl-mode

    ;; web-mode
    web-mode
    )
  )


(dolist (package favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; ido-uniquitous
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)


(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
(setq company-idle-delay 0.8) ; デフォルトは0.5
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

(use-package eglot
  :ensure t
  :bind ("M-r" . 'xref-find-references)
  :hook ((c-mode c++-mode ruby-mode js-mode typescript-mode) . eglot-ensure)
  )

(use-package saveplace
  :ensure t
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


(use-package magit :ensure t)

(set-face-attribute 'default nil :height 100)

(setq ido-enable-flex-matching t)

(use-package powerline
  :ensure t
  :config (powerline-default-theme)
  )




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:ask-p nil)
 '(package-selected-packages
   (quote
    (shader-mode web-mode glsl-mode yaml-mode back-button omnisharp mwim zop-to-char dashboard editorconfig smart-jump ag typescript-mode flycheck-elixir alchemist elixir-mode avy ido-ubiquitous projectile company migemo ido-vertical-mode package-utils use-package undohist smex powerline magit-stgit magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package avy
  :bind
  ("C-]" . avy-goto-char)
  ("C-l" . avy-goto-line)
)

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t)

(use-package flycheck-elixir
  :ensure t)

(setq alchemist-key-command-prefix (kbd "C-c ,"))

;; typescript
(load-theme 'solarized-light t)

;; migemo
(use-package migemo
  :if (executable-find "cmigemo")
  :ensure t
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
)

;; yasnippet
(use-package yasnippet
  :ensure t
  :commands yas-reload-all
  :delight yas-minor-mode
  :hook ((prog-mode). yas-minor-mode)
  :config (yas-reload-all)
  :ensure t
  :bind  
  ("C-j" . company-yasnippet)
)


;; point-undo
(require 'point-undo)
(bind-key "C--" 'point-undo)
(bind-key "C-=" 'point-redo)


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
(bind-key* "C-h" 'delete-backward-char)

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
(if window-system
  (progn (bind-key "C-x C-c" 'smex))
  )


;; I never use C-x C-c
;; exit で抜けられます
(defalias 'exit 'save-buffers-kill-emacs)

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

(bind-key* "C-t" 'other-window)

(bind-key* "[f12]" 'eval-buffer)


;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)

(global-auto-revert-mode 1)

;; 大文字小文字を区別しない
(setq completion-ignore-case t)

(bind-key "M-o" 'occur)

;;; ido smex
(use-package ido
  :bind*
  (("C-x C-r" . recentf-ido-find-file)
   ("C-x C-f" . ido-find-file)
   ("C-c C-d" . ido-dired)
   ("C-x b" . ido-switch-buffer)
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
  (setq ido-enable-flex-matching t)
  (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-max-window-height 0.75)
  (ido-ubiquitous-mode 1)
)


(use-package smex
  :bind
  (("M-x" . smex))
  :init
  (setq smex-save-file "~/.emacs.d/cache/.smex-items")
  :config
  (smex-initialize)
  )



;;----------------------
;; undohistの設定
(use-package undohist
  :config
  (undohist-initialize))

;; undo-tree
;; undo-treeモードの設定
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  :bind
  ("C-c u" . 'undo-tree-visualize)
  ("M-/" . 'undo-tree-redo))

;; projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/sources/repos/"))  
  :config
  (projectile-mode +1)
  :bind* (("C-c C-f" . projectile-find-file)
         ("M-p" . projectile-switch-project)))



(defhydra hydra-projectile (:color teal
			    :columns 4)
  "Projectile"
  ("f"   projectile-find-file                "Find File")
  ("r"   projectile-recentf                  "Recent Files")
  ("z"   projectile-cache-current-file       "Cache Current File")
  ("x"   projectile-remove-known-project     "Remove Known Project")
  
  ("d"   projectile-find-dir                 "Find Directory")
  ("b"   projectile-switch-to-buffer         "Switch to Buffer")
  ("c"   projectile-invalidate-cache         "Clear Cache")
  ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")
  
  ("o"   projectile-multi-occur              "Multi Occur")
  ("s"   projectile-switch-project           "Switch Project")
  ("k"   projectile-kill-buffers             "Kill Buffers")
  ("q"   nil "Cancel" :color blue))

(bind-key "C-c p" 'hydra-projectile/body)
(bind-key "C-c C-p" 'hydra-projectile/body)

;; flycheck
(use-package flycheck
  :ensure t)
(setq flycheck-check-syntax-automatically
      '(save idle-change mode-enabled))

(setq flycheck-idle-change-delay 1)
(add-hook 'after-init-hook #'global-flycheck-mode)

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

(use-package hydra
  :ensure t)

(use-package git-gutter
  :ensure t
  :custom
  (global-git-gutter-mode 1)
  ;; stage, revertで確認を出さないようにする
  ;; (undoでもどせるからいいや、という気持ち)
  (git-gutter:ask-p nil)

  :bind
  ;; hydra-git-gutter起動のキーバインド
  ("C-c g" . hydra-git-gutter/body))

;; git-gutter:popup-hunkをそのまま割り当てるとdiffウィンドウを閉じれないので
;; トグルできる関数を定義
(defun git-gutter:toggle-popup-hunk ()
  "Toggle git-gutter hunk window."
  (interactive)
  (if (window-live-p (git-gutter:popup-buffer-window))
      (delete-window (git-gutter:popup-buffer-window))
      (git-gutter:popup-hunk)))

;; git-gutterのhydra定義
(defhydra hydra-git-gutter nil
  "git hunk"

  ("p" git-gutter:previous-hunk "previous")
  ("n" git-gutter:next-hunk "next")
  ("s" git-gutter:stage-hunk "stage")
  ("r" git-gutter:revert-hunk "revert")
  ("m" magit-status "status")
  ("b" magit-blame "blame")
  ("d" magit-dispatch "dispatch")
  ("SPC" git-gutter:popup-hunk "toggle diffinfo")
  ("q" nil "exit")
  )

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
  ("q" nil "exit" :color blue)
  )

; terminal にはtmuxがあるので使わない
(if window-system
  (progn (bind-key* "C-q" 'hydra-buffer-split/body))
  (bind-key* "C-q" nil)
)

;; hydra flycheck 操作
(defhydra hydra-flycheck nil
  "hydra-flycheck"
  ("j" next-error     "next-error")
  ("k" previous-error "prev-error")
  ("h" first-error    "first-error")
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
  :ensure t
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
      )
)

(add-to-list 'recentf-exclude "ido.last")
;; 最近使ったファイルに加えないファイルを
(add-to-list 'recentf-exclude "smex-items")
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
  :ensure t
  :config
  (smart-jump-setup-default-registers))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode)
)

(bind-key "M-z" 'zop-up-to-char)

(bind-key "C-a" 'mwim-beginning-of-code-or-line)
(bind-key "C-e" 'mwim-end-of-code-or-line)


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
(use-package omnisharp)
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
  :ensure t
  :mode (("\\.effect\\'" . glsl-mode)
         ("\\.fsh\\'" . glsl-mode)
         ("\\.vsh\\'" . glsl-mode)
         ("\\.shader\\'" . glsl-mode)))

(use-package web-mode :ensure t)

(use-package shader-mode :ensure t)
