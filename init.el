;; Emacs 26 以上のみをサポート
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
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)

;; Native compilation settings (Emacs 28+)
(when (fboundp 'native-compile-async)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))


;; use-packageをstraight.elにフォールバックする
(defvar straight-use-package-by-default t)

;; use-packageで可読性の高いinit.elを書く
;; https://qiita.com/kai2nenobu/items/5dfae3767514584f5220

;; ------- load path ------------------
;; elisp read config (only needed for persp-mode-projectile-bridge)
(add-to-list 'load-path "~/.emacs.d/elisp")

(use-package init-loader
  :straight t
  :config
(init-loader-load "~/.emacs.d/inits"))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:ask-p nil)
 '(package-selected-packages
  (quote
    (highlight-symbol exec-path-from-shell pip-requirements bundler solidity-mode smartparens smartparens-config smart-parens inf-ruby rspec-mode ruby-hash-syntax ibuffer-projectile shader-mode web-mode glsl-mode yaml-mode back-button omnisharp mwim zop-to-char dashboard editorconfig smart-jump ag typescript-mode flycheck-elixir alchemist elixir-mode avy ido-ubiquitous projectile migemo ido-vertical-mode package-utils use-package undohist smex powerline magit-stgit magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;; (use-package ddskk
;;   :straight t)
;; (setq skk-jisyo-code 'utf-8)
;; (global-set-key (kbd "C-x C-j") 'skk-mode)
;; (setq skk-server-prog "google-ime-skk") ; google-ime-skkの場所
;; (setq skk-server-inhibit-startup-server nil) ; 辞書サーバが起動していなかったときに Emacs からプロセスを立ち上げる
;; (setq skk-server-host "localhost") ; サーバー機能を利用
;; (setq skk-server-portnum 55100)     ; ポートはgoogle-ime-skk
;; (setq skk-share-private-jisyo t)   ; 複数 skk 辞書を共有
(put 'set-goal-column 'disabled nil)
