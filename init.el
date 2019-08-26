;; package configuration
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; elisp read config
(add-to-list 'load-path "~/.emacs.d/elisp")

(setq mac-command-modifier 'control)

(package-initialize)

(package-refresh-contents)

(defvar favorite-packages
  '(
    ;; package
    use-package
    ;; ido-mode
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
    ))

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

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")

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
    (company migemo ido-vertical-mode package-utils use-package undohist smex powerline magit-stgit magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
