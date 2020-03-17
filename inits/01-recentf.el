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

;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)

(recentf-mode 1)
