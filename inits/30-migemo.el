
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
