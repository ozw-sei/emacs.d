(use-package flyspell
  :straight t
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#653fb5" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))
  :config
  (when (eq system-type 'windows-nt) ; Windows
    (setq ispell-program-name "hunspell")
    (add-to-list 'exec-path "~/.emacs.d/hunspell/bin")
    (setq exec-path (parse-colon-path (getenv "PATH")))
    (setq exec-path (parse-colon-path (getenv "DICTIONARY")))
    (setq exec-path (parse-colon-path (getenv "DICPATH")))
    (setq exec-path (parse-colon-path (getenv "PATH")))
    (setq eshell-path-env (getenv "DICTIONARY"))
    (setq eshell-path-env (getenv "DICPATH")))

  (setq ispell-local-dictionary "en_US")

  (when (eq system-type 'darwin) ; mac
    (setenv "DICTIONARY" "en-US")
    (setenv "DICPATH" "~/.emacs.d/dict")
    (add-to-list 'exec-path "/opt/local/bin")
    (add-to-list 'exec-path "/usr/local/bin"))

  (setq flyspell-mode 1))

(use-package flyspell-correct
  :straight t

  :after flyspell
  :config
  (global-set-key (kbd "<f7>") 'flyspell-correct-wrapper))

(use-package flyspell-correct-helm
  :straight t
  :after flyspell-correct)
