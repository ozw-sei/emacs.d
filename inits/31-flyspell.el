(use-package flyspell
  :straight t
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

  (when (eq system-type 'darwin) ; mac
    (setenv "DICTIONARY" "en-US")
    (setenv "DICPATH" "~/.emacs.d/dict")
    (add-to-list 'exec-path "/opt/local/bin")
    (add-to-list 'exec-path "/usr/local/bin"))

  (setq flyspell-mode 1))

(use-package flyspell-correct
  :straight t)

(use-package flyspell-correct-ivy
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (require 'flyspell-correct-ivy)
  (setq flyspell-correct-interface #'flyspell-correct-ivy))
