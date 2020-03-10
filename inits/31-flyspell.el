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
