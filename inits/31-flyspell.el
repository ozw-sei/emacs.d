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
    (setenv "DICTIONARY" "en_US")
    (setenv "DICPATH" (expand-file-name "~/.emacs.d/dict"))
    (add-to-list 'exec-path "/opt/homebrew/bin")
    (add-to-list 'exec-path "/opt/local/bin")
    (add-to-list 'exec-path "/usr/local/bin"))

  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d" "en_US"))

  ;; Enable flyspell in text modes
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Enable flyspell prog mode in programming modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  
  ;; Customize what gets spell-checked in prog-mode
  (setq flyspell-prog-text-faces
        '(font-lock-string-face 
          font-lock-comment-face 
          font-lock-doc-face
          font-lock-comment-delimiter-face))
  
  ;; Custom function to handle camelCase and snake_case
  (defun flyspell-check-word-p ()
    "Don't spell check words that are camelCase or snake_case variables."
    (let ((word (thing-at-point 'word t)))
      (when word
        (not (or
              ;; Skip ALL_CAPS constants
              (string-match-p "^[A-Z_]+$" word)
              ;; Skip camelCase (starts lowercase, has uppercase)
              (string-match-p "^[a-z]+[A-Z]" word)
              ;; Skip snake_case with underscores
              (string-match-p "^[a-z]+_[a-z_]*$" word)
              ;; Skip hex values
              (string-match-p "^0x[0-9a-fA-F]+$" word)
              ;; Skip numbers
              (string-match-p "^[0-9]+$" word))))))
  
  (put 'flyspell-check-word-p 'flyspell-mode-predicate t))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind ("<f7>" . flyspell-correct-wrapper))


(use-package flyspell-correct-popup
  :straight t
  :after flyspell-correct
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;; Enhanced spell checking for code
(use-package jinx
  :straight t
  :hook ((text-mode prog-mode conf-mode) . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config
  ;; Set enchant library path for macOS
  (when (eq system-type 'darwin)
    (setq jinx-module-directory (expand-file-name "~/.emacs.d/jinx-modules/"))
    (setenv "PKG_CONFIG_PATH" 
            (concat "/opt/homebrew/lib/pkgconfig:" 
                    (getenv "PKG_CONFIG_PATH")))
    ;; Set LANG to English to ensure English dictionaries are used
    (setenv "LANG" "en_US.UTF-8"))
  
  ;; Use native spell-checker for better performance
  (setq jinx-languages "en_US")
  ;; Configure to work with camelCase and snake_case
  (add-to-list 'jinx-exclude-regexps '(t "\\b[[:upper:]]+\\b"))  ; Exclude ALL_CAPS
  (add-to-list 'jinx-exclude-regexps '(prog-mode "\\b[a-z]+[A-Z][a-zA-Z]*\\b")) ; camelCase
  (add-to-list 'jinx-exclude-regexps '(prog-mode "\\b[a-z]+_[a-z_]*\\b")))      ; snake_case
