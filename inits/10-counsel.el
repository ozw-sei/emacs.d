
 (use-package selectrum
   :straight t
   :config
   (selectrum-mode +1))

(use-package prescient
  :straight t
  :after selectrum
  :config
  (prescient-persist-mode +1))

 (use-package selectrum-prescient
   :straight t
   :after prescient
   :config
   (selectrum-prescient-mode +1))

(use-package ctrlf
  :straight t
  :config
  (ctrlf-mode +1))

(use-package selected
  :straight t
  :init
  (selected-minor-mode 1)
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
              ("r" . my-randomize-region)
              ("t" . org-table-convert-region)))

(use-package expand-region
  :straight t)

;; https://www.emacswiki.org/emacs/RandomizeBuffer
(defun my-randomize-region (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
                (delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
           (mapcar 'cdr
                   (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
                         (lambda (a b) (< (car a) (car b))))))))
