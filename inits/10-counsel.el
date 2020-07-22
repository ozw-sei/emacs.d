
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
   (selectrum-prescient-mode +1)
   (prescient-persist-mode +1))

(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(global-set-key (kbd "C-x C-r") 'recentf-open-files+)

(defun yank-pop+ (&optional arg)
 "Call `yank-pop' with ARG when appropriate, or offer completion."
 (interactive "*P")
 (if arg (yank-pop arg)
   (let* ((old-last-command last-command)
          (selectrum-should-sort-p nil)
          (enable-recursive-minibuffers t)
          (text (completing-read
                 "Yank: "
                 (cl-remove-duplicates
                  kill-ring :test #'string= :from-end t)
                 nil t nil nil))
          ;; Find `text' in `kill-ring'.
          (pos (cl-position text kill-ring :test #'string=))
          ;; Translate relative to `kill-ring-yank-pointer'.
          (n (+ pos (length kill-ring-yank-pointer))))
     (unless (string= text (current-kill n t))
       (error "Could not setup for `current-kill'"))
     ;; Restore `last-command' over Selectrum commands.
     (setq last-command old-last-command)
     ;; Delegate to `yank-pop' if appropriate or just insert.
     (if (eq last-command 'yank)
         (yank-pop n) (insert-for-yank text)))))

(bind-key* "M-y" 'yank-pop+)

(use-package swiper
  :straight t
  :config
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "p")
    ;; (interactive "P") ;; 大文字のPだと，C-u C-sでないと効かない
    (let (current-prefix-arg)
      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
  )

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
