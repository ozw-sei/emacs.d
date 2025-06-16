(use-package powerline
  :straight t
  :config
  (powerline-default-theme)
  )


;;; modeの名前を自分で再定義
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (flymake-mode . " Fm")

    ;; Major modes
    (fundamental-mode . "Fund")
    (dired-mode . "Dir")
    (lisp-interaction-mode . "Li")
    (cperl-mode . "Pl")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (emacs-lisp-mode . "El")
    (markdown-mode . "Md")))

(defun clean-mode-line ()
  (interactive)
  (cl-loop for (mode . mode-str) in mode-line-cleaner-alist
           do
           (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)
