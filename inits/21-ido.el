
(use-package ido
  :bind
  (("C-x C-r" . recentf-ido-find-file)
   ("C-x C-f" . ido-find-file)
   ("C-x C-d" . ido-dired)
   ("C-x b" . ido-switch-buffer)
   ("C-x C-b" . ido-switch-buffer)
 ;("M-x" . smex)
   )
  :init
  (recentf-mode 1)
  
  (defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (setq ido-max-window-height 0.75)
  (when (fboundp 'skk-mode)
    (fset 'ido-select-text 'skk-mode))
  )


(use-package smex
  :bind
  (("M-x" . smex))
  :init
  (setq smex-save-file "~/.emacs.d/cache/.smex-items")
  :config
  (smex-initialize)
  )



