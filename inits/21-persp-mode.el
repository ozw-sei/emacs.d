 ;; (use-package perspective
;;    :straight t
;;    :bind (("C-x b" . persp-switch-to-buffer*)
;;           ("C-x k" . persp-kill-buffer*))
;;    :custom
;;    (persp-state-default-file "~/.emacs.d/persp-state-file")
;;    (persp-switch-prefix "M-%d")
;;    (persp-first-perspective "2") ;; 最初のワークスペースは"2"に設定
;;    (persp-top-perspective "0")
;;    (persp-bottom-perspective "5")
;;    (even-window-sizes 1)

;;    :config
;;    (persp-mode 1)


;;    )

;; (use-package persp-projectile
;;   :straight t
;;   :config
;;   (define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)
;; )

;; (add-hook 'kill-emacs-hook #'persp-state-save)


;; (require 'persp-mode-projectile-bridge)
;; (with-eval-after-load "persp-mode-projectile-bridge-autoloads"
;;   (add-hook 'persp-mode-projectile-bridge-mode-hook
;;             #'(lambda ()
;;                 (if persp-mode-projectile-bridge-mode
;;                     (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
;;                   (persp-mode-projectile-bridge-kill-perspectives))))
;;   (add-hook 'after-init-hook
;;             #'(lambda ()
;;                 (persp-mode-projectile-bridge-mode 1))
;;             t))
