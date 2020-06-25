 (use-package persp-mode
   :straight t
   :custom
   (persp-add-on-switch-or-display t)
   :config
   (persp-mode))

(require 'persp-mode-projectile-bridge)
(with-eval-after-load "persp-mode-projectile-bridge-autoloads"
  (add-hook 'persp-mode-projectile-bridge-mode-hook
            #'(lambda ()
                (if persp-mode-projectile-bridge-mode
                    (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                  (persp-mode-projectile-bridge-kill-perspectives))))
  (add-hook 'after-init-hook
            #'(lambda ()
                (persp-mode-projectile-bridge-mode 1))
            t))
