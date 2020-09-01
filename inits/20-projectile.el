
;; projectile
(use-package projectile
  :straight t
  :diminish (projectile-mode)
  :config
  (projectile-mode +1)
  (setq projectile-git-submodule-command nil)
  (setq projectile-indexing-method 'alien))

(defhydra hydra-projectile nil
  "Projectile"
  ("f"   projectile-find-file         "Find File" :exit t)
  ("a"   projectile-ag                "ag" :exit t)
  ("r"   projectile-recentf                  "Recent Files" :exit t)
  ("d"   projectile-find-dir                 "Find Directory" :exit t)
  ("b"   projectile-switch-to-buffer         "Switch to Buffer")
  ("s"   projectile-switch-project           "Switch Project" :exit t)
  ("l"   persp-switch           "Switch Project" :exit t)
  ("k"   projectile-kill-buffers             "Kill Buffers" :exit t))


(bind-key* "C-c p" 'hydra-projectile/body)

(projectile-relevant-known-projects)
