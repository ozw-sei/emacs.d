;; projectile
(use-package projectile
  :straight t
  :diminish (projectile-mode)
  :config
  (projectile-mode +1)
  (setq projectile-indexing-method 'hybrid))

(defhydra hydra-projectile nil
  "Projectile"
  ("f"   counsel-projectile-find-file         "Find File" :exit t)
  ("A"   projectile-ag                "ag" :exit t)
  ("a"   counsel-projectile-ag                "ag in counsel" :exit t)
  ("r"   projectile-recentf                  "Recent Files" :exit t)
  ("d"   counsel-projectile-find-dir                 "Find Directory" :exit t)
  ("b"   counsel-projectile-switch-to-buffer         "Switch to Buffer")
  ("c" counsel-compile "compile-project" :exit t)
  ("s"   projectile-switch-project           "Switch Project" :exit t)
  ("l"   persp-switch           "Switch Project" :exit t)
  ("k"   projectile-kill-buffers             "Kill Buffers" :exit t))



(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode 1)
  (setq counsel-projectile-sort-files t) ;; 当該プロジェクト内リストをソート
  (setq counsel-projectile-sort-projects t) ;; プロジェクトリストをソート
  :after (projectile))

(bind-key* "C-c p" 'hydra-projectile/body)

(projectile-relevant-known-projects)
