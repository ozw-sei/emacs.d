
;; projectile
(use-package projectile
  :straight t
  :diminish (projectile-mode)
  :config
  (projectile-mode +1)
  (setq projectile-git-submodule-command nil)
  (setq projectile-indexing-method 'alien))

(use-package helm-projectile
  :straight t)

(defhydra hydra-projectile nil
  "Projectile"
  ("f"   helm-projectile-find-file         "Find File" :exit t)
  ("a"   helm-projectile-ag                "ag" :exit t)
  ("r"   helm-projectile-recentf                  "Recent Files" :exit t)
  ("d"   helm-projectile-find-dir                 "Find Directory" :exit t)
  ("b"   helm-projectile-switch-to-buffer         "Switch to Buffer")
  ("s"   helm-projectile-switch-project           "Switch Project" :exit t)
  ("l"   helm-persp-switch           "Switch Project" :exit t)
  ("k"   projectile-kill-buffers             "Kill Buffers" :exit t))


(use-package helm-ag
  :straight t)

(bind-key* "C-c p" 'hydra-projectile/body)

(projectile-relevant-known-projects)


(use-package ace-window
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key (kbd "C-o") 'ace-window)
  )
