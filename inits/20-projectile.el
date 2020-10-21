(use-package perspective
   :straight t
   :bind (("C-x b" . persp-switch-to-buffer*)
          ("C-x k" . persp-kill-buffer*))
   :custom
   (persp-state-default-file "~/.emacs.d/persp-state-file")
   (persp-switch-prefix "M-%d")
   (persp-first-perspective "2") ;; 最初のワークスペースは"2"に設定
   (persp-top-perspective "0")
   (persp-bottom-perspective "5")
   (even-window-sizes 1)

   :config
   (persp-mode 1))

(use-package persp-projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)
)

(add-hook 'kill-emacs-hook #'persp-state-save)

;; projectile
(use-package projectile
  :straight t
  :diminish (projectile-mode)
  :custom
  (projectile-git-submodule-command nil)
  (projectile-indexing-method 'alien)
  :config
  (projectile-mode +1))



(use-package helm-projectile
  :straight t
  :config
  (define-key projectile-mode-map [remap projectile-find-other-file] #'helm-projectile-find-other-file)
  (define-key projectile-mode-map [remap projectile-find-file] #'helm-projectile-find-file)
  (define-key projectile-mode-map [remap projectile-find-file-in-known-projects] #'helm-projectile-find-file-in-known-projects)
  (define-key projectile-mode-map [remap projectile-find-file-dwim] #'helm-projectile-find-file-dwim)
  (define-key projectile-mode-map [remap projectile-find-dir] #'helm-projectile-find-dir)
  (define-key projectile-mode-map [remap projectile-recentf] #'helm-projectile-recentf)
  (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer)
  (define-key projectile-mode-map [remap projectile-grep] #'helm-projectile-grep)
  (define-key projectile-mode-map [remap projectile-ack] #'helm-projectile-ack)
  (define-key projectile-mode-map [remap projectile-ag] #'helm-projectile-ag)
  (define-key projectile-mode-map [remap projectile-ripgrep] #'helm-projectile-rg)
  (define-key projectile-mode-map [remap projectile-browse-dirty-projects] #'helm-projectile-browse-dirty-projects)
  )

(defhydra hydra-projectile nil
  "Projectile"
  ("f"   helm-projectile-find-file         "Find File" :exit t)
  ("h"   helm-projectile         "helm" :exit t)
  ("a"   helm-projectile-ag                "ag" :exit t)
  ("r"   helm-projectile-recentf                  "Recent Files" :exit t)
  ("d"   helm-projectile-find-dir                 "Find Directory" :exit t)
  ("b"   helm-projectile-switch-to-buffer         "Switch to Buffer")
  ("k"   projectile-kill-buffers             "Kill Buffers" :exit t)
  ("R"   projectile-replace             "Replace" :exit t)
  ("s"   projectile-switch-project           "Switch Project" :exit t)
  ("c"   projectile-invalidate-cache           "invalidate" :exit t)
  ("l"   persp-switch           "Switch Project" :exit t)
  ("i"   persp-state-restore           "import" :exit t)
  ("x"   persp-kill           "kill" :exit t)
  ("n"   persp-next           "next")
  ("p"   persp-prev           "prev")
  ("k"   projectile-kill-buffers             "Kill Buffers" :exit t))

(use-package helm-ag
  :straight t)

(bind-key* "C-c p" 'hydra-projectile/body)
(bind-key* "C-c C-p" 'hydra-projectile/body)

(projectile-relevant-known-projects)


(use-package ace-window
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key (kbd "C-o") 'ace-window)
  (global-set-key (kbd "C--") 'ace-swap-window)
  )
