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
   (persp-mode-prefix-key "C-c M-p") ;; Added line

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

(use-package consult-projectile
  :straight t
  :ensure t)

(defhydra hydra-projectile nil
  "Projectile"
  ("f"   projectile-find-file         "Find File" :exit t)
  ("h"   projectile-switch-project    "Switch Project (was helm)" :exit t)
  ("a"   consult-ripgrep              "Ripgrep (ag)" :exit t)
  ("r"   consult-recent-file          "Recent Files" :exit t)
  ("d"   projectile-find-dir          "Find Directory" :exit t)
  ("b"   nsult-buffer               "Switch to Buffer" :exit t) ;; Added :exit t for consistency
  ("k"   projectile-kill-buffers      "Kill Buffers" :exit t)
  ("R"   projectile-replace           "Replace" :exit t)
  ("s"   projectile-switch-project    "Switch Project" :exit t)
  ("c"   projectile-invalidate-cache  "Invalidate Cache" :exit t)
  ("l"   persp-switch                 "Switch Perspective" :exit t) ;; Clarified label
  ("m"   consult-make                 "Make" :exit t)
  ("i"   persp-state-restore          "Import Perspective" :exit t) ;; Clarified label
  ("x"   persp-kill                   "Kill Perspective" :exit t) ;; Clarified label
  ("n"   persp-next                   "Next Perspective") ;; Clarified label
  ("p"   persp-prev                   "Prev Perspective") ;; Clarified label
  ;; ("k"   projectile-kill-buffers             "Kill Buffers" :exit t) ;; Duplicate k, removed
  )

(bind-key* "C-c p" 'hydra-projectile/body)
(bind-key* "C-c C-p" 'hydra-projectile/body)

(projectile-relevant-known-projects)
