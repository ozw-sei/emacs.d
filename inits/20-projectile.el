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
  :straight t)

(defhydra hydra-projectile nil
  "Projectile"
  ("f"   consult-projectile-find-file         "Find File" :exit t)
  ("h"   consult-projectile-switch-project    "Switch Project" :exit t)
  ("a"   consult-ripgrep              "Ripgrep (ag)" :exit t)
  ("r"   consult-recent-file          "Recent Files" :exit t)
  ("d"   consult-projectile-find-dir          "Find Directory" :exit t)
  ("b"   consult-buffer               "Switch to Buffer" :exit t) ;; Added :exit t for consistency
  ("k"   consult-projectile-kill-buffers      "Kill Buffers" :exit t)
  ("R"   consult-projectile-replace           "Replace" :exit t)
  ("s"   consult-projectile-switch-project    "Switch Project" :exit t)
  ("c"   projectile-invalidate-cache  "Invalidate Cache" :exit t)
  ("l"   persp-switch                 "Switch Perspective" :exit t) ;; Clarified label
  ("i"   persp-state-restore          "Import Perspective" :exit t) ;; Clarified label
  ("x"   persp-kill                   "Kill Perspective" :exit t) ;; Clarified label
  ("n"   persp-next                   "Next Perspective") ;; Clarified label
  ("p"   persp-prev                   "Prev Perspective") ;; Clarified label
  ("t" my/vterm-project-toggle-bottom "open terminal")
  )

(bind-key* "C-c p" 'hydra-projectile/body)
(bind-key* "C-c C-p" 'consult-projectile)

(projectile-relevant-known-projects)
