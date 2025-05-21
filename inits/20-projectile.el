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

(use-package consult-ripgrep
  :straight t
  :ensure t)

(use-package consult-make
  :straight t
  :ensure t)

(use-package projectile-consult-bindings ;; Renamed from helm-projectile
  :straight t ;; This might be redundant if it's just for config, but keep for structure
  :after (projectile consult) ;; Ensure projectile and consult are loaded
  :config
  (define-key projectile-mode-map [remap projectile-find-other-file] #'projectile-find-other-file)
  (define-key projectile-mode-map [remap projectile-find-file] #'projectile-find-file)
  (define-key projectile-mode-map [remap projectile-find-file-in-known-projects] #'projectile-find-file-in-known-projects)
  (define-key projectile-mode-map [remap projectile-find-file-dwim] #'projectile-find-file-dwim)
  (define-key projectile-mode-map [remap projectile-find-dir] #'projectile-find-dir)
  (define-key projectile-mode-map [remap projectile-recentf] #'consult-recent-file)
  (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'consult-buffer)
  (define-key projectile-mode-map [remap projectile-grep] #'consult-ripgrep) ;; Changed from consult-grep to consult-ripgrep
  (define-key projectile-mode-map [remap projectile-ack] #'consult-ripgrep)
  (define-key projectile-mode-map [remap projectile-ag] #'consult-ripgrep)
  (define-key projectile-mode-map [remap projectile-ripgrep] #'consult-ripgrep)
  ;; (define-key projectile-mode-map [remap projectile-browse-dirty-projects] #'helm-projectile-browse-dirty-projects) ;; Commented out
  )

(defhydra hydra-projectile nil
  "Projectile"
  ("f"   projectile-find-file         "Find File" :exit t)
  ("h"   projectile-switch-project    "Switch Project (was helm)" :exit t)
  ("a"   consult-ripgrep              "Ripgrep (ag)" :exit t)
  ("r"   consult-recent-file          "Recent Files" :exit t)
  ("d"   projectile-find-dir          "Find Directory" :exit t)
  ("b"   consult-buffer               "Switch to Buffer" :exit t) ;; Added :exit t for consistency
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

;; (use-package helm-ag
;;   :straight t)

(bind-key* "C-c p" 'hydra-projectile/body)
(bind-key* "C-c C-p" 'hydra-projectile/body)

(projectile-relevant-known-projects)
