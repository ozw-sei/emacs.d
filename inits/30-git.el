(use-package magit
  :straight t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package magit-lfs
  :straight t
  :after magit)
(use-package magit-todos
  :straight t
  :after magit)
(use-package magit-gitflow
  :straight t
  :after magit)

(use-package forge
  :straight t
  :after magit)

(use-package git-timemachine
  :straight t)

(use-package git-gutter
  :straight t
  :diminish (git-gutter-mode)
  :custom
  (global-git-gutter-mode t)
  :bind
  ("C-c g" . hydra-git-gutter/body))

;; git-gutterのhydra定義
(defhydra hydra-git-gutter nil
  "git hunk"
  ("p" git-gutter:previous-hunk "previous" :exit nil)
  ("n" git-gutter:next-hunk "next" :exit nil)
  ("s" git-gutter:stage-hunk "stage")

  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" magit-log-buffer-file "log")
  ("L" magit-log "log")
  ("r" git-gutter:revert-hunk "revert")

  ("m" magit-status "magit-status" :exit t)
  ("i" git-gutter:popup-hunk)
  ("d" magit-status-here "status-here" :exit t)
  ("b" magit-blame-addition "blame" :exit t)
  ("P" magit-push "push" :exit t)
  ("c" magit-commit-create "commit" :exit t)
  ("x" magit-dispatch "dispatch" :exit t)
  ("t" git-timemachine "time-machine" :exit t)
  ; このタイミングでpopup-hunkをクリアしたい
  ("q" nil :color blue))

(use-package git-modes
  :straight t)

(add-to-list 'auto-mode-alist
             (cons "/.dockerignore\\'" 'gitignore-mode))

(use-package git-commit
  :straight t)
