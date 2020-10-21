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
  :straight t
  :config
  (defhydra hydra-git-timemachine (:hint nil :color pink)
"
_p_ previous commit          _n_ next commit
_b_ magit-blame              _c_ Show current commit
_w_ Copy the abbreviated hash of the current historic version  _W_  Copy the full hash of the current historic version
_g_ Goto nth revision  _t_ Goto revision by selected commit message
_?_ help
_._ Exit the time machine.
_q_ close
"
    ("p" git-timemachine-show-previous-revision :exit t)
    ("n" git-timemachine-show-next-revision :exit t)
    ("b" git-timemachine-blame :exit t)
    ("c" git-timemachine-show-commit :exit t)
    ("t" git-timemachine-show-revision-fuzzy :exit t)
    ("w" git-timemachine-kill-abbreviated-revision :exit t)
    ("W" git-timemachine-kill-revision :exit t)
    ("?" git-timemachine-help :exit t)
    ("." git-timemachine-quit :exit t)
    ("q" nil :color blue))
  (define-key git-timemachine-mode-map "." 'hydra-git-timemachine/body))


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
