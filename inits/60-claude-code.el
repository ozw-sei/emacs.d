;; Claude Code integration via claude-code.el
;; Uses vterm as backend to run `claude` CLI within Emacs
(use-package claude-code
  :straight (:host github :repo "stevemolitor/claude-code.el")
  :after vterm
  :custom
  (claude-code-terminal-backend 'vterm)
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :config
  ;; Use projectile project root instead of project.el
  (defun my/claude-code--directory-projectile ()
    "Get project root using projectile, falling back to default."
    (cond
     ((and (fboundp 'projectile-project-p) (projectile-project-p))
      (projectile-project-root))
     ((buffer-file-name)
      (file-name-directory (buffer-file-name)))
     (t default-directory)))
  (advice-add 'claude-code--directory :override #'my/claude-code--directory-projectile))
