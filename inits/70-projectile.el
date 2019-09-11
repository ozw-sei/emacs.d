(require 'projectile)
(projectile-mode +1)

(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-c C-g") 'projectile-ag)

(setq projectile-project-search-path '("~/sources/repos/"))
