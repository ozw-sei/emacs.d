(use-package ag
  :straight t
  :config
  (setq ag-executable "ag")
  (setq ag-arguments (list "--path-to-ignore" "--skip-vcs-ignores")))
