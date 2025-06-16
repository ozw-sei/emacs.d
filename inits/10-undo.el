(use-package undohist
  :straight t
  :config
  (setq undohist-ignored-files
      '("/tmp/" "COMMIT_EDITMSG"))
  (undohist-initialize)
  )
;; vundo - Visual undo tree
(use-package vundo
  :straight t
  :commands (vundo)
  :bind (("C-c u" . vundo)
         ("C-x u" . undo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  ;; Use horizontal layout (like undo-tree's vertical display)
  (vundo-window-side 'bottom)
  (vundo-window-max-height 10)
  :config
  ;; Optional: Customize the display to be more vertical-like
  (setq vundo-glyph-alist
        '((selected-node   . ?●)
          (node            . ?○)
          (horizontal-stem . ?─)
          (vertical-stem   . ?│)
          (branch          . ?├)
          (last-branch     . ?└))))
