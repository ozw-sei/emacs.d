(use-package ace-window
  :straight t)

(defun other-window-or-split ()
  "If there is one window, open split window.
If there are two or more windows, it will go to another window."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (aw-flip-window))


(bind-key* "C-t" 'other-window-or-split)
