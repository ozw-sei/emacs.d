(use-package shader-mode
  :straight t)
(use-package glsl-mode
  :straight t
  :mode (("\\.effect\\'" . glsl-mode)
        ("\\.fsh\\'" . glsl-mode)
        ("\\.vsh\\'" . glsl-mode)
        ("\\.shader\\'" . glsl-mode)))
