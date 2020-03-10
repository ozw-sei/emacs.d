(require 'breadcrumb)
(defhydra hydra-breadcrumb nil
  "
Breadcrumb bookmarks:
  _p_:   prev   _M-p_:   local prev
  _n_: next   _M-n_: local next
  _s_: set  _c_: clear  _l_: list
"
  ("n" bc-next nil :exit nil)
  ("p" bc-previous nil :exit nil)
  ("M-n" bc-local-next nil :exit nil)
  ("M-p" bc-local-previous nil :exit nil)
  ("l" bc-list nil)
  ("s" bc-set nil)
  ("c" bc-clear nil))

(bind-key "C-x o" 'hydra-breadcrumb/body)
