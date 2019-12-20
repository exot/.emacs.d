;;; db-hydras.el --- Personal hydras ' -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'hydra)

(defhydra hydra-toggle (:color blue)
  "toggle"
  ("c" column-number-mode "column")
  ("d" toggle-debug-on-error "debug-on-error")
  ("e" toggle-debug-on-error "debug-on-error")
  ("f" auto-fill-mode "auto-fill")
  ("l" toggle-truncate-lines "truncate lines")
  ("q" toggle-debug-on-quit "debug-on-quit")
  ("r" read-only-mode "read-only"))

;; zooming with single keystrokes (from oremacs)
(defhydra hydra-zoom (:color red
                      :body-pre (require 'face-remap))
  "zoom"
  ("g" text-scale-increase "increase")
  ("l" text-scale-decrease "decrease"))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1))
   nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("e" rectangle-exchange-point-and-mark nil)
  ("o" nil nil))

(defun hydra-shortcuts/body ()
  "Dummy default value for shortcuts hydra.  Will simply barf."
  (interactive)
  (user-error "Shortcuts Hydra not defined yet."))

(defun db/define-hydra-from-frequently-used-features ()
  "Defines `hydra-shortcuts/body' based on the current value of
`db/frequently-used-features'.  Raises an error if the latter is
not bound."
  (if (not (boundp 'db/frequently-used-features))
      (user-error "Variable `db/frequently-used-features' is not defined, please set that variable first.")
    (eval
     `(defhydra hydra-shortcuts (:color blue)
        ""
        ,@(mapcar (lambda (entry)
                    (pcase-let ((`(,description ,shortcut ,function) entry))
                      (list (string shortcut) function description)))
                  db/frequently-used-features)))))

(provide 'db-hydras)
;; db-hydras.el ends here
