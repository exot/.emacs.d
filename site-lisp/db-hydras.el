;;; db-hydras.el --- Personal hydras ' -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(autoload 'rectangle-exchange-point-and-mark "rect")

(require 'hydra)
(require 'db-customize)

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

;; The hydra for our frequently used features should be defined here, but should
;; also be redefined every time `db/frequently-used-features' is redefined via
;; customize.  To this end, we provide a special function here that defines this
;; hydra, that can also be called in the setter of
;; `db/frequently-used-features'.

(defun db/define-feature-shortcuts-hydra ()
  "Globally define `hydra-feature-shortcuts' for feature shortcuts.
If instead of a shortcut character nil is provided, no entry in
the hydra will be generated.  See documentation of
`db/frequently-used-features' for details."
  (eval
   `(defhydra hydra-feature-shortcuts (:color blue)
      ""
      ,@(mapcar (lambda (entry)
                  (pcase-let ((`(,description ,shortcut ,function) entry))
                    (list (string shortcut) function description)))
                (cl-remove-if #'(lambda (entry)
                                  (null (cl-second entry)))
                              db/frequently-used-features)))))

(db/define-feature-shortcuts-hydra)

(provide 'db-hydras)
;; db-hydras.el ends here
