;;; db-emacsclient.el --- Execute functions called from emacsclient

;;; Commentary:
;;
;;

;;; Code:

(defvar db/emacsclient-map (make-sparse-keymap)
  "Keymap used for keys called from emacsclient.")

(defun db/emacsclient-key (key)
  "Run command associated with `KEY' in `db/emacsclient-map'."
  (let ((function (lookup-key db/emacsclient-map key)))
    (when function (apply function nil))))

(define-key db/emacsclient-map "w"
  (lambda ()
    (interactive)
    (clock-in-task-by-id org-working-task-id)
    ;;; FIXME: duplicate code
    (call-process "xrandr" nil nil nil
                  "--output" "HDMI-3" "--primary" "--right-of" "LVDS-1" "--auto")
    (call-process "xkbcomp" nil nil nil
                  "-I" "$HOME/.local/share/xkb/"
                  "~/.local/share/xkb/keymap/xkbtest $DISPLAY")))

(define-key db/emacsclient-map "2"
  (lambda ()
    (call-process "xrandr" nil nil nil
                  "--output" "HDMI-3" "--primary" "--right-of" "LVDS-1" "--auto")
    (call-process "xkbcomp" nil nil nil
                  "-I" "$HOME/.local/share/xkb/"
                  "~/.local/share/xkb/keymap/xkbtest $DISPLAY")))

(define-key db/emacsclient-map "o"
  #'org-clock-out)

(define-key db/emacsclient-map "b"
  (lambda ()
    (interactive)
    (clock-in-task-by-id org-break-task-id)))

(define-key db/emacsclient-map "h"
  (lambda ()
    (interactive)
    (clock-in-task-by-id org-home-task-id)
    ;;; FIXME: duplicate code
    (call-process "xrandr" nil nil nil
                  "--output" "HDMI-3" "--off")))

(define-key db/emacsclient-map "1"
  (lambda ()
    (call-process "xrandr" nil nil nil
                  "--output" "HDMI-3" "--off")))

;;;

(provide 'db-emacsclient)

;;; db-emacsclient.el ends here
