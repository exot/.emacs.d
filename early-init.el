;; -*- lexical-binding: t -*-

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; From https://protesilaos.com/emacs/dotemacs: temporarily increase the GC
;; threshold on startup to speed up booting.  Reenable GC limits as soon as
;; Emacs is started.

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)   ; 100mb
                  gc-cons-percentage 0.1)))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

(dolist (mode '(tool-bar-mode
                scroll-bar-mode
                menu-bar-mode
                blink-cursor-mode
                tooltip-mode))
  (when (fboundp mode)
    (funcall mode 0)))

