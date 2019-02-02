;;; gnus --- Daniel's Gnus Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Sources:
;; - http://page.math.tu-berlin.de/~freundt/.gnus
;; - Formatting from http://www.emacswiki.org/emacs/GnusFormatting, Version 3
;; - http://www.emacswiki.org/emacs/GnusDemon
;; - http://people.irisa.fr/Nicolas.Berthier/file:.gnus.el

;;; Code:


;;; General

;; Requires

(require 'db-mail)


;;; Mail Formatting

;; http://mbork.pl/2015-11-28_Fixing_mml-attach-file_using_advice
(defun db/mml-attach-file--go-to-eob (orig-fun &rest args)
  "Go to the end of buffer before attaching files."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (apply orig-fun args))))

(advice-add 'mml-attach-file :around #'db/mml-attach-file--go-to-eob)

;;;

t
