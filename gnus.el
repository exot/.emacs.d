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

;; Accounts

(setq gnus-select-method '(nnnil "")
      ;; XXX: this should be set by the customize interface of
      ;; `db/mail-accounts’
      gnus-secondary-select-methods
      (append
       ;; immutable account definitions
       `((nntp "etsep"
               (nntp-open-connection-function nntp-open-tls-stream)
               (nntp-port-number 563)
               (nntp-address "news.eternal-september.org"))
         (nntp "gmane"
               (nntp-open-connection-function nntp-open-network-stream)
               (nntp-address "news.gmane.org"))
         (nnimap "algebra20"
                 (nnimap-stream shell)
                 (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/Mail/algebra20")
                 (nnimap-split-methods nnimap-split-fancy)
                 (nnimap-inbox "INBOX")
                 (nnimap-split-fancy ,db/personal-gnus-filter-rules))
         (nnml "local"
               (nnmail-split-methods nnmail-split-fancy)
               (nnmail-split-fancy
                (| ("subject" ".*Tiger Auditing Report for.*" "mail.tiger")
                   "mail.misc")))
         (nnmaildir "archive"
                    (directory "~/Mail/archive/")
                    (directory-files nnheader-directory-files-safe)
                    (nnir-search-engine notmuch)
                    (nnir-notmuch-remove-prefix ,(expand-file-name "~/Mail/archive/"))))

       ;; automatically add accounts when address is not nil and not the empty string
       ;; XXX: this should be abstracted away in some kind of function
       (remove-if #'null
                  (mapcar (lambda (account)
                            (let ((account-name (nth 1 account))
                                  (account-address (nth 2 account)))
                              (when (and account-address
                                         (stringp account-address)
                                         (< 0 (length account-address)))
                                `(nnimap ,account-name
                                         (nnimap-address ,account-address)
                                         (nnimap-inbox "INBOX")))))
                          db/mail-accounts))))


;;; Mail Formatting

;; XXX: This should actually be set by the customize setter of
;; `db/mail-accounts’
(setq gnus-posting-styles
      (append
       `((".*"
          (name ,user-full-name)
          (address ,user-mail-address)
          (signature-file "~/.signature")
          ("X-Jabber-ID" ,db/jabber-id)))
       ;; XXX: this should be abstracted away in some kind of function
       (mapcar (lambda (account)
                 (let ((account-name (nth 1 account))
                       (account-address (nth 0 account)))
                   `(,(concat account-name ":")
                     (name ,user-full-name)
                     (address ,account-address)
                     (signature-file "~/.signature")
                     ("X-Jabber-ID" ,db/jabber-id))))
               db/mail-accounts)))

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
