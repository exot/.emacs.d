;;; db-mail.el --- Utility Functions for sending mail -*- lexical-binding: t -*-

;;; Commentary:

;; This package consists mostly of utility functions for sending and editing
;; mail and extending Gnus in the way I like it.  However, the most important
;; feature of is an abstract way to hande remote mail accounts.  Theses are
;; stored in the variable `db/mail-accounts’ and consist of various entries
;; denoting how to read mail via IMAP (POP not supported) and how to send mail
;; via SMTP (server, port, protocol, …).  Based on the entries in this variable,
;; appropriate settings of `gnus-secondary-select-methods’ are derived
;; automatically by the custom setter of `db/mail-accounts’.  Then when inside
;; Gnus, the mail can be read without further configuration.  Note that when the
;; "IMAP address" of an entry in `db/mail-accounts’ is empty, it will not be
;; included as an IMAP account in `gnus-secondary-select-methods’.  This let’s
;; you specify mail accounts that can be used for sending mail, but that are
;; read not directly via IMAP, but by other means (POP, offlineimap, …).

;; When sending mail, `db/mail-accounts’ is used to determine settings of the
;; relevant variables from `smtpmail’ based on the current value of the "From: "
;; header entry in the mail.  If this header entry is set correctly, then
;; `db/smtpmail-send-it’ will set these variables automatically.  To make this
;; work, however, two things have to be done:
;;
;; - Ensure that the "From: " header is set correctly.  In Gnus this can be done
;;   by configuring `gnus-posting-style’ accordingly.
;;
;; - Make sure `db/smtpmail-send-it’ is called when sending mail.  For this set
;;   the value `send-mail-function’ to `db/smtpmail-send-it’.

;; All this functionality is provided under "Mail related customizations".  The
;; other headlines provide the aforementioned utility functions.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'cl-macs)
(require 'mail-extr)
(require 'epg)
(require 'mml-sec)
(require 'gnus)
(require 'gnus-start)
(require 'smtpmail)                   ; to have the globals bound below by let
(require 'message)
(require 'db-customize)

(declare-function gnus-summary-select-article-buffer "gnus-sum")
(declare-function gnus-mime-pipe-part "gnus-art")


;; Mail related customizations

;; See definition of `db/mail-accounts’ below, sorry for the scatter …

(defsubst db/mail-accounts--mail-address (account)
  "Extract account mail address from ACCOUNT.
ACCOUNT must be a valid element of `db/mail-accounts’."
  (nth 0 account))

(defsubst db/mail-accounts--name (account)
  "Extract account name from ACCOUNT.
ACCOUNT must be a valid element of `db/mail-accounts’."
  (nth 1 account))

(defsubst db/mail-accounts--imap-address (account)
  "Extract account IMAP address from ACCOUNT.
ACCOUNT must be a valid element of `db/mail-accounts’."
  (nth 2 account))

(defsubst db/mail-accounts--smtp-server (account)
  "Extract account SMTP server address from ACCOUNT.
ACCOUNT must be a valid element of `db/mail-accounts’."
  (nth 3 account))

(defsubst db/mail-accounts--smtp-stream-type (account)
  "Extract account SMTP stream type from ACCOUNT.
ACCOUNT must be a valid element of `db/mail-accounts’."
  (nth 4 account))

(defsubst db/mail-accounts--smtp-service-port (account)
  "Extract account SMTP port from ACCOUNT.
ACCOUNT must be a valid element of `db/mail-accounts’."
  (nth 5 account))

(defsubst db/mail-accounts--smtp-user (account)
  "Extract account SMTP user from ACCOUNT.
ACCOUNT must be a valid element of `db/mail-accounts’."
  (nth 6 account))

(defun db/-set-gnus-secondary-select-methods (other-gnus-accounts remote-mail-accounts)
  "Set `gnus-secondary-select-methods’ from OTHER-GNUS-ACCOUNTS and REMOTE-MAIL-ACCOUNTS.
The values of the latter two variables are usually those of
`db/other-gnus-accounts’ and `db/mail-accounts’.  If multiple
accounts exist with the same (equalp) account name, only the
first will be added to `gnus-secondary-select-methods'."
  (let ((select-methods (append other-gnus-accounts
                                ;; Only add those remote accounts whose IMAP address is neither
                                ;; `nil’ nor the empty string
                                (cl-remove-if #'null
                                              (mapcar (lambda (account)
                                                        (let ((account-name (db/mail-accounts--name account))
                                                              (account-address (db/mail-accounts--imap-address account)))
                                                          (when (and account-address
                                                                     (stringp account-address)
                                                                     (< 0 (length account-address)))
                                                            `(nnimap ,account-name
                                                                     (nnimap-address ,account-address)
                                                                     (nnimap-stream starttls)
                                                                     (nnimap-inbox "INBOX")))))
                                                      remote-mail-accounts)))))
    (setq gnus-secondary-select-methods
          (cl-remove-duplicates select-methods
                                :key #'cl-second ; account name is second element
                                :test #'equalp))))

;; Let's make the byte-compiler happy
(defvar gnus-posting-styles)
(defvar bbdb-user-mail-address-re)
(defvar message-dont-reply-to-names)
(defvar gnus-ignored-from-addresses)

(defun db/mail-accounts--set-value (symbol value)
  "Set SYMBOL to VALUE, as needed for `db/mail-accounts’."
  (cl-assert (eq symbol 'db/mail-accounts)
             nil
             "Only use `db/mail-accounts--set-value’ for setting `db/mail-accounts’.")

  (set-default symbol value)

  ;; Set `gnus-secondary-select-methods’
  (db/-set-gnus-secondary-select-methods
   ;; Don’t complain if `db/other-gnus-accounts’ is not defined yet
   (and (boundp 'db/other-gnus-accounts) db/other-gnus-accounts)
   value)

  ;; Set posting styles based on existing mail addresses
  (setq gnus-posting-styles
        (append
         `((".*"
            (name ,user-full-name)
            (address ,user-mail-address)
            (signature-file "~/.signature")
            ("X-Jabber-ID" ,db/jabber-id)))
         (mapcar (lambda (account)
                   (let ((account-name (db/mail-accounts--name account))
                         (account-address (db/mail-accounts--mail-address account)))
                     `(,(concat account-name ":")
                       (name ,user-full-name)
                       (address ,account-address)
                       (signature-file "~/.signature")
                       ("X-Jabber-ID" ,db/jabber-id))))
                 value)))

  ;; Update some variables
  (setq bbdb-user-mail-address-re (regexp-opt (mapcar #'car value) 'words)
        message-dont-reply-to-names (regexp-opt (mapcar #'car value) 'words)
        gnus-ignored-from-addresses message-dont-reply-to-names))

(defcustom db/mail-accounts nil
  "Configuration for remote email accounts.
This is a list of lists, where each such list specifies necessary
parameters for one particular email address.  These addresses
will also be recognized when sending mail."
  :group 'personal-settings
  :type '(repeat
          (list
           (string :tag "EMail Address")
           (string :tag "Group Name")
           (string :tag "IMAP Server Address (StartTLS)")
           (string :tag "SMTP Server Address")
           (choice :tag "SMTP Stream Type"
                   (const nil) (const starttls) (const plain) (const ssl))
           (integer :tag "SMTP Service Port")
           (string :tag "SMTP Login Name")))
  :set #'db/mail-accounts--set-value)

(defun db/smtpmail-send-it ()
  "Send prepared message in current buffer.
This function uses `message-smtpmail-send-it’, but sets
`smtpmail-smtp-server’, `smtpmail-stream-type’,
`smtpmail-smtp-service’, and `smtpmail-smtp-user’ based on the
entry of the \"From: \" header and the value of
`db/mail-accounts’."
  (let* ((from    (or (save-restriction
                        (message-narrow-to-headers)
                        (mail-fetch-field "From"))
                      user-mail-address))
         (address (cadr (mail-extract-address-components from)))
         (account (assoc address db/mail-accounts)))
    (message "Using address: %s" address)
    (condition-case signal-data
        (if account
            (progn
              (message "Sending with account for %s" address)
              (let ((smtpmail-smtp-server (db/mail-accounts--smtp-server account))
                    (smtpmail-stream-type (db/mail-accounts--smtp-stream-type account))
                    (smtpmail-smtp-service (db/mail-accounts--smtp-service-port account))
                    (smtpmail-smtp-user (db/mail-accounts--smtp-user account)))
                (cl-assert (cl-notany #'null (list smtpmail-smtp-server
                                                   smtpmail-stream-type
                                                   smtpmail-smtp-service
                                                   smtpmail-smtp-user))
                           t
                           "Settings %s for sending mail are not complete for account %s."
                           address)
                (message-smtpmail-send-it)))
          (if (yes-or-no-p "Sending with default account settings?")
              (message-smtpmail-send-it)
            (message "Sending aborted as requested by user.")))
      ;; in case of error, display the SMTP trace buffer if available
      (error (when-let ((smtp-trace-buffer (get-buffer (format "*trace of SMTP session to %s*"
                                                               smtpmail-smtp-server))))

               (shrink-window-if-larger-than-buffer (display-buffer smtp-trace-buffer)))
             (signal (car signal-data) (cdr signal-data))))))


;; Setting other Gnus accounts

(defun db/other-gnus-accounts--set-value (symbol value)
  "Set SYMBOL to VALUE as needed by `db/other-gnus-accounts’"
  (cl-assert (eq symbol 'db/other-gnus-accounts)
             nil
             "Only use `db/other-gnus-accounts--set-value’ for setting `db/other-gnus-accounts’.")

  (set-default symbol value)
  (db/-set-gnus-secondary-select-methods
   value
   ;; Don’t complain if `db/mail-accounts’ is not defined yet.
   (and (boundp 'db/mail-accounts) db/mail-accounts)))

(defcustom db/other-gnus-accounts nil
  "Configuration for gnus accounts that are not IMAP/SMTP related.
Those are all accounts that are not derived from the entries in
`db/mail-accounts’.  The value of this variable should be a valid
value for `gnus-secondary-select-methods’."
  :group 'personal-settings
  ;; type definition for `gnus-select-method’ widget from gnus.el
  :type '(repeat gnus-select-method)
  :set #'db/other-gnus-accounts--set-value)


;; Functions related to email encryption

(defun db/public-key (address &optional method)
  "Return valid public keys for ADDRESS and given METHOD.

METHOD can be \"smime\" or \"pgpmime\"; defaults to \"pgpmime\".
ADDRESS is a string containing exactly one email address."
  (cl-check-type address string)
  (unless method (setq method "pgpmime"))
  (epg-list-keys (epg-make-context
                  (cond
                   ((string= method "smime")
                    'CMS)
                   ((string= method "pgpmime")
                    'OpenPGP)
                   (t (error "Unknown method %s" method))))
                 address))

(defun db/encryption-possible-p (recipients method)
  "Check whether sending encrypted emails to all RECIPIENTS is possible.

METHOD specifies the encrypt method used.  Can be either
\"smime\" or \"pgpmime\"."
  (cl-every (lambda (recipient)
              (not (null (db/public-key recipient method))))
            recipients))

(defun db/message-recipients ()
  "Return all recipients of the email in the current buffer."
  (cl-mapcan (lambda (field)
               (let ((field-value (message-field-value field)))
                 (when field-value
                   (mapcar #'cadr
                           (mail-extract-address-components field-value t)))))
             (list "to" "cc" "bcc")))

(defun db/signencrypt-message-when-possible ()
  "Add mml markers for signing and encryption of an email if possible."
  (interactive)
  (when (eq major-mode 'message-mode)
    (let ((from (message-field-value "from")))
      (when from
        (let ((methods (if (string-match "@tu-dresden\.de>" from)
                           (list "smime" "pgpmime")
                         (list "pgpmime")))
              (recipients (db/message-recipients)))

          ;; if there is no recipient, encrypt with default method
          (if (null recipients)
              (mml-secure-message (cl-first methods) 'signencrypt)

            ;; go through available keys
            (let ((available-method
                   (cl-find-if (lambda (method)
                                 (db/encryption-possible-p recipients method))
                               methods)))

              (if available-method
                  (mml-secure-message available-method 'signencrypt)

                ;; if nothing works, sign with default method
                (mml-secure-message (cl-first methods) 'sign)))))))))


;; Gnus utility functions

(defun db/gnus-save-newsrc-with-whitespace-1 ()
  "Save ~/.newsrc.eld with extra whitespace."
  ;; http://ding.gnus.narkive.com/pq3Z8ZjQ/pretty-printing-newsrc-eld#post3
  (gnus-message 5 "Adding whitespace to .newsrc.eld")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "(\\\"\\| ((\\| (nn" nil t)
      (replace-match "\n \\&" t))
    (delete-trailing-whitespace)))

(defun db/gnus-summary-open-Link ()
  "Open link named \"Link\" in current article."
  (interactive)
  (save-window-excursion
   (save-mark-and-excursion
    (save-restriction
     (widen)
     (let ((url nil))
       (unless (eq major-mode 'gnus-article-mode)
         (gnus-summary-select-article-buffer))
       (goto-char (point-min))
       (while (and (not url)
                   (search-forward "Link"))
         (backward-char)
         (setq url (get-text-property (point) 'shr-url)))
       (when url
         (browse-url url)))))))

(defun db/gnus-html-mime-part-to-org ()
  "Convert current gnus article to org mode."
  (interactive)
  (let ((return-code (gnus-mime-pipe-part "pandoc -f html -t org")))
    (unless (zerop return-code)
      (error "Error in extracting text"))
    (with-current-buffer "*Shell Command Output*"
      (kill-ring-save (point-min) (point-max)))))

(defun db/gnus-demon-scan-news-on-level-2 ()
  "Scan for news in Gnus on level 2."
  ;; from https://www.emacswiki.org/emacs/GnusDemon
  (let ((win (current-window-configuration))
        (gnus-read-active-file 'some)
        (gnus-check-new-newsgroups nil)
        (level 2))
    (while-no-input
      (unwind-protect
          (save-window-excursion
            (when (gnus-alive-p)
              (with-current-buffer gnus-group-buffer
                (gnus-group-get-new-news level))))
        (set-window-configuration win)))))


(provide 'db-mail)
;;; db-mail ends here
