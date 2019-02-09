;;; db-mail.el --- Utility Functions for sending mail -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'mail-extr)
(require 'epg)
(require 'mml-sec)
(require 'gnus)


;; Mail related customizations

(defun db/-set-gnus-secondary-select-methods (other-gnus-accounts remote-mail-accounts)
  "Set `gnus-secondary-select-methods’ from OTHER-GNUS-ACCOUNTS and REMOTE-MAIL-ACCOUNTS.
The values of the latter two variables are usually those of
`db/other-gnus-accounts’ and `db/mail-accounts’."
  (setq gnus-secondary-select-methods
        (append other-gnus-accounts
                ;; Only add those remote accounts whose IMAP address is neither
                ;; `nil’ nor the empty string
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
                                   remote-mail-accounts)))))

(defun db/mail-accounts--set-value (symbol value)
  "Set SYMBOL to VALUE, as needed for `db/mail-accounts’."
  (cl-assert (eq symbol 'db/mail-accounts)
             "Only use `db/mail-accounts--set-value’ only for setting `db/mail-accounts’.")

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
                   (let ((account-name (nth 1 account))
                         (account-address (nth 0 account)))
                     `(,(concat account-name ":")
                       (name ,user-full-name)
                       (address ,account-address)
                       (signature-file "~/.signature")
                       ("X-Jabber-ID" ,db/jabber-id))))
                 value))))

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
           (string :tag "IMAP Server Address")
           (string :tag "SMTP Server Address")
           (choice :tag "SMTP Stream Type"
                   (const nil) (const starttls) (const plain) (const ssl))
           (integer :tag "SMTP Service Port")
           (string :tag "SMTP Login Name")))
  :set #'db/mail-accounts--set-value)

(defun db/other-gnus-accounts--set-value (symbol value)
  "Set SYMBOL to VALUE as needed by `db/other-gnus-accounts’"
  (cl-assert (eq symbol 'db/other-gnus-accounts)
             "Only use `db/other-gnus-accounts--set-value’ only for setting `db/other-gnus-accounts’.")

  (set-default symbol value)
  (db/-set-gnus-secondary-select-methods
   value
   ;; Don’t complain if `db/mail-accounts’ is not defined yet.
   (and (boundp 'db/mail-accounts) db/mail-accounts)))

(defcustom db/other-gnus-accounts nil
  "Configuration for gnus accounts that are not IMAP/SMTP related.
The value of this variable should be a valid value for `gnus-secondary-select-methods’."
  :group 'personal-settings
  ;; type definition for `gnus-select-method’ widget from from gnus.el
  :type '(repeat gnus-select-method)
  :set #'db/other-gnus-accounts--set-value)


;; Functions related to email encryption

(defun db/public-key (address &optional method)
  "Return valid public keys for ADDRESS and given METHOD.

METHOD can be \"smime\" or \"pgpmime\"; defaults to \"pgpmime\".
ADDRESS is a string containing exactly one email address."
  (check-type address string)
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


;; SMTP related functions

(defun db/set-smtp-server-from-header (orig-fun &rest args)
  "Choose smtp-settings dynamically, based on the From: header
entry of the current mail."
  (require 'mail-extr)
  (let* ((from    (or (save-restriction
                        (message-narrow-to-headers)
                        (mail-fetch-field "From"))
                      user-mail-address))
         (address (cadr (mail-extract-address-components from)))
         (account (assoc address db/mail-accounts)))
    (message "Using address: %s" address)
    (if account
        (progn
          (message "Sending with account for %s" address)
          ;; XXX: these calls to `nth’ should be abstracted away
          (let ((smtpmail-smtp-server (nth 3 account))
                (smtpmail-stream-type (nth 4 account))
                (smtpmail-smtp-service (nth 5 account))
                (smtpmail-smtp-user (nth 6 account)))
            (cl-assert (cl-notany #'null (list smtpmail-smtp-server
                                               smtpmail-stream-type
                                               smtpmail-smtp-service
                                               smtpmail-smtp-user))
                       t
                       "Settings %s for sending mail are not complete for account %s."
                       address)
            (apply orig-fun args)))
      (if (yes-or-no-p "Sending with default account settings?")
          (apply orig-fun args)
        (message "Sending aborted as requested by user.")))))


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
  (require 'gnus-start)                 ; load global variables
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


;; Fixes

;; http://mbork.pl/2015-11-28_Fixing_mml-attach-file_using_advice
(defun db/mml-attach-file--go-to-eob (orig-fun &rest args)
  "Go to the end of buffer before attaching files."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (apply orig-fun args))))

(provide 'db-mail)
;;; db-mail ends here
