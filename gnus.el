;;; gnus --- Daniel's Gnus Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Sources:
;; - http://page.math.tu-berlin.de/~freundt/.gnus
;; - Formatting from http://www.emacswiki.org/emacs/GnusFormatting, Version 3
;; - http://www.emacswiki.org/emacs/GnusDemon
;; - http://people.irisa.fr/Nicolas.Berthier/file:.gnus.el

;;; Code:


;;; General

;; Customization

;; XXX: This needs some functionality for local accounts
(defcustom db/mail-accounts nil
  "Configuration for email accounts.
This is a list of lists, where each such list specifies necessary
parameters for one particular email address."
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
           (string :tag "SMTP Login Name"))))

(defcustom db/personal-gnus-filter-rules nil
  "Default filter rules as used by Gnus for `user-mail-address’."
  :group 'personal-settings
  :type 'sexp)

;; Requires

(require 'dash)
(require 'gnus)
(require 'message)
(require 'gnus-util)
(require 'gnus-start)
(require 'gnus-group)
(require 'gnus-sum)
(require 'gnus-art)
(require 'gnus-score)
(require 'nntp)
(require 'gnus-agent)
(require 'nnml)
(require 'gnus-async)

;; Accounts

(setq-default message-dont-reply-to-names
              (regexp-opt (cons user-mail-address db/additional-mail-addresses)
                          'words))

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

;; General Configuration

(setq gnus-ignored-from-addresses message-dont-reply-to-names
      message-directory (expand-file-name "mail/" gnus-directory)
      nnmail-message-id-cache-file (expand-file-name ".nnmail-cache" gnus-directory)
      nnml-directory message-directory
      mail-sources '((file))
      mail-source-delete-incoming t
      nntp-nov-is-evil t
      nntp-connection-timeout nil
      gnus-asynchronous t
      gnus-save-killed-list nil
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-check-new-newsgroups nil
      gnus-use-cache 'passive
      gnus-read-active-file 'some
      gnus-build-sparse-threads 'some
      gnus-subscribe-newsgroup-method 'gnus-subscribe-killed
      gnus-group-list-inactive-groups t
      gnus-suppress-duplicates nil
      gnus-large-newsgroup 200
      nnmail-expiry-wait 7
      nnmail-cache-accepted-message-ids t
      gnus-summary-next-group-on-exit nil
      gnus-use-full-window nil
      gnus-always-force-window-configuration t
      gnus-fetch-old-headers nil)

(setq gnus-visible-headers (regexp-opt '("From:"
                                         "Newsgroups:"
                                         "Subject:"
                                         "Date:"
                                         "Followup-To:"
                                         "Reply-To:"
                                         "Organization:"
                                         "Summary:"
                                         "Keywords:"
                                         "Mail-Copies-To:"
                                         "To:"
                                         "Cc:"
                                         "BCC:"
                                         "X-Newsreader:"
                                         "X-Mailer:"
                                         "X-Sent:"
                                         "Posted-To:"
                                         "Mail-Copies-To:"
                                         "Apparently-To:"
                                         "Gnus-Warning:"
                                         "Resent-From:"
                                         "gpg-key-ID:"
                                         "fingerprint:"
                                         "X-Jabber-ID:"
                                         "User-Agent:")))

;; Ensure that whenever we compose new mail, this mail will use the correct
;; posting style.  This is ensured by setting ARG of `gnus-group-mail’ to 1 to
;; let it query the user for a group.
(defadvice gnus-group-mail (before inhibit-no-argument activate)
  (unless (ad-get-arg 0)
    (ad-set-arg 0 1)))

(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
(add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)

(add-hook 'kill-emacs-hook
          #'(lambda ()
              (interactive)
              (when (get-buffer "*Group*")
                (gnus-group-exit))))

(bind-key "q" #'gnus-summary-expand-window gnus-article-mode-map)


;;; Appearence

(setq gnus-group-line-format "%S%p%P%5y(%2i):%B%(%s:%G%)\n"
      gnus-auto-select-first nil
      gnus-auto-select-next nil)

(setq gnus-summary-line-format "%U%O%R%6k %(%&user-date;  %-13,13f  %B%s%)\n"
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-subthread-sort-functions '(gnus-thread-sort-by-date)
      gnus-thread-hide-subtree t
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
      gnus-sum-thread-tree-indent          "  "
      gnus-sum-thread-tree-root            "● "
      gnus-sum-thread-tree-false-root      "◎ "
      gnus-sum-thread-tree-single-indent   "◯ "
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-vertical "│"
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references

      ;; Yay (seen here: `https://github.com/cofi/dotfiles/blob/master/gnus.el')
      gnus-ancient-mark ?✓
      ;; gnus-cached-mark ?☍
      gnus-canceled-mark ?↗
      gnus-del-mark ?✗
      ;; gnus-dormant-mark ?⚐
      gnus-expirable-mark ?♻
      gnus-forwarded-mark ?↪
      ;; gnus-killed-mark ?☠
      ;; gnus-process-mark ?⚙
      gnus-read-mark ?✓
      gnus-recent-mark ?✩
      gnus-replied-mark ?↺
      gnus-unread-mark ?✉
      ;; gnus-unseen-mark ?★
      ;; gnus-ticked-mark ?⚑
      )

;; we need to do some magic as otherwise the agent does not delete articles from
;; its .overview when we move them around
(defadvice gnus-summary-move-article (around
                                      no-cancel-mark
                                      (&optional n to-newsgroup
                                                 select-method action)
                                      activate)
  (let ((articles (gnus-summary-work-articles n))
        (return   ad-do-it))
    (when (or (null action)
              (eq action 'move))
      (dolist (article articles)
        (gnus-summary-mark-article article gnus-expirable-mark)))
    return))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-treat-hide-boring-headers 'head
      gnus-treat-strip-multiple-blank-lines nil
      gnus-treat-display-smileys t
      gnus-treat-emphasize 'head
      gnus-treat-unsplit-urls t)


;;; Adaptive Scoring

(setq gnus-use-scoring nil
      gnus-use-adaptive-scoring nil
      gnus-adaptive-word-length-limit 5
      gnus-adaptive-word-no-group-words t
      gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 4))
        (gnus-dormant-mark (from 5))
        (gnus-del-mark (from -4) (subject -1))
        (gnus-read-mark (from 4) (subject 2))
        (gnus-expirable-mark (from -1) (subject -1))
        (gnus-killed-mark (from -1) (subject -3))
        (gnus-kill-file-mark)
        (gnus-ancient-mark)
        (gnus-low-score-mark)
        (gnus-catchup-mark (from -1) (subject -1))))

(setq-default gnus-summary-mark-below nil)

(setq gnus-parameters
      '(("^nnimap.*"
         (gnus-use-scoring nil))
        ("^nnimap.*:lists.*"
         (gnus-use-scoring t)
         (gnus-use-adaptive-scoring '(word line)))
        ("^nntp.*"
         (gnus-use-scoring nil)
         (gnus-summary-mark-below -1000)
         (gnus-use-adaptive-scoring '(word line)))))

(add-hook 'gnus-summary-exit-hook
          'gnus-summary-bubble-group)


;;; Gnus Registry

(require 'gnus-registry)

(setq gnus-registry-split-strategy 'majority
      gnus-registry-ignored-groups '(("^nntp" t)
                                     ("^nnfolder" t)
                                     ("^nnir" t)
                                     ("^nnmaildir" t)
                                     ("INBOX$" t))
      gnus-registry-max-entries 40000
      gnus-registry-track-extra '(sender subject recipient)
      gnus-registry-cache-file (expand-file-name "gnus.registry.eioioi"
                                                 emacs-d)
      gnus-refer-article-method 'current)

(gnus-registry-initialize)


;;; MIME

(setq gnus-ignored-mime-types '("text/x-vcard")
      message-forward-as-mime t
      gnus-inhibit-mime-unbuttonizing nil
      gnus-buttonized-mime-types '("multipart/signed" "multipart/encrypted")
      gnus-inhibit-images t
      gnus-blocked-images ".")

(setq message-citation-line-function
      (lambda ()
        (when message-reply-headers
          (insert "ghItlhpu' " (mail-header-from message-reply-headers) ":")
          (newline))))


;;; Signing and Encryption

(setq mm-encrypt-option nil
      mm-sign-option nil
      mm-decrypt-option 'known
      mm-verify-option 'known
      mml-smime-use 'epg
      ;;mml2015-encrypt-to-self t
      mml2015-display-key-image nil
      gnus-message-replysign t
      gnus-message-replyencrypt t
      gnus-message-replysignencrypted t
      mml-secure-cache-passphrase nil)

;; Automatic encryption if all necessary keys are present

(require 'mail-extr)

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
              (mml-secure-message (first methods) 'signencrypt)

            ;; go through available keys
            (let ((available-method
                   (cl-find-if (lambda (method)
                                 (db/encryption-possible-p recipients method))
                               methods)))

              (if available-method
                  (mml-secure-message available-method 'signencrypt)

                ;; if nothing works, sign with default method
                (mml-secure-message (first methods) 'sign)))))))))

(add-hook 'gnus-message-setup-hook
          #'db/signencrypt-message-when-possible)


;;; Custom commands

;; Visit group under point and immediately close it; this updates gnus’ registry
;; as a side-effect
(bind-key "v u"
          '(lambda ()
             (interactive)
             (save-mark-and-excursion
               (when (gnus-topic-select-group)
                 (gnus-summary-exit))))
          gnus-group-mode-map)

;; Toggle visibility of News group
(bind-key "v c"
          (lambda ()
            (interactive)
            (save-mark-and-excursion
              (gnus-topic-jump-to-topic "News")
              (gnus-topic-read-group)))
          gnus-group-mode-map)

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

(bind-key "C-<return>" #'db/gnus-summary-open-Link gnus-summary-mode-map)
(bind-key "C-<return>" #'db/gnus-summary-open-Link gnus-article-mode-map)

(defun db/gnus-html-mime-part-to-org ()
  "Convert current gnus article to org mode."
  (interactive)
  (let ((return-code (gnus-mime-pipe-part "pandoc -f html -t org")))
    (unless (zerop return-code)
      (error "Error in extracting text"))
    (with-current-buffer "*Shell Command Output*"
      (kill-ring-save (point-min) (point-max)))))


;;; Daemons

(defun db/gnus-demon-scan-news-on-level-2 ()
  "Scan for news in Gnus on level 2."
  ;; from https://www.emacswiki.org/emacs/GnusDemon
  (let ((win (current-window-configuration))
        (gnus-read-active-file 'some)
        (gnus-check-new-newsgroups nil)
        (gnus-verbose 2)
        (gnus-verbose-backends 5)
        (level 2))
    (while-no-input
      (unwind-protect
           (save-window-excursion
             (when (gnus-alive-p)
               (with-current-buffer gnus-group-buffer
                 (gnus-group-get-new-news level))))
        (set-window-configuration win)))))

(gnus-demon-add-handler 'db/gnus-demon-scan-news-on-level-2 5 5)


;;; Agents

(setq gnus-agent-mark-unread-after-downloaded nil
      gnus-agent-synchronize-flags t
      gnus-agent-go-online t)


;;; Do some pretty printing before saving the newsrc file

(defun db/gnus-save-newsrc-with-whitespace-1 ()
  "Save ~/.newsrc.eld with extra whitespace."
  ;; http://ding.gnus.narkive.com/pq3Z8ZjQ/pretty-printing-newsrc-eld#post3
  (gnus-message 5 "Adding whitespace to .newsrc.eld")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "(\\\"\\| ((\\| (nn" nil t)
      (replace-match "\n \\&" t))
    (delete-trailing-whitespace)))

(add-hook 'gnus-save-quick-newsrc-hook #'db/gnus-save-newsrc-with-whitespace-1)


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


;;; Archiving

;; We store messages in the current group, so there is no need to use Gnus’
;; archiving method

(setq gnus-message-archive-method nil
      gnus-update-message-archive-method t
      gnus-message-archive-group nil
      gnus-gcc-mark-as-read t)


;;; SMTP configuration

(require 'smtpmail)
(require 'starttls)

(defadvice smtpmail-send-it (around display-trace-buffer disable)
  "If an error is signalled, display the process buffer."
  (condition-case signals-data
      ad-do-it
    (error (shrink-window-if-larger-than-buffer
            (display-buffer (get-buffer (format "*trace of SMTP session to %s*"
                                                smtpmail-smtp-server))))
           (signal (car signals-data) (cdr signals-data)))))

(setq send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      starttls-use-gnutls t
      starttls-extra-arguments '("--strict-tofu")
      smtpmail-smtp-server (nth 3 (car db/mail-accounts))
      smtpmail-smtp-user (nth 6 (car db/mail-accounts)))

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
            (apply orig-fun args)))
      (progn
        (message "Sending with default account settings")
        (apply orig-fun args)))))

(advice-add 'smtpmail-via-smtp
            :around #'db/set-smtp-server-from-header)

(setq smtpmail-debug-info t)


;;; Notmuch

(require 'nnir)

(setq nnir-method-default-engines '((nnimap . imap)
                                    (nnmaildir . notmuch)
                                    (nntp . gmane)))

(use-package notmuch
  :init (progn
          (setq notmuch-fcc-dirs nil)))

;;;

t
