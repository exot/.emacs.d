;;; db-customize.el --- Custom variables  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gnutls)



(defgroup personal-settings nil
  "A bunch of functions and variables for personalizing Emacs."
  :prefix "db/"
  :group 'convenience
  :group 'help
  :tag "Personal settings")

(defcustom db/jabber-id ""
  "Personal XMPP ID."
  :group 'personal-settings
  :type 'string)

(defcustom db/matrix-user-id ""
  "Main Matrix User ID."
  :group 'personal-settings
  :type 'string)

(defcustom db/matrix-password-store-entry ""
  "Password Store entry for Matrix User ID in `db/matrix-user-id'."
  :group 'personal-settings
  :type 'string)

(defcustom db/important-documents-path "~/Documents/library/"
  "Path to look for important documents.

These documents can then be listed in extended search commands
like `db/helm-shortcuts’."
  :group 'personal-settings
  :type 'string)

(defcustom db/path-to-onenote "c:/Program Files (x86)/Microsoft Office/Office15/ONENOTE.EXE"
  "Path to OneNote executable, for opening corresponding Org mode links."
  :group 'personal-settings
  :type 'file)

(defcustom db/path-to-outlook "c:/Program Files (x86)/Microsoft Office/Office15/OUTLOOK.EXE"
  "Path to Outlook executable, for opening corresponding Org mode links."
  :group 'personal-settings
  :type 'file)

(defun db/update-cert-file-directory (symbol new-value)
  "Set SYMBOL to NEW-VALUE and add all certificate in it to `gnutls-trustfiles’.

Assumes that NEW-VALUE points to a directory, and certificates
are assumed to be of the form *.crt."
  (set symbol new-value)
  (when (file-directory-p new-value)
    (dolist (cert-file (directory-files new-value t ".crt$"))
      (add-to-list 'gnutls-trustfiles cert-file))))

(defcustom db/cert-file-directory "~/.local/etc/certs/"
  "Local directory with additional certificates."
  :group 'personal-settings
  :type 'string
  :set #'db/update-cert-file-directory)

(defcustom db/rfc-cache-path nil
  "Path where documents are automatically downloaded to when opening rfc: links.

If this path is not set, i.e., is null, no automatic download will happen."
  :group 'personal-settings
  :type '(choice (const nil) file))

(defcustom db/after-init-load-files nil
  "A list of files to be loaded by `db/run-init' as the last step."
  :group 'personal-settings
  :type '(repeat file))



(defcustom org-working-task-id ""
  "Task ID of default working task."
  :group 'personal-settings
  :type 'string)

(defcustom org-break-task-id ""
  "Task ID of default break task."
  :group 'personal-settings
  :type 'string)

(defcustom org-home-task-id ""
  "Task ID of default home task."
  :group 'personal-settings
  :type 'string)

(defcustom db/org-clock-current-task-file "~/.org-current-task"
  "File to save the currently clocked in task to."
  :group 'personal-settings
  :type 'string)



;; NB: some of those files should also be elements of `org-agenda-files', but
;; this is not done automatically.  The reason is that automatically changing
;; `org-agenda-files' when setting those variables may conflict with the
;; customization of `org-agenda-files' itself.  Thus, when setting one of those
;; variables would update `org-agenda-files' (possibly saving the customiztion),
;; the original value of `org-agenda-files' would be gone.  Conversely, loading
;; the customization for `org-agenda-files' would overwrite the work done by
;; custom setters.  Thus, the only reasonable thing to do is to not update
;; `org-agenda-files' automatically and leave it to the user to update it.

(defcustom db/org-default-org-file nil
  "Path to default org-mode file for general use.
You may also want to add this file to `org-agenda-files'."
  :group 'personal-settings
  :type '(choice (const nil) file))

(defcustom db/org-default-work-file nil
  "Path to default org-mode file at work.
You may also want to add this file to `org-agenda-files'."
  :group 'personal-settings
  :type '(choice (const nil) file))

(defcustom db/org-default-home-file nil
  "Path to default org-mode file at home.
You may also want to add this file to `org-agenda-files'."
  :group 'personal-settings
  :type '(choice (const nil) file))

(defcustom db/org-default-notes-file nil
  "Path to default org-mode file for notes.
You may also want to add this file to `org-agenda-files'."
  :group 'personal-settings
  :type '(choice (const nil) file))

(defcustom db/org-default-refile-file nil
  "Path to default org-mode file for capturing.
This file is used by `org-agenda' to query for tasks that need to
be refiled, independently of whether it's part of
`org-agenda-files' or not.  You may still want to add this file
to `org-agenda-files' to have appointments, deadlines, etc shown
in the main agenda view."
  :group 'personal-settings
  :type '(choice (const nil) file))

(defcustom db/org-default-pensieve-file nil
  "Path to default org-mode file for private notes."
  :group 'personal-settings
  :type '(choice (const nil) file))

(defvar db/frequently-used-features-map (make-sparse-keymap)
  "Functions from `db/frequently-used-features' bound to shortcuts.")

(defcustom db/frequently-used-features
  '(("Mail" ?m db/gnus)
    ("Agenda" ?a db/org-agenda)
    ("Init File" ?i db/find-user-init-file)
    ("Main Org File" ?o (lambda () (interactive) (find-file db/org-default-org-file)))
    ("EMMS" ?M emms)
    ("Shell" ?s project-shell)
    ("EShell" ?e project-eshell)
    ("Refile File" ?r (lambda () (interactive) (find-file db/org-default-refile-file)))
    ("Goto Currnet Clock" ?c db/org-clock-goto-first-open-checkbox)
    ("Info Lookup" ?I info-lookup-symbol)
    ("Unicode Lookup" ?U insert-char)
    ("Timeline of Day" ?T timeline-tools-format-timeline-of-day)
    ("Copy template to point" ?C db/org-insert-checklist))
  "Mapping of frequently used features to functions implementing
them.  Can be used in application shortcuts such as
`db/helm-shortcuts’.  Each entry is a list of three items: a
short description, a shortcut character, and the function to
call.  Customizing this variable redefines the global
`hydra-feature-shortcuts'.  Instead of a shortcut character, nil
can be chosen, in which case no entry in the
`hydra-feature-shortcuts' will be generated."
  :group 'personal-settings
  :type  '(repeat (list string (choice character (const nil)) function))
  :set #'(lambda (symbol value)
           (set-default symbol value)
           (setq db/frequently-used-features-map (make-sparse-keymap))
           (mapc #'(lambda (entry)
                     (pcase-let ((`(_ ,shortcut ,function) entry))
                       (keymap-set db/frequently-used-features-map (string shortcut) function)))
                 db/frequently-used-features)))



(provide 'db-customize)

;;; db-customize ends here
