;;; db-customize.el --- Custom variables  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gnutls)

(defgroup personal-settings nil
  "A bunch of functions and variables for personalizing emacs."
  :prefix "db/"
  :group 'convenience
  :group 'help
  :tag "Personal settings")

(defcustom db/jabber-id ""
  "Personal XMPP ID."
  :group 'personal-settings
  :type 'string)

(defcustom db/important-documents-path "~/Documents/library/"
  "Path to look for documents that can be listed in extended
search commands like `db/helm-shortcuts’."
  :group 'personal-settings
  :type 'string)

(defcustom db/path-to-onenote "c:/Program Files (x86)/Microsoft Office/Office15/ONENOTE.EXE"
  "Path to OneNote executable, for opening corresponding org-mode links."
  :group 'personal-settings
  :type 'file)

(defcustom db/path-to-outlook "c:/Program Files (x86)/Microsoft Office/Office15/OUTLOOK.EXE"
  "Path to Outlook executable, for opening corresponding org-mode links."
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
  "Path where RFC documents are automatically downloaded to when opening rfc: links.
If this path is not set, i.e., is null, no automatic download will happen."
  :group 'personal-settings
  :type '(choice (const nil) file))

(provide 'db-customize)

;;; db-customize ends here
