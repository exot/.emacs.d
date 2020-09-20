;;; db-projects.el -- Simple Directory-Based Project Management -*- lexical-binding: t -*-

;;; Commentary:

;; A project is simply a directory under `projects-main-project-directory'
;; containing either .git or .projectile.  This little collection of functions
;; helps to manage these project directories and also integration them
;; consistently with the projectile package.

;; To start, first customize `projects-main-project-directory' and
;; `projects-archive-directory' as needed.  Then use `projects-add-project' to
;; add new projects and `projects-archive-project' to archive them (i.e., move
;; them to `projects-archive-directory').  This package does not offer to remove
;; projects; this has to be done manually.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'dash)
(require 'bookmark)
(require 'projectile)

(defgroup projects nil
  "Simple directory-based project management"
  :tag "Project Management"
  :group 'projects)

(defcustom projects-main-project-directory "~/Documents/projects/"
  "Main directory to host projects."
  :group 'projects
  :type 'directory)

(defcustom projects-archive-directory "~/Documents/projects/.archive/"
  "Directory to archive projects into."
  :group 'projects
  :type 'directory)

(defun projects-project-exists-p (short-name)
  "Check whether a project named SHORT-NAME already exists."
  (or
   (file-exists-p (expand-file-name (concat (file-name-as-directory short-name)
                                            ".git")
                                    projects-main-project-directory))
   (file-exists-p (expand-file-name (concat (file-name-as-directory short-name)
                                            ".projectile")
                                    projects-main-project-directory))))

(defun projects-existing-projects ()
  "Return list of all short-names of existing projects."
  (cl-remove-if-not #'projects-project-exists-p
                    (directory-files projects-main-project-directory)))

;;;###autoload
(defun projects-add-project (short-name long-name)
  "Add new project with SHORT-NAME and LONG-NAME.
The project directory will be located under
`projects-main-project-directory' within a directory named
SHORT-NAME.  A bookmark to the project diary will be created,
using the given LONG-NAME.  The project diary will be pre-filled
with some standard information like title and creation date."
  (interactive "sShort Name: \nsLong Name: ")
  (when (projects-project-exists-p short-name)
    (user-error "Project %s already exists, exiting" short-name))
  (when (file-exists-p (expand-file-name short-name
                                         projects-archive-directory))
    (user-error "Project %s already exists as archived project, exiting" short-name))
  (let* ((project-directory (expand-file-name short-name
                                              projects-main-project-directory))
         (default-directory project-directory))
    (make-directory project-directory)
    (make-directory (expand-file-name "scripts"))
    (make-directory (expand-file-name "data"))
    (with-temp-buffer
      (insert (format "#+title: %s\n" long-name))
      (insert (format "#+created: %s\n\n"
                      (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))))
      (write-file (expand-file-name "projekttagebuch.org"))
      (bookmark-set (format "Projekttagebuch %s" long-name)))
    (if-let ((git-executable (executable-find "git")))
        (call-process git-executable nil nil nil "init")
      (write-region "" nil (expand-file-name ".projectile")))
    (when (require 'projectile nil 'no-error)
      (projectile-add-known-project project-directory))))

(defun projects--find-bookmarks-for-path (path)
  "Find all bookmark names that point into PATH."
  (unless (file-name-absolute-p path)
    (user-error "Given path %s is not absolute" path))
  (let ((path (file-truename path)))
    (cl-remove-if-not #'(lambda (bmk)
                          (let ((filename (bookmark-get-filename bmk)))
                            (and (not (file-remote-p filename))
                                 (string-prefix-p path (file-truename filename)))))
                      (bookmark-all-names))))

;;;###autoload
(defun projects-archive-project (short-name)
  "Archive existing project identified by SHORT-NAME.
This amounts to moving the project directory SHORT-NAME under
`projects-main-project-directory' to
`projects-archive-directory', deleting all bookmarks into the
project, and updating projectile's cache."
  (interactive
   (list (completing-read "Short Name: " (projects-existing-projects) nil t)))
  (unless (projects-project-exists-p short-name)
    (user-error "Project %s does not exist, exiting" short-name))

  ;; Remove bookmarks first
  (mapc #'bookmark-delete (projects--find-bookmarks-for-path
                           (expand-file-name short-name projects-main-project-directory)))

  ;; Move project directory into archive
  (unless (file-exists-p projects-archive-directory)
    (make-directory projects-archive-directory))
  (rename-file (expand-file-name short-name projects-main-project-directory)
               (expand-file-name short-name projects-archive-directory)
               nil)

  ;; Update projectile’s cache
  (projectile-remove-known-project
   (expand-file-name short-name
                     projects-main-project-directory)))

(defun projects--org-files ()
  "Return all Org Mode files in all known projects, recursively."
  (mapcan #'(lambda (dir)
              (directory-files-recursively (expand-file-name dir projects-main-project-directory)
                                           ".*\\.org" nil))
          (projects-existing-projects)))

(defvar org-agenda-text-search-extra-files) ; to keep the byte-compiler happy

(defun projects-find-unsearched-org-files ()
  "Find Org Mode files in known projects that are not searched by default.
This is done by checking all org Mode files in every project
whether it is included in `org-agenda-text-search-extra-files'."
  (require 'org)
  (let ((extra-files (make-hash-table :test #'equal)))
    (mapc #'(lambda (entry)
              (when (stringp entry)
                (puthash (file-truename entry) t extra-files)))
          org-agenda-text-search-extra-files)
    (cl-remove-if #'(lambda (org-file)
                      (gethash (file-truename org-file) extra-files nil))
                  (projects--org-files))))

(defun projects-check-project-diary-bookmarks ()
  "Check that all known projects have a bookmark to their diary.
Return list of short names of projects whose project diary does
not have a corresponding bookmark."
  (let ((projects (make-hash-table :test #'equal)))

    ;; Make hash table of all diary paths to all known projects; as values we
    ;; keep the short names, because these are the ones we want to return in the
    ;; end
    (dolist (project (projects-existing-projects))
      (let ((project-diary-path (expand-file-name (concat (file-name-as-directory project)
                                                          "projekttagebuch.org")
                                                  projects-main-project-directory)))
        (when (file-exists-p project-diary-path)
          (puthash project-diary-path project projects))))

    ;; Delete all those diary links that have a bookmark
    (dolist (bmkn (bookmark-all-names))
      (unless (file-remote-p (bookmark-get-filename bmkn))
        (remhash (file-truename (bookmark-get-filename bmkn)) projects)))

    ;; Return remaining short names; those are the ones that do not have a
    ;; bookmark yet
    (hash-table-values projects)))

(provide 'db-projects)

;;; db-projects.el ends here
