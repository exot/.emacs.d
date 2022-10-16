;;; db-projects.el -- Simple Directory-Based Project Management -*- lexical-binding: t -*-

;; TODO: this package is deprecated and should be replaced or at least built
;; upon either project.el or projectile.

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
  "Simple directory-based project management."
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
                    (directory-files projects-main-project-directory
                                     nil "^[^.]")))

;;;###autoload
(defun projects-add-project (short-name)
  "Add new project with SHORT-NAME.
The project directory will be located under
`projects-main-project-directory' within a directory named
SHORT-NAME."
  (interactive "sShort Name: ")
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
    (if-let ((git-executable (executable-find "git")))
        (call-process git-executable nil nil nil "init")
      (write-region "" nil (expand-file-name ".projectile")))
    (projectile-add-known-project project-directory)))

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

  (let ((project-path (expand-file-name short-name projects-main-project-directory))
        (archive-path (expand-file-name short-name projects-archive-directory)))

    (when (file-exists-p archive-path)
      (user-error "Archived project named %s already exists, aborting" short-name))

    ;; Move project directory into archive
    (unless (file-exists-p projects-archive-directory)
      (make-directory projects-archive-directory))
    (rename-file project-path archive-path nil)

    ;; Update projectile’s cache
    (projectile-cleanup-known-projects)))

(defun projects--org-files ()
  "Return all Org Mode files in all known projects, recursively.
Paths returned are absolute, but may not be canonical."
  (mapcan #'(lambda (dir)
              (directory-files-recursively (expand-file-name dir projects-main-project-directory)
                                           ".*\\.org" nil))
          (projects-existing-projects)))

;; Let's keep the byte compiler happy
(defvar org-agenda-text-search-extra-files)
(defvar org-agenda-files)

(defun projects-find-unsearched-org-files ()
  "Find Org Mode files in known projects that are not searched by default.
This is done by checking all org Mode files in every project
whether it is included in `org-agenda-text-search-extra-files' or
in `org-agenda-files'."
  (require 'org)
  (let ((extra-files (make-hash-table :test #'equal)))
    (mapc #'(lambda (entry)
              (when (stringp entry)
                (puthash (file-truename entry) t extra-files)))
          (append org-agenda-files org-agenda-text-search-extra-files))
    (cl-remove-if #'(lambda (org-file)
                      (gethash (file-truename org-file) extra-files nil))
                  (projects--org-files))))

;;;###autoload
(defun projects-lint-projects ()
  "Check all known projects for proper configuration.
This includes checking whether all bookmarks are in place and
whether `org-agenda-text-search-extra-files' is set up to search
through all included Org Mode files."
  (interactive)
  (with-current-buffer (get-buffer-create " *projects lint results*")

    (erase-buffer)

    (when-let ((unsearched-org-files (projects-find-unsearched-org-files)))
      (insert "The following Org Mode files are not included in `org-agenda-text-search-extra-files'; you may want to add them.")
      (dolist (file unsearched-org-files)
        (insert "\n  " file))
      (insert "\n\n"))

    (display-buffer (current-buffer))))

(provide 'db-projects)

;;; db-projects.el ends here
