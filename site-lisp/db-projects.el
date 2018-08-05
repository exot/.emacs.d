;;; db-projects.el -- Simple Directory-Based Project Management -*- lexical-binding: t -*-

;;; Commentary:

;; XXX: add projectile integration
;; XXX: check that newly created projects aren’t name the same as archived projects

;;; Code:

(defgroup projects nil
  "Simple directory-based project management"
  :tag "Project Management"
  :group 'projects)

(defcustom projects-main-project-directory "~/Documents/projects/"
  "Main directory to host projects."
  :group 'projects)

(defcustom projects-archive-directory "~/Documents/projects/.archive"
  "Directory to archive projects into"
  :group 'projects)

(defun projects-project-exists-p (short-name)
  "Check whether a project named SHORT-NAME already exists"
  (file-exists-p (expand-file-name ".projectile"
                                   (expand-file-name short-name
                                                     projects-main-project-directory))))

(defun projects-existing-projects ()
  "Return list of all short-names of existing projects"
  (cl-remove-if-not (lambda (name)
                      (file-exists-p (expand-file-name ".projectile"
                                                       (expand-file-name name projects-main-project-directory))))
                    (directory-files projects-main-project-directory)))

(defun projects-add-project (short-name long-name)
  "Add new project."
  (interactive "sShort Name: \nsLong Name: ")
  (when (projects-project-exists-p short-name)
    (user-error "Project %s already exists, existing" short-name))
  (let ((project-directory (expand-file-name short-name
                                             projects-main-project-directory)))
    (make-directory project-directory)
    (make-directory (expand-file-name "scripts" project-directory))
    (make-directory (expand-file-name "data" project-directory))
    (with-temp-buffer
      (insert (format "#+title: %s\n" long-name))
      (insert (format "#+created: %s\n\n"
                      (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))))
      (write-file (expand-file-name "projekttagebuch.org" project-directory))
      (bookmark-set (format "Projekttagebuch %s" short-name)))
    (with-temp-buffer
      (insert (format "%s" long-name))
      (write-file (expand-file-name ".projectile" project-directory)))))

(defun projects-archive-project (short-name)
  "Archive existing project."
  (interactive (list
                (completing-read "Short Name: " (projects-existing-projects) nil t)))
  (unless (projects/project-exists-p short-name)
    (user-error "Project %s does not exist, exiting" short-name))
  (unless (file-exists-p projects-archive-directory)
    (make-directory projects-archive-directory))
  (rename-file (expand-file-name short-name projects-main-project-directory)
               (expand-file-name short-name projects-archive-directory)
               nil)
  (bookmark-delete (format "Projekttagebuch %s" short-name)))

(provide 'db-projects)

;;; db-projects.el ends here
