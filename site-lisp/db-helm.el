(require 'helm-files)
(require 'helm-bookmark)
(require 'helm-source)
(require 'helm)

(defvar db/helm-source-frequently-used-features
  (helm-make-source "Frequently Used" 'helm-source-sync
    :candidates #'db/frequently-used-features
    :action '(("Open" . funcall))
    :filtered-candidate-transformer #'helm-adaptive-sort) ; effect?
  "Helm source for `db/helm-frequently-used-features’.")

(defun db/important-documents ()
  "Recursively return paths of all files found in `db/important-documents-path’.
The result will be a list of cons cells, where the car is the
path relative to `db/important-documents’ and the cdr is the full
path."
  ;; code adapted from `directory-files-recursively’
  (let ((db/important-documents-path (expand-file-name db/important-documents-path)))
    (cl-labels ((all-files-in-dir (dir)
                 (let ((result nil)
                       (files nil))
                   (dolist (file (sort (file-name-all-completions "" dir)
                                       'string<))
                     (unless (eq ?. (aref file 0)) ; omit hidden files
                       (if (directory-name-p file)
                           (let* ((leaf (substring file 0 (1- (length file))))
                                  (full-file (expand-file-name leaf dir)))
                             ;; Don't follow symlinks to other directories.
                             (unless (file-symlink-p full-file)
                               (setq result
                                     (nconc result (all-files-in-dir full-file)))))
                           (push (cons
                                  (string-remove-prefix db/important-documents-path
                                                        (expand-file-name file dir))
                                  (expand-file-name file dir))
                                 files))))
                   (nconc result (nreverse files)))))
      (when (file-directory-p db/important-documents-path)
        (all-files-in-dir db/important-documents-path)))))

(defvar db/helm-source-important-documents
  (helm-make-source "Important files" 'helm-source-sync
    :candidates #'db/important-documents
    :action '(("Open externally" . db/system-open)
              ("Find file" . find-file)))
  "Helm source for important documents.")

(defun db/helm-shortcuts (arg)
  "Open helm completion on common locations."
  (interactive "p")
  (helm :sources `(db/helm-source-frequently-used-features
                   ,(when (and (= arg 4)
                               (file-directory-p db/important-documents-path))
                      'db/helm-source-important-documents)
                   helm-source-bookmarks
                   helm-source-bookmark-set)))

(provide 'db-helm)
