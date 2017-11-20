;;; db-utils.el --- Utility Functions for Daniel's Emacs Configuration

;;; Commentary:
;;
;; Some functions used in my ~/.emacs.d/init.el.  Most of them are copied from
;; various sources around the internet.
;;

;;; Code:

(require 'dash)


;;; application shortcuts

(defun db/run-or-hide-ansi-term ()
  "Find `*ansi-term*' or run `ansi-term' with `explicit-shell-file-name'.
If already in `*ansi-term*' buffer, bury it."
  (interactive)
  (if (string= "term-mode" major-mode)
      (bury-buffer)
      (if (get-buffer "*ansi-term*")
          (switch-to-buffer "*ansi-term*")
          (ansi-term explicit-shell-file-name))))

(defun db/gnus ()
  "Switch to the `*Group*' buffer, starting `gnus' if not existent."
  (interactive)
  (require 'gnus)
  (if (get-buffer "*Group*")
      (switch-to-buffer "*Group*")
    (gnus)))

(defun db/org-agenda ()
  "Show the main `org-agenda'."
  (interactive)
  (org-agenda nil "A"))

(defun db/scratch ()
  "Switch to `*scratch*'."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun db/find-user-init-file ()
  "Edit `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun db/run-or-hide-eshell (arg)
  "Opens an eshell buffer if not already in one, and otherwise
  returns to where we have been before."
  ;; idea to split the current window is from
  ;; http://howardism.org/Technical/Emacs/eshell-fun.html
  (interactive "P")
  (if (string= "eshell-mode" major-mode)
      ;; bury buffer; reopen with current working directory if arg is given
      (progn
        (bury-buffer)
        (delete-window)
        (and arg (db/run-or-hide-eshell arg)))
    (if-let ((eshell-window (db/find-window-by-buffer-mode 'eshell-mode)))
        (select-window eshell-window)
      ;; open eshell
      (let ((current-dir (expand-file-name (dired-default-directory)))
            (height      (/ (window-total-height) 3)))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell 1)
        (when arg
          (end-of-line)
          (eshell-kill-input)
          (insert (format "cd %s" current-dir))
          (eshell-send-input))))))

(defun db/run-or-hide-shell (arg)
  "Opens an shell buffer if not already in one, and otherwise
  returns to where we have been before."
  (interactive "P")
  (if (string= "shell-mode" major-mode)
      (progn
        (bury-buffer)
        (other-window -1))
    (shell)))


;;; general utilities

(defun db/get-url-from-link ()
  "Copy url of link under point into clipboard."
  (interactive)
  (let ((url (plist-get (text-properties-at (point)) 'help-echo)))
    (if url
        (kill-new url)
      (error "No link found."))))

(defun db/test-emacs ()
  ;; from oremacs
  "Test whether emacs' configuration is not throwing any errors."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs.d/init.el\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

(defun db/isearch-forward-symbol-with-prefix (p)
  ;; http://endlessparentheses.com/quickly-search-for-occurrences-of-the-symbol-at-point.html
  "Like `isearch-forward', unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p
         #'isearch-forward-symbol-at-point
         #'isearch-forward))))

(defun db/go-dark ()
  "Enable dark themes."
  (interactive)
  (load-theme 'solarized-dark)
  (load-theme 'smart-mode-line-dark))

(defun db/go-light ()
  "Enable light themes."
  (interactive)
  (load-theme 'solarized-light)
  (load-theme 'smart-mode-line-light))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  ;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun db/delete-trailing-whitespace-maybe ()
  "Call `delete-trailing-whitespace', but not in `message-mode'."
  (unless (derived-mode-p 'message-mode)
    (delete-trailing-whitespace)))

(defun db/find-window-by-buffer-mode (mode)
  "Return first window in current frame displaying a buffer with
major mode MODE."
  (cl-find-if (lambda (window)
                (with-current-buffer (window-buffer window)
                  (eq major-mode mode)))
              (window-list-1)))

(defun db/show-current-org-task ()
  "Show title of currently clock in task in modeline."
  (interactive)
  (message org-clock-current-task))

(defun db/hex-to-ascii (hex-string)
  "Convert HEX-STRING to its ASCII equivalent."
  ;; https://stackoverflow.com/questions/12003231/how-do-i-convert-a-string-of-hex-into-ascii-using-elisp
  (interactive "sString (hex): ")
  (->> (string-to-list hex-string)
       (-partition 2)
       (--map (string-to-number (concat it) 16))
       concat
       message))

(defun db/ascii-to-hex (ascii-string)
  "Convert ASCII-STRING to its hexadecimal representation."
  (interactive "sString (ascii): ")
  (->> (string-to-list ascii-string)    ; redundant
       (--map (format "%2X" it))
       (apply #'concat)
       message))

(defun db/ntp-to-time (high low &optional format-string)
  "Format NTP time given by HIGH and LOW (both integer) to time as given by FORMAT-STRING.
If not given, FORMAT-STRING defaults to some ISO 8601-like format."
  (interactive
   (list (string-to-number (read-string "High (hex): ") 16)
         (string-to-number (read-string "Log (hex): ") 16)))
  (let* ((high-seconds (- high 2208992400)) ; subtract seconds between 1900-01-01 and the epoch
         (h (lsh high-seconds -16))
         (l (% high-seconds 65536))
         (u (floor (* (/ low 4294967296.0) 1e6)))
         (p (- low (floor (/ (* u 4294967296) 1e6)))))
    (message
     (format-time-string (or format-string "%Y-%m-%dT%H:%M:%S.%9NZ")
                         (list h l u p)))))


;;; dired

(defun dired-back-to-top ()
  "Jump to first non-trivial line in dired."
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(defun dired-jump-to-bottom ()
  "Jump to last non-trivial line in dired."
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun dired-get-size ()                ; from emacswiki, via oremacs
  "print size of all files marked in the current dired buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[0-9.,]+[a-za-z]+\\).*total$")
         (match-string 1))))))

(defun dired-open-term ()               ; from oremacs
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (term-send-string
     (db/ansi-term)
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))


;;; helm configuration

(defcustom db/helm-frequently-used-features
  '(("Mail"      . db/gnus)
    ("Agenda"    . db/org-agenda)
    ("Init File" . db/find-user-init-file)
    ("EMMS"      . emms)
    ("Gnus"      . (lambda ()
                     (interactive)
                     (find-file gnus-init-file)))
    ("Shell"     . shell)
    ("EShell"    . eshell)
    ("scratch"   . db/scratch))
  "Helm shortcuts for frequently used features."
  :group 'personal-settings
  :type  '(alist :key-type string :value-type sexp))

(defvar db/helm-source-frequently-used-features
  '((name . "Frequently Used")
    (candidates . db/helm-frequently-used-features)
    (action . (("Open" . funcall)))
    (filtered-candidate-transformer . helm-adaptive-sort))
  "Helm source for `db/helm-frequently-used-features’.")

(defcustom db/helm-frequently-visited-locations
  '(("db-utils" . "~/.emacs.d/site-lisp/db-utils.el")
    ("db-org" . "~/.emacs.d/site-lisp/db-org.el")
    ("db-private" . "~/.emacs.d/site-lisp/db-private.el")
    ("notes" . "~/Documents/home/notes.org")
    ("pensieve" . "~/Documents/home/pensieve.org.gpg")
    ("things (home)" . "~/Documents/home/admin/things.gpg")
    ("things (work)" . "~/Documents/uni/admin/misc/things.gpg"))
  "Helm shortcuts to frequentely visited locations"
  :group 'personal-settings
  :type  '(alist :key-type string :value-type sexp))

(defvar db/helm-source-frequently-visited-locations
  '((name . "Locations")
    (candidates . db/helm-frequently-visited-locations)
    (action . (("Open" . (lambda (entry)
                           (if (consp entry)
                               (funcall (car entry) (cdr entry))
                             (find-file entry))))))
    (filtered-candidate-transformer . helm-adaptive-sort)))

(defcustom db/important-documents-path "~/Documents/library/"
  "Path of important documents."
  :group 'personal-settings
  :type 'string)

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

(defun db/system-open (path)
  "Open PATH with default program as defined by the underlying system."
  (if on-windows
      (w32-shell-execute "open" path)
    (start-process "" nil "xdg-open" path)))

(defvar db/helm-source-important-documents
  '((name . "Important files")
    (candidates . db/important-documents)
    (action . (("Open externally" . db/system-open)
               ("Find file" . find-file))))
  "Helm source for important documents.")

(defun db/helm-shortcuts ()
  "Open helm completion on common locations."
  (interactive)
  (require 'helm-files)
  (require 'helm-bookmark)
  (helm :sources `(db/helm-source-frequently-used-features
                   db/helm-source-frequently-visited-locations
                   ,(when (file-directory-p db/important-documents-path)
                      'db/helm-source-important-documents)
                   helm-source-bookmarks
                   helm-source-bookmark-set)))


;;; Org Utilities

(defun db/bank-csv-to-org-table ()
  (interactive)
  (goto-char (point-min))
  (kill-line 8)
  (replace-regexp "^\"\\|\"$\\|\";\"" "|")
  (goto-char (point-min))
  (org-mode)
  (org-table-align)
  ;; move columns around
  (cl-loop
     for (word . count) in '(("Wertstellung" . 6) ("Umsatzart" . 6) ("Buchungsdetails" . 3))
     do (progn (goto-char (point-min))
               (search-forward word)
               (dotimes (i count)
                 (org-table-move-column-right))))
  (goto-char (point-min)))

(defun db/org-cleanup-continuous-clocks ()
  "Join continuous clock lines in the current buffer."
  (interactive)
  (let* ((inactive-timestamp (org-re-timestamp 'inactive))
         (clock-line (concat "\\(^ *\\)CLOCK: " inactive-timestamp "--" inactive-timestamp " => .*"
                             "\n"
                             " *CLOCK: " inactive-timestamp "--\\[\\2\\] => .*$")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp clock-line nil t)
       (replace-match "\\1CLOCK: [\\4]--[\\3]")
       (org-clock-update-time-maybe)))))


;;;

(provide 'db-utils)

;;; db-utils.el ends here
