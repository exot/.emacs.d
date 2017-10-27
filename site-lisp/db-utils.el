;;; db-utils.el --- Utility Functions for Daniel's Emacs Configuration

;;; Commentary:
;;
;; Some functions used in my ~/.emacs.d/init.el.  Most of them are copied from
;; various sources around the internet.
;;

;;; Code:


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


;;; helpers

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

(defun db/get-mail (arg)
  "Use offlineimap to get emails from remote accounts.
With optional ARG, fetch only personal email addresses."
  (interactive "P")
  (if arg
      (message "Receiving Mail... (personal only)")
    (message "Receiving Mail..."))
  (let ((process (apply #'start-process
                        "offlineimap"
                        " *offlineimap*"
                        "systemctl"
                        `("--user" "start" ,(format  "offlineimap@%s" (getenv "DISPLAY"))))))
    (set-process-sentinel process
                          (lambda (process event)
                            (if (string= event "finished\n")
                                (progn (gnus-group-get-new-news 2)
                                       (message "Receiving Mail... done"))
                              (error "Receiving Mail... failed"))))))

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

(defun db/ssh-keys ()
  "Return list of private ssh keys available on the system."
  (cl-flet ((file-ssh-key-p (file)
              (string-suffix-p ": PEM RSA private key\n"
                               (shell-command-to-string (format "file %s" file)))))
    (cl-remove-if-not #'file-ssh-key-p (directory-files "~/.ssh/" t))))

(defun db/add-to-keyring (&optional file)
  "Add FILE to local keyring.
If FILE is not given, prompt for one."
  (interactive)
  (if file
      (let ((return-value (call-process "ssh-add" nil nil nil "-t" "86400" (expand-file-name file))))
        (unless (zerop return-value)
          (error "Aborted: %s" return-value)))
    (let ((ssh-keys `((name . "SSH Keys")
                      (candidates . ,(mapcar (lambda (file)
                                               (cons (file-name-nondirectory file) file))
                                             (db/ssh-keys)))
                      (action . (("Add to keyring" . db/add-to-keyring))))))
      (helm :sources (list ssh-keys)))))

(defun db/show-current-org-task ()
  "Show title of currently clock in task in modeline."
  (interactive)
  (message org-clock-current-task))


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
  '((name . "Frequently Used")
    (candidates . (("Mail"      . db/gnus)
                   ("Agenda"    . db/org-agenda)
                   ("Init File" . db/find-user-init-file)
                   ("EMMS"      . emms)
                   ("Gnus"      . (lambda ()
                                    (interactive)
                                    (find-file gnus-init-file)))
                   ("Shell"     . shell)
                   ("EShell"    . eshell)
                   ("scratch"   . db/scratch)))
    (action . (("Open" . funcall)))
    (filtered-candidate-transformer . helm-adaptive-sort))
  "Helm shortcuts for frequently used features."
  :group 'personal-settings
  :type  '(alist :key-type symbol :value-type sexp))

(defcustom db/helm-frequently-visited-locations
  '((name . "Locations")
    (candidates . (("db-utils" . "~/.emacs.d/site-lisp/db-utils.el")
                   ("db-org"   . "~/.emacs.d/site-lisp/db-org.el")
                   ("db-private" . "~/.emacs.d/site-lisp/db-private.el")
                   ("notes"    . "~/Documents/home/notes.org")
                   ("pensieve" . "~/Documents/home/pensieve.org.gpg")
                   ("things (home)" . "~/Documents/home/admin/things.gpg")
                   ("things (work)" . "~/Documents/uni/admin/misc/things.gpg")
                   ("research ideas" . "~/Documents/uni/research/ideas.org")
                   ("teaching ideas" . "~/Documents/uni/lehre/ideas.org")))
    (action . (("Open" . find-file)))
    (filtered-candidate-transformer . helm-adaptive-sort))
  "Helm shortcuts to frequentely visited locations"
  :group 'personal-settings
  :type  '(alist :key-type symbol :value-type sexp))

(defcustom db/important-documents-path "..." ; invalid directory as default
  "Path of important documents."
  :group 'personal-settings
  :type 'string)

(defun db/important-documents ()
  "Recursively return paths of all important documents found in `db/important-documents-path’."
  (when (file-directory-p db/important-documents-path)
    (mapcar (lambda (path)
              (cons (string-remove-prefix db/important-documents-path path)
                    path))
            (directory-files-recursively db/important-documents-path ""))))

(defun db/system-open (path)
  "Open PATH with default program as defined by the underlying system."
  (ecase system-type
    ((windows-nt cygwin) (w32-shell-execute "open" path))
    ((gnu/linux) (start-process "" nil "xdg-open" path))))

(defcustom db/helm-important-documents
  `((name . ,db/important-documents-path)
    (candidates . db/important-documents)
    (action . (("Open" . db/system-open))))
  "Helm source for important documents."
  :group 'personal-settings
  :type '(alist :key-type symbol :value-type sexp))

(defun db/helm-shortcuts ()
  "Open helm completion on common locations."
  (interactive)
  (require 'helm-files)
  (helm :sources '(db/helm-frequently-used-features
                   db/helm-frequently-visited-locations
                   db/helm-important-documents)))

(defun db/helm-shortcuts ()
  "Open helm completion on common locations."
  (interactive)
  (require 'helm-files)
  (helm :sources `(db/helm-frequently-used-features
                   db/helm-frequently-visited-locations
                   ,(when (file-directory-p db/important-documents-path)
                      'db/helm-important-documents))))


;;; Other Utilities

(defun db/bank-csv-to-org-table ()
  (interactive)
  (goto-char (point-min))
  (kill-line 8)
  (replace-regexp "^\"" "| ")
  (goto-char (point-min))
  (replace-regexp "\"$" " |")
  (goto-char (point-min))
  (replace-regexp "\";\"" " | ")
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
