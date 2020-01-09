;;; db-utils.el --- Utility Functions for Daniel's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some functions used in my ~/.emacs.d/init.el.  Most of them are copied from
;; various sources around the internet.
;;

;;; Code:

(require 'subr-x)


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
      (let ((current-dir (expand-file-name default-directory))
            (height      (/ (window-total-height) 3)))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell 1)
        (when arg
          (end-of-line)
          (eshell-kill-input)
          (insert (format "cd '%s'" current-dir))
          (eshell-send-input))))))

(defun db/run-or-hide-shell ()
  "Opens an shell buffer if not already in one, and otherwise
  returns to where we have been before."
  (interactive "")
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
      (error "No link found"))))

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
  "Convert HEX-STRING to its ASCII equivalent.
Allowed characters in hex-string are hexadecimal digits and
whitespaces.  If region is active, replace region by the
corresponding ASCII string, otherwise query for input and display
the result in the minibuffer."
  ;; https://stackoverflow.com/questions/12003231/how-do-i-convert-a-string-of-hex-into-ascii-using-elisp
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "String (hex): "))))
  (cl-assert (not (string-match-p "[^A-Fa-e0-9 \t\n]" hex-string))
             "String contains invalid characters.")
  (require 'dash)
  (let ((result (->> hex-string
                     (replace-regexp-in-string "[ \t\n]" "")
                     (string-to-list)
                     (-partition 2)
                     (--map (string-to-number (concat it) 16))
                     concat)))
    (if (use-region-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (insert result))
      (message result))))

(defun db/text-to-hex (text-string)
  "Convert TEXT-STRING to its hexadecimal representation.
This function will return hexadecimal numbers with more than two
digits if the input string contains wide characters.  The result
might depend on the coding system of the current buffer."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "String (ascii): "))))
  (require 'dash)
  (let ((result (->> text-string
                     (--map (format "%2X " it))
                     (apply #'concat)
                     (string-trim-right))))
    (if (use-region-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (insert result))
      (message result))))

(defun db/ntp-to-time (high low &optional format-string)
  "Format NTP time given by HIGH and LOW (both hex strings) to time as given by FORMAT-STRING.
If not given, FORMAT-STRING defaults to some ISO 8601-like format."
  (interactive
   (list (string-to-number (read-string "High (hex): ") 16)
         (string-to-number (read-string "Log (hex): ") 16)))
  (let* ((high-seconds (- high 2208992400)) ; subtract seconds between 1900-01-01 and the epoch
         (h (lsh high-seconds -16))
  (let* ((high-seconds (- high 2208988800)) ; subtract seconds between 1900-01-01 and the epoch
         (h (if (< high-seconds 0)
                (- (lsh (- high-seconds) -16))
              (lsh high-seconds -16)))
         (l (% high-seconds 65536))
         (u (floor (* (/ low 4294967296.0) 1e6)))
         (p (- low (floor (/ (* u 4294967296) 1e6)))))
    (message
     (format-time-string (or format-string "%FT%H:%M:%S.%9NZ")
                         (list h l u p)
                         t))))

(defun conditionally-enable-lispy ()
  "Enable lispy-mode when in `eval-expression’ or in
`pp-eval-expression’.  lispy must have been loaded for this
first, i.e., this function will not automatically load
lispy."
  (when (and (featurep 'lispy)
             (or (eq this-command 'eval-expression)
                 (eq this-command 'pp-eval-expression)))
    (lispy-mode 1)))

(defun turn-on-lispy-when-available ()
  "Activate `lispy’ in current buffer when possible.
Will print a warning in case of failure."
  (interactive)
  (with-demoted-errors "Cannot activate lispy: %s"
    (require 'lispy)
    (lispy-mode)))

(defun db/sort-nsm-permanent-settings ()
  "Sort values in `nsm-permanent-host-settings’."
  (setq nsm-permanent-host-settings
        (cl-sort nsm-permanent-host-settings
                 #'string<
                 :key #'cl-second)))

(defun db/update-cert-file-directory (symbol new-value)
  "Set SYMBOL to NEW-VALUE and add all certificate in it to `gnutls-trustfiles’.

Assumes that NEW-VALUE points to a directory, and certificates
are assumed to be of the form *.crt."
  (set symbol new-value)
  (require 'gnutls)
  (when (file-directory-p new-value)
    (dolist (cert-file (directory-files new-value t ".crt$"))
      (add-to-list 'gnutls-trustfiles cert-file))))

(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun db/add-use-package-to-imenu ()
  "Add `use-package’ statements to `imenu-generic-expression."
  (add-to-list 'imenu-generic-expression
               '("Used Packages"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)"
                 2)))

(defun db/turn-off-local-electric-pair-mode ()
  "Locally turn off electric pair mode."
  (interactive)
  (electric-pair-local-mode -1))

(defun db/pretty-print-xml ()
  "Stupid function to pretty print XML content in current buffer."
  ;; We assume that < and > only occur as XML tag delimiters, not in strings;
  ;; this function is not Unicode-safe
  (interactive)

  (unless (eq major-mode 'nxml-mode)
    (require 'nxml-mode)
    (nxml-mode))

  (save-mark-and-excursion

   ;; First make it all into one line
   (goto-char (point-min))
   (while (re-search-forward "\n[\t ]*" nil 'no-error)
     ;; In case there was a space, we have to keep at least one as a separator
     (if (save-match-data (looking-back "[\t ]" 1))
         (replace-match " ")
       (replace-match "")))

   ;; Next break between tags
   (goto-char (point-min))
   (while (re-search-forward ">[\t ]*<" nil 'no-error)
     (replace-match ">\n<"))

   ;; Move opening and closing tags to same line in case there’s nothing in
   ;; between
   (goto-char (point-min))
   (while (re-search-forward "<\\([^>]*\\)>\n</\\1>" nil 'no-error)
     (replace-match "<\\1></\\1>"))

   ;; Indent
   (indent-region (point-min) (point-max))))

(defun db/lookup-smime-key (mail)
  "Look up `MAIL' on ldap-server of the DFN.

If found, imports the certificate via gpgsm."
  ;; inspired by https://www.emacswiki.org/emacs/ExtendSMIME
  (interactive "sMail: ")
  (require 'ldap)
  (when (get-buffer " *ldap-value*")
    (kill-buffer " *ldap-value*"))
  (ldap-search (format "(mail=%s)" mail))
  (let ((bufval (get-buffer " *ldap-value*")))
    (when bufval
      (with-current-buffer bufval
        (save-restriction
          (widen)                       ; just to be sure
          (let ((result (call-process-region (point-min) (point-max)
                                             "gpgsm"
                                             nil nil nil
                                             "--import")))
            (if (zerop result)
                (message "Successfully imported certificate for <%s>" mail)
              (error "Could not import certificate for <%s>" mail))))))))

;; https://emacs.stackexchange.com/questions/3089/how-can-i-create-a-dired-buffer-listing-all-open-files
;; https://emacs.stackexchange.com/questions/2567/programmatically-insert-files-into-dired-buffer
(defun db/dired-from-shell-command (command &optional directory)
  "Run COMMAND in DIRECTORY and display resulting list of files via `dired’.

COMMAND must be a shell command that produces a list of files as
output, separated by \\n, when called with
`shell-command-to-string’.  DIRECTORY defaults to
`default-directory’."
  (interactive "sCommand: ")
  (when (and directory (not (directory-name-p directory)))
    (user-error "Value for DIRECTORY is not a directory name: %s"
                directory))
  (let* ((default-directory (or directory default-directory))
         (list-of-files (cl-remove-if-not
                         (lambda (entry)
                           (and (not (string-empty-p entry))
                                (file-exists-p entry)
                                (file-readable-p entry)))
                         (split-string (shell-command-to-string command)
                                       "\n"))))
    (dired (cons "Command output" list-of-files))))

(defun db/system-open (path)
  "Open PATH with default program as defined by the underlying system."
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" path))
   ((eq system-type 'cygwin)
    (start-process "" nil "cygstart" path))
   (t
    (start-process "" nil "xdg-open" path))))


;;; Org Utilities

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

(defun db/find-csv-in-org (arg)
  "Interactively CSV find file and open it as Org mode table.
Default separator is \";\", but this can be changed interactively
by passing a universal argument."
  (interactive "P")
  (let ((separator (if arg (read-from-minibuffer "Separator (regular expression): ")
                     ";")))
    (call-interactively #'find-file)
    (org-mode)
    (org-table-convert-region (point-min) (point-max) separator)))

(defun db/org-mark-current-default-task ()
  "Mark current task as default when equal to work task or home task.
Work task and home task are determined by the current values of
`org-working-task-id’ and `org-home-task-id’, respectively."
  (let ((current-id (org-id-get org-clock-marker)))
    (when (member current-id (list org-working-task-id
                                   org-home-task-id))
      (org-clock-mark-default-task))))


;;; Calendar

(defun db/export-diary ()
  "Export diary.org as ics file to the current value of `org-icalendar-combined-agenda-file’.
This is done only if the value of this variable is not null."
  (interactive)
  (require 'ox-icalendar)
  (cond
   ((null org-icalendar-combined-agenda-file)
    (message "`org-icalendar-combined-agenda-file’ not set, not exporting diary."))
   ((not (file-name-absolute-p org-icalendar-combined-agenda-file))
    (user-error "`org-icalendar-combined-agenda-file’ not an absolute path, aborting."))
   (t
    (progn
      (org-save-all-org-buffers)
      (let ((org-agenda-files (cl-remove-if #'null
                                            (list db/org-default-home-file
                                                  db/org-default-work-file)))
            (org-agenda-new-buffers nil))
        ;; check whether we need to do something
        (when (cl-some (lambda (org-file)
                         (file-newer-than-file-p org-file
                                                 org-icalendar-combined-agenda-file))
                       org-agenda-files)
          (message "Exporting diary ...")
          ;; open files manually to avoid polluting `org-agenda-new-buffers’; we
          ;; don’t want these buffers to be closed after exporting
          (mapc #'find-file-noselect org-agenda-files)
          ;; actual export; calls `org-release-buffers’ and may thus close
          ;; buffers we want to keep around … which is why we set
          ;; `org-agenda-new-buffers’ to nil
          (when (file-exists-p org-icalendar-combined-agenda-file)
            (delete-file org-icalendar-combined-agenda-file)
            (sit-for 3))
          (org-icalendar-combine-agenda-files)
          (message "Exporting diary ... done.")))))))


;;; Extend Input Methods

(defun db/add-symbols-to-TeX-input-method ()
  "Add some new symbols to TeX input method."
  (when (string= current-input-method "TeX")
    (let ((quail-current-package (assoc "TeX" quail-package-alist)))
      (quail-define-rules
       ((append . t))
       ("\\land" ?∧)
       ("\\lor" ?∨)
       ("\\lnot" ?¬)
       ("\\implies" ?⇒)
       ("\\powerset" ?𝔓)
       ("\\mathbbK" ?𝕂)
       ("\\mathbbR" ?ℝ)
       ("\\mathbbN" ?ℕ)
       ("\\mathbbZ" ?ℤ)
       ("\\mathbbP" ?ℙ)
       ("\\mathcalA" ?𝒜)
       ("\\mathcalB" ?ℬ)
       ("\\mathcalC" ?𝒞)
       ("\\mathcalD" ?𝒟)
       ("\\mathcalE" ?ℰ)
       ("\\mathcalH" ?ℋ)
       ("\\mathcalI" ?ℐ)
       ("\\mathcalJ" ?𝒥)
       ("\\mathcalK" ?𝒦)
       ("\\mathcalL" ?ℒ)
       ("\\mathcalM" ?ℳ)
       ("\\mathcalR" ?ℛ)
       ("\\mathcalQ" ?𝒬)
       ("\\mathcalS" ?𝒮)
       ("\\mathfrakP" ?𝔓)))))


;;; Wrappers for external applications

(defun db/two-monitors-xrandr ()
  "Activate second monitor using xrandr."
  (call-process "xrandr" nil nil nil
                "--output" "HDMI-3" "--primary" "--right-of" "LVDS-1" "--auto"))

(defun db/one-monitor-xrandr ()
  "Deactivate all additional monitors."
  (call-process "xrandr" nil nil nil
                "--output" "HDMI-3" "--off"))

(defun db/org-onenote-open (path)
  "Visit OneNote document on PATH."
  (unless (file-executable-p db/path-to-onenote)
    (user-error "Path for OneNote is not executable, please customize `db/path-to-onenote’."))
  (start-process "OneNote" nil db/path-to-onenote "/hyperlink" path))

(defun db/org-outlook-open (id)
  "Open the Outlook item identified by ID.
  ID should be an Outlook GUID."
  (unless (file-executable-p db/path-to-outlook)
    (user-error "Path for Outlook is not executable, please customize `db/path-to-outlook’."))
  (w32-shell-execute "open" db/path-to-outlook (concat "/select outlook:" id)))

(defun db/org-rfc-open (number)
  "Open browser to show RFC of given NUMBER."
  (unless (string-match "[1-9][0-9]*" number)
    (user-error "Not a valid number for an RFC: %s" number))
  (browse-url (concat "https://tools.ietf.org/html/rfc" number)))


;;; Bookmarks

(defun db/bookmark-add-with-handler (name location handler)
  "Add NAME as bookmark to LOCATION and use HANDLER to open it.
HANDLER is a function receiving a single argument, namely
LOCATION.  If a bookmark named NAME is already present, replace
it.  The bookmarks will finally be sorted by their name."
  (setq bookmark-alist
        (cl-delete-if #'(lambda (bmk) (equal (car bmk) name))
                      bookmark-alist))
  (push `(,name
          (filename . ,location)
          (handler . ,#'(lambda (arg)
                          (funcall handler (cdr (assoc 'filename arg))))))
        bookmark-alist)
  (setq bookmark-alist (cl-sort bookmark-alist #'string-lessp :key #'car)))

(defun db/bookmark-add-external (location name)
  "Add NAME as bookmark to LOCATION that is opened by the operating system."
  (interactive "sLocation: \nsName: ")
  (db/bookmark-add-with-handler name location #'db/system-open))

(defun db/bookmark-add-url (url name)
  "Add NAME as bookmark to URL that is opened by `browse-url’."
  (interactive "sURL: \nsName: ")
  (db/bookmark-add-with-handler name url #'browse-url))

(defun db/bookmark-add-eww (url name)
  "Add NAME as bookmark to URL to be opened with `eww’."
  (interactive "sURL: \nsName: ")
  (db/bookmark-add-with-handler name url #'eww))


;;; End

(provide 'db-utils)

;;; db-utils.el ends here
