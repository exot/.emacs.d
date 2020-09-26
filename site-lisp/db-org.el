;;; org.el -- Daniel's org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file defines functions used in the main configuration of org-mode and
;; it’s subpackages.  Nothing here changes the behavior of org-mode per se, as
;; loading this file only defines a couple of functions.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'hydra)
(require 'db-customize)
(require 'ox-icalendar)

(autoload 'counsel-org-goto-all "counsel")
(autoload 'which-function "which-func")
(autoload 'org-element-property "org-element")

(declare-function w32-shell-execute "w32fns.c")


;;; Agenda Customization

(defun db/check-special-org-files-in-agenda (&rest args)
  "Check whether the special org-mode files are part of `org-agenda-files', ignoring ARGS.
The special org-mode files are `db/org-default-org-file',
`db/org-default-work-file', `db/org-default-home-file', and
`db/org-default-refile-file'."
  (ignore args)
  (require 'org)
  (let ((agenda-files (mapcar #'file-truename (org-agenda-files t))))
    (dolist (file '(db/org-default-org-file
                    db/org-default-home-file
                    db/org-default-work-file
                    db/org-default-refile-file))
      (when (and (symbol-value file)
                 (not (member (file-truename (symbol-value file))
                              agenda-files)))
        (warn "File %s is not part of `org-agenda-files'."
              file)))))

(defun db/org-agenda-list-deadlines (&optional match)
  "Prepare agenda view that only lists upcoming deadlines.

Ignores MATCH.  Date is always today, no forward or backward is
supported.  Consequently, no date is shown.  Also does not
support any of the usual key bindings, e.g., showing a
clockreport.  It is, plainly speaking, just listing all active
deadlines."
  (interactive "P")
  (catch 'exit
    (org-agenda-prepare "Deadlines")
    (org-compile-prefix-format 'agenda)
    (org-set-sorting-strategy 'agenda)

    (let* ((today (org-today))
	   (thefiles (org-agenda-files nil 'ifmode))
	   (inhibit-redisplay (not debug-on-error))
	   s rtn rtnall file files date start-pos)

      ;; headline
      (unless org-agenda-compact-blocks
        (setq s (point))
        (if org-agenda-overriding-header
            (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                        nil 'face 'org-agenda-structure) "\n"))
	(org-agenda-mark-header-line s))

      ;; actual content
      (setq date (calendar-gregorian-from-absolute today)
            s (point)
            start-pos (point)
            files thefiles
            rtnall nil)
      (while (setq file (pop files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (setq rtn (apply 'org-agenda-get-day-entries
                           file date
                           '(:deadline)))
          (setq rtnall (append rtnall rtn)))) ;; all entries
      (when rtnall
        (insert (org-agenda-finalize-entries rtnall 'agenda)
                "\n"))

      ;; finalize
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (unless (and (pos-visible-in-window-p (point-min))
		   (pos-visible-in-window-p (point-max)))
	(goto-char (1- (point-max)))
	(recenter -1)
	(if (not (pos-visible-in-window-p (or start-pos 1)))
	    (progn
	      (goto-char (or start-pos 1))
	      (recenter 1))))
      (goto-char (or start-pos 1))
      (add-text-properties
       (point-min) (point-max)
       `(org-agenda-type agenda
                         org-redo-cmd
                         (db/org-agenda-list-deadlines ,match)))
      (org-agenda-finalize)
      (setq buffer-read-only t)
      (message ""))))

(defun db/org-agenda-skip-tag (tag &optional others)
  ;; https://stackoverflow.com/questions/10074016/org-mode-filter-on-tag-in-agenda-view
  "Skip all entries that correspond to TAG.

If OTHERS is true, skip all entries that do not correspond to TAG."
  (let* ((next-headline    (save-mark-and-excursion
                             (or (outline-next-heading) (point-max))))
         (current-headline (or (and (org-at-heading-p)
                                    (point))
                               (save-mark-and-excursion
                                 ;; remember to also consider invisible headings
                                 (org-back-to-heading t))))
         (has-tag          (member tag (org-get-tags current-headline))))
    (if (or (and others (not has-tag))
            (and (not others) has-tag))
        next-headline
      nil)))

;; A Hydra for changing agenda appearance
;; http://oremacs.com/2016/04/04/hydra-doc-syntax/

(defun db/org-agenda-span ()
  "Return the display span of the current shown agenda."
  (let ((args (get-text-property
               (min (1- (point-max)) (point))
               'org-last-args)))
    (nth 2 args)))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
_w_: ?w? week       _[_: inactive      _A_: arch-files
_t_: ?t? fortnight  _F_: follow=?F?    _r_: report=?r?
_m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
_y_: ?y? year       _q_: quit          _L__l__c_: ?l?

"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view
       (if (eq 'day (db/org-agenda-span))
           "[x]" "[ ]"))
  ("w" org-agenda-week-view
       (if (eq 'week (db/org-agenda-span))
           "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view
       (if (eq 'fortnight (db/org-agenda-span))
           "[x]" "[ ]"))
  ("m" org-agenda-month-view
       (if (eq 'month (db/org-agenda-span)) "[x]" "[ ]"))
  ("y" org-agenda-year-view
       (if (eq 'year (db/org-agenda-span)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode
       (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("F" org-agenda-follow-mode
       (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode
       (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode
       (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid
       (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary
       (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("["
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" (message "Abort") :exit t))


;;; Capturing

(defun db/org-timestamp-difference (stamp-1 stamp-2)
  "Returns time difference between two given org-mode timestamps."
  ;; Things copied from `org-clock-update-time-maybe’
  (let* ((s (-
             (float-time
              (apply #'encode-time (org-parse-time-string stamp-2 t)))
             (float-time
              (apply #'encode-time (org-parse-time-string stamp-1 t)))))
         (neg (< s 0))
         (s (abs s))
         (h (floor (/ s 3600)))
         (m (floor (/ (- s (* 3600 h)) 60))))
    (format (if neg "-%d:%02d" "%2d:%02d") h m)))

;; Capture Code Snippets
;; from http://ul.io/nb/2018/04/30/better-code-snippets-with-org-capture/
(defun db/org-capture-code-snippet (filename)
  "Format Org mode entry for capturing code in active region in
the buffer visiting FILENAME."
  (with-current-buffer (find-buffer-visiting filename)
    (let ((code-snippet (buffer-substring-no-properties (mark) (- (point) 1)))
          (func-name (which-function))
          (file-name (buffer-file-name))
          (line-number (line-number-at-pos (region-beginning)))
          (org-src-mode (let ((mm (intern (replace-regexp-in-string
                                           "-mode" "" (format "%s" major-mode)))))
                          (or (car (rassoc mm org-src-lang-modes))
                              (format "%s" mm)))))
      (format
       "file:%s::%s
In ~%s~:
#+BEGIN_SRC %s
%s
#+END_SRC"
       file-name
       line-number
       func-name
       org-src-mode
       code-snippet))))

;; Make capture frame, made for being called via emacsclient
;; https://cestlaz.github.io/posts/using-emacs-24-capture-2/

(defun db/make-org-capture-frame ()
  "Create a new frame for capturing."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (org-capture))

(defun db/delete-frame-if-capture (&rest r)
  "If current frame was made for a capture, close after done."
  (ignore r)
  (when (equal (frame-parameter nil 'name)
               "capture")
    (delete-frame)))

(advice-add 'org-capture-finalize
            :after #'db/delete-frame-if-capture)


;;; Refiling

(defun db/verify-refile-target ()
  "Verify that a certain location is eligible as a refile target.
In other words, exclude tasks with a done state and those with
tag PERIODIC."
  (and
   ;; Exclude DONE state tasks from refile targets (from bh)
   (not (member (nth 2 (org-heading-components))
                org-done-keywords))
   ;; Don't refile to elements of periodic tasks
   (not (member "PERIODIC" (org-get-tags)))))


;;; Reset checklists

;; from `org-checklist’ by James TD Smith (@ ahktenzero (. mohorovi cc)),
;; version: 1.0
(defun org-reset-checkbox-state-maybe ()
  "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_CHECK_BOXES")
      (org-reset-checkbox-state-subtree)))


;;; Helper Functions for Clocking

(defun db/find-parent-task ()
  ;; http://doc.norang.ca/org-mode.html#Clocking
  "Return point of the nearest parent task, and NIL if no such task exists."
  (save-mark-and-excursion
   (save-restriction
     (widen)
     (let ((parent-task nil))
       (or (org-at-heading-p)
           (org-back-to-heading t))
       (while (and (not parent-task)
                   (org-up-heading-safe))
         (let ((tags (org-get-tags nil 'local)))
           (unless (or (member "NOP" tags)
                       (member "PERIODIC" tags))
             (setq parent-task (point)))))
       parent-task))))

(defun db/ensure-running-clock ()
  "Clocks in into the parent task, if it exists, or the default task."
  (when (and (not org-clock-clocking-in)
             (not org-clock-resolving-clocks-due-to-idleness))
    (let ((parent-task (db/find-parent-task)))
      (save-mark-and-excursion
       (cond
        (parent-task
         ;; found parent task
         (org-with-point-at parent-task
           (org-clock-in)))
        ((and (markerp org-clock-default-task)
              (marker-buffer org-clock-default-task))
         ;; default task is set
         (org-with-point-at org-clock-default-task
           (org-clock-in)))
        (t
         (org-clock-in '(4))))))))

(defun db/save-current-org-task-to-file ()
  "Format currently clocked task and write it to
`db/org-clock-current-task-file'."
  (with-temp-file db/org-clock-current-task-file
    (let ((clock-buffer (marker-buffer org-clock-marker)))
      (if (null clock-buffer)
          (insert "No running clock")
        (insert org-clock-heading)))))

(defun db/org-update-frame-title-with-current-clock ()
  "Set the title of all active frames to the headline of the
  current task."
  (interactive)
  (let ((clock-buffer (marker-buffer org-clock-marker)))
    (when clock-buffer
      (dolist (frame (frame-list))
        (modify-frame-parameters frame `((name . , org-clock-heading)))))))

(defun db/show-current-org-task ()
  "Show title of currently clock in task in modeline."
  (interactive)
  (message org-clock-current-task))


;;; Fixes

(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))


;;; Hydra

(defun db/clock-in-task-by-id (task-id)
  "Clock in org mode task as given by TASK-ID."
  (let ((location (org-id-find task-id 'marker)))
    (if (null location)
        (user-error "Invalid location «%s» given." task-id)
      (org-with-point-at location
        (org-clock-in))
      (org-save-all-org-buffers))))

(defun db/clock-out-task-by-id (task-id)
  "Clock out org mode task as given by TASK-ID."
  (org-with-point-at (org-id-find task-id 'marker)
    (org-clock-out))
  (org-save-all-org-buffers))

(defun db/org-clock-out ()
  "Clock out current clock."
  (interactive)
  (org-clock-out))

(defun db/org-clock-in-break-task ()
  "Clock into default break task as given by `org-break-task-id’."
  (interactive)
  (db/clock-in-task-by-id org-break-task-id))

(defun db/org-clock-in-home-task ()
  "Clock into default home task as given by `org-home-task-id’."
  (interactive)
  (db/clock-in-task-by-id org-home-task-id))

(defun db/org-clock-in-work-task ()
  "Clock into default work task as given by `org-work-task-id’."
  (interactive)
  (db/clock-in-task-by-id org-working-task-id))

(defun db/org-clock-in-last-task (&optional arg)
  ;; from doc.norang.ca, originally bh/clock-in-last-task
  "Clock in the interrupted task if there is one.

Skip the default task and get the next one.  If ARG is given,
forces clocking in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history))
           (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defhydra hydra-org-clock (:color blue)
  ;; Quote %, as otherwise they would be misinterpreted as format characters
  "\nCurrent Task: %s(replace-regexp-in-string \"%\" \"%%\" org-clock-current-task); "
  ("w" (db/org-clock-in-work-task) "work")
  ("h" (db/org-clock-in-home-task) "home")
  ("b" (db/org-clock-in-break-task) "break")
  ("i" (lambda ()
         (interactive)
         (org-clock-in '(4))) "interactive")
  ("a" counsel-org-goto-all "goto")
  ("o" org-clock-out "clock out")
  ("l" db/org-clock-in-last-task "last")
  ("d" (lambda ()
         (interactive)
         (when (org-clock-is-active)
           (save-window-excursion
             (org-clock-goto)
             (let ((org-inhibit-logging 'note))
               (org-todo 'done)
               (org-save-all-org-buffers)))))
   "default"))


;;; Babel

(defun org-babel-execute:hy (body params)
  ;; http://kitchingroup.cheme.cmu.edu/blog/2016/03/30/OMG-A-Lisp-that-runs-python/
  "Execute hy code BODY with parameters PARAMS."
  (ignore params)
  (let* ((temporary-file-directory ".")
         (tempfile (make-temp-file "hy-")))
    (with-temp-file tempfile
      (insert body))
    (unwind-protect
        (shell-command-to-string
         (format "hy %s" tempfile))
      (delete-file tempfile))))


;;; Custom link handlers

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
  "Open browser to show RFC of given NUMBER.
If `db/rfc-cache-path' is defined, download the RFC in txt format
there and open it.  If the RFC has already been downloaded
before, just open it.  If `db/rfc-cache-path' is not defined,
open RFC in HTML format in the default browser."
  (cond
   ((not (string-match "[1-9][0-9]*" number))
    (user-error "Not a valid number for an RFC: %s" number))
   ((and db/rfc-cache-path
         (file-name-absolute-p db/rfc-cache-path)
         (file-writable-p db/rfc-cache-path))
    (let ((rfc-path (expand-file-name (format "rfc%s.txt" number)
                                      db/rfc-cache-path)))
      (cond
       ((file-exists-p rfc-path)
        (find-file rfc-path))
       (t
        (with-temp-buffer
          (url-insert-file-contents (format "https://tools.ietf.org/rfc/rfc%s.txt"
                                            number))
          (write-file rfc-path))
        (find-file rfc-path)))))
   (t
    (warn "`db/rfc-cache-path' not defined or not an absolute writable path, opening RFC in browser.")
    (browse-url (concat "https://tools.ietf.org/html/rfc" number)))))



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

(defun db/org-copy-template-for-periodic-task ()
  "Copy template of the enclosing periodic task to item at point.
The template must be placed into an item titled 'Template',
called the template item.  The template item must be the first
headline of the periodic task, i.e., of the parent of the current
item at point.  The body of the template item, without any
drawers, will be copied to point."
  (interactive)
  (let ((template (save-restriction
                    (save-mark-and-excursion
                      (let ((template-element (progn
                                                (outline-up-heading 1 'invisible-ok)
                                                (outline-next-heading)
                                                (org-element-at-point))))
                        (unless (string-equal (org-element-property :title template-element)
                                              "Template")
                          (user-error "Template must be first headline in periodic task."))
                        ;; Starting from the end of the last element in the
                        ;; subtree, we go up until we find a drawer or a
                        ;; headline; everything in between is considered to be the template
                        (let ((content-end (org-element-property :contents-end template-element))
                              content-begin current-element)
                          (goto-char content-end)
                          (while (progn
                                   (setq current-element (org-element-at-point))
                                   (not (memq (org-element-type current-element)
                                              '(drawer property-drawer headline))))
                            (setq content-begin (org-element-property :begin current-element))
                            (goto-char (1- content-begin)))
                          (string-trim-right
                           (buffer-substring-no-properties content-begin content-end))))))))
    (insert template)
    (org-update-statistics-cookies nil)))


;;; Calendar

(defun db/export-diary ()
  "Export diary.org as ics file to the current value of `org-icalendar-combined-agenda-file’.
This is done only if the value of this variable is not null."
  (interactive)
  (cond
   ((null org-icalendar-combined-agenda-file)
    (message "`org-icalendar-combined-agenda-file’ not set, not exporting diary."))
   ((not (file-name-absolute-p org-icalendar-combined-agenda-file))
    (user-error "`org-icalendar-combined-agenda-file’ not an absolute path, aborting."))
   (t
    (progn
      (org-save-all-org-buffers)
      (let ((org-agenda-files (cl-remove-if #'null
                                            (list db/org-default-org-file
                                                  db/org-default-home-file
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


;;; Find items by link to current headline

(defun db/org-find-items-linking-by-id (id custom-id)
  "List all Org Mode items that link to ID.
Uses `org-search-view' to conduct the actual search.  ID must be
a UUID as generated by, e.g., `org-id-get-create', and CUSTOM-ID
must consist of ASCII letters, numbers, and hyphens only.  Each
of ID and CUSTOM-ID may be nil, but at least one of them must be
not."
  (unless (or (not id)
              (and (stringp id)
                   (string-match-p  "^[a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}$" id)))
    (user-error "Given ID is not a valid UUID: %s" id))
  (unless (or (not custom-id)
              (and (stringp custom-id)
                   (string-match-p "[-a-zA-Z0-9]" custom-id)))
    ;; sorry, only ASCII right now …
    (user-error "CUSTOM_ID must consist of alphanumeric charaters only"))

  (let ((query (cond
                ((and id custom-id) (format "{\\[\\[id:%s\\]\\|\\[\\[#%s\\]}" id custom-id))
                (id (format "[[id:%s]" id))
                (custom-id (format "[[#%s]" custom-id))
                (t (user-error "Neither ID nor CUSTOM_ID given")))))
    (org-search-view nil query)))

(defun db/org-find-links-to-current-item (arg)
  "Find links to current item.
Only links using the ID or CUSTOM_ID property are considered.

If ARG is given, or if not in an Org Mode buffer, interactively
prompt for an item."
  (interactive "P")
  (apply #'db/org-find-items-linking-by-id
         (if (and (derived-mode-p 'org-mode) (not arg))
             (list (org-id-get) (org-entry-get nil "CUSTOM_ID"))
           (let ((pom (nth 3 (org-refile-get-location nil (get-file-buffer db/org-default-org-file)))))
             (if (not pom)
                 (user-error "Invalid location")
               (org-with-point-at pom
                 (list (org-id-get) (org-entry-get nil "CUSTOM_ID"))))))))

(defun db/org-add-link-to-other-item ()
  "Interactively query for item and add link to it at point.
Uses `org-id-get-create' to get the ID or CUSTOM_ID propery of
the target headline."
  (interactive)
  (let ((pom (nth 3 (org-refile-get-location nil (get-file-buffer db/org-default-org-file)))))
    (if (not pom)
        (user-error "Invalid location")
      (let (id item)
        (save-mark-and-excursion
          (org-with-point-at pom
            (setq item (org-entry-get nil "ITEM")
                  id (org-id-get-create)))
          (insert (format "[[id:%s][%s]]" id item)))))))


;;; End

(provide 'db-org)

;;; db-org.el ends here
