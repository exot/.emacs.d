;;; org.el -- Daniel's org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file defines functions used in the main configuration of org-mode and
;; it’s subpackages.  Nothing here changes the behavior of org-mode per se, as
;; loading this file only defines a couple of functions.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'hydra)
(require 'db-customize)
(require 'ox-icalendar)
(require 'org-ql)

(autoload 'counsel-org-goto-all "counsel")
(autoload 'which-function "which-func")
(autoload 'org-element-property "org-element")

(declare-function w32-shell-execute "w32fns.c")


;;; Agenda Customization

(defun db/check-special-org-files-in-agenda (&rest args)
  "Check special Org mode files to be part of the variable `org-agenda-files'.
The special Org mode files are `db/org-default-org-file',
`db/org-default-work-file', `db/org-default-home-file', and
`db/org-default-refile-file'.  Ignore ARGS."
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
                        nil 'face 'org-agenda-structure)
                    "\n"))
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

(defun db/org-agenda-insert-active-filters (&optional match)
  "Insert string showing the current agenda filters.

The filter display is added after the structural header.

If no agenda filters are active, nothing will be inserted.

Add this function to `org-agenda-finalize-hook' to enable this."
  ;; First delete any present active filter display, as it might be obsolete.
  (save-excursion
    (when-let ((pos (text-property-any
                     (point-min) (point-max) 'db/active-filter-display t)))
      (goto-char pos)
      (kill-line)))

  ;; Insert current active filters if there are any.
  (when (org-agenda-filter-any)
    (save-excursion
      (when-let ((pos (text-property-any
                       (point-min) (point-max) 'org-agenda-structural-header t)))
        (goto-char pos)
        (end-of-line)

        ;; Current filter display shamelessly stolen from `org-agenda-filter'.
        ;; Is there a function to do this?
        (let* ((cf (mapconcat #'identity org-agenda-category-filter ""))
	       (tf (mapconcat #'identity org-agenda-tag-filter ""))
	       (ef (replace-regexp-in-string "^\\+" "" (or (car org-agenda-effort-filter) "")))
	       (rf (replace-regexp-in-string "^\\+" "" (or (car org-agenda-regexp-filter) "")))
	       (ff (concat cf tf ef (when (not (equal rf "")) (concat "/" rf "/")))))
          (insert-and-inherit
           (propertize (format " [%s]" ff)
                       'db/active-filter-display t)))))))

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


;;; Effort computation and display

(defun db/org-remaining-effort-of-current-item (&optional as-number)
  "Return remaining effort of Org item at point.

The remaining effort is computed as the planned effort minus the
already clocked time.  If this result is negative, return zero.

Return the remaining effort as duration string by default.  When
optional AS-NUMBER is non-nil, return the effort as number.

If no effort is specified at the item at point, return an empty
string, or nil when AS-NUMBER is non-nil."

  (if (derived-mode-p 'org-agenda-mode)

      (if-let ((hd-marker (org-get-at-bol 'org-hd-marker)))
          ;; `org-hd-marker' is set, there is some Org item that corresponds to
          ;; this line.  Get the remaining effort from there.
          (org-with-point-at hd-marker
            (db/org-remaining-effort-of-current-item))
        ;; We are at some special item in the Org agenda (e.g. some diary
        ;; entry), just show nothing.
        "")

    (unless (derived-mode-p 'org-mode)
      (user-error "Not in Org mode buffer, aborting"))

    (if-let ((effort (org-entry-get (point) "Effort")))
        (let ((remaining-effort (max 0 (- (org-duration-to-minutes effort)
                                          (db/org-clocked-time-for-current-item)))))
          (if as-number
              remaining-effort
            (org-duration-from-minutes remaining-effort)))
      (if as-number nil ""))))

(defun db/org-cmp-remaining-effort (a b)
  "Compare the remaining efforts of Org items A and B.

A and B are strings only, but their corresponding Org items are
accessible via the `org-hd-marker' text property."
  (let* ((def (if org-agenda-sort-noeffort-is-high 32767 -1))
         (ma (or (get-text-property 1 'org-marker a)
                 (get-text-property 1 'org-hd-marker a)))
         (mb (or (get-text-property 1 'org-marker b)
                 (get-text-property 1 'org-hd-marker b)))
         (ea (or (and (markerp ma)
                      (marker-buffer ma)
                      (org-with-point-at ma
                        (db/org-remaining-effort-of-current-item 'as-number)))
                 def))
         (eb (or (and (markerp mb)
                      (marker-buffer mb)
                      (org-with-point-at mb
                        (db/org-remaining-effort-of-current-item 'as-number)))
                 def)))
    (cond ((> ea eb) +1)
          ((< ea eb) -1))))

;; Show sum of daily efforts in agenda, the following two functions are from
;; anpandey,
;; cf. https://emacs.stackexchange.com/questions/21380/show-sum-of-efforts-for-a-day-in-org-agenda-day-title#21902

(defun db/org-agenda-calculate-efforts (limit)
  "Sum efforts of day entries up to LIMIT in the agenda buffer.
Entries included are those scheduled for that day, scheduled at
some past day (and still on display), active
timestamps (appointments), and deadlines (assuming they are only
shown because they are due)."
  (let (total)
    (save-excursion
      (let (already-seen)
        (while (< (point) limit)
          (when (member (org-get-at-bol 'type)
                        '("scheduled" "past-scheduled" "timestamp" "deadline" "block"))
            (let ((item-id (org-with-point-at (org-get-at-bol 'org-hd-marker) (org-id-get-create))))
              (unless (member item-id already-seen)
                (push (org-with-point-at (org-get-at-bol 'org-hd-marker)
                        (db/org-remaining-effort-of-current-item))
                      total)
                (push item-id already-seen))))
          (forward-line))))
    (org-duration-from-minutes
     (cl-reduce #'+
                (mapcar #'org-duration-to-minutes
                        (cl-remove-if-not 'identity total))))))

(defun db/org-agenda-insert-efforts ()
  "Insert efforts for each day into the agenda buffer.

Add this function to `org-agenda-finalize-hook' to enable this."
  (save-excursion
    (let (pos)
      (while (setq pos (text-property-any
                        (point) (point-max) 'org-agenda-date-header t))
        (goto-char pos)
        (end-of-line)

        ;; When there is already an effort sum shown, delete it first
        (when (re-search-backward " ([0-9]+:[0-9]\\{2\\})"
                                  (save-mark-and-excursion
                                    ;; Only search until start of line
                                    (beginning-of-line)
                                    (point))
                                  t)
          (kill-line))

        ;; Insert effort sum
        (insert-and-inherit
         (concat " ("
                 (db/org-agenda-calculate-efforts
                  (or (next-single-property-change (point) 'day)
                      ;; If nothing is shown on the current day, the previous
                      ;; call may return nil; in that case, don't sum anything
                      ;; by setting the limit to 0
                      0))
                 ")"))
        (forward-line)))))


;;; Capturing

(defun db/org-timestamp-difference (stamp-1 stamp-2)
  "Return time difference between two Org mode timestamps.
STAMP-1 and STAMP-2 must be understood by
`org-parse-time-string'."
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
  "Format Org mode source block with contant of active region in FILENAME."
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

(defun db/delete-frame-if-capture (&rest _r)
  "If current frame was made for a capture, close after done."
  (when (equal (frame-parameter nil 'name)
               "capture")
    (delete-frame)))

(advice-add 'org-capture-finalize
            :after #'db/delete-frame-if-capture)


;;; Refiling

(defun db/verify-refile-target ()
  "Verify that a certain location is eligible as a refile target."
  (and
   ;; Exclude DONE state tasks from refile targets (from bh)
   (not (member (nth 2 (org-heading-components))
                org-done-keywords))))


;;; Helper Functions for Clocking

(defun db/find-parent-task ()
  ;; http://doc.norang.ca/org-mode.html#Clocking
  "Return point of the nearest parent task, and NIL if no such task exists.

Ignores headlines tagged with NOP or PERIODIC, as those items
should not be clocked."
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
  "Format currently clocked task and write it to`db/org-clock-current-task-file'."
  (with-temp-file db/org-clock-current-task-file
    (let ((clock-buffer (marker-buffer org-clock-marker)))
      (if (null clock-buffer)
          (insert "No running clock")
        (insert org-clock-heading)))))

(defun db/org-update-frame-title-with-current-clock ()
  "Set title of all active frames to the headline of the current task."
  (interactive)
  (let ((clock-buffer (marker-buffer org-clock-marker)))
    (when clock-buffer
      (setq frame-title-format org-clock-heading)
      (dolist (frame (frame-list))
        (modify-frame-parameters frame `((name . ,org-clock-heading)))))))

(defun db/show-current-org-task ()
  "Show title of currently clock in task in modeline."
  (interactive)
  (message org-clock-current-task))

(defun db/org-clocked-time-for-current-item ()
  "Return all clocked time for Org item at point.

Also includes the currently running clock when the current item
is clocked in."

  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode buffer, aborting"))

  ;; XXX: This does not take into account the current setting of
  ;; CLOCK_MODELINE_TOTAL.

  (let ((at-current-clock-p (and (markerp org-clock-hd-marker)
                                 (save-mark-and-excursion ; from `org-clock-in'
                                   (org-back-to-heading t)
                                   (and (eq (marker-buffer org-clock-hd-marker)
                                            (org-base-buffer (current-buffer)))
                                        (= (point) (marker-position org-clock-hd-marker))
                                        (equal org-clock-current-task (org-get-heading t t t t)))))))

    (+ (org-clock-sum-current-item)
       (if at-current-clock-p
           ;; From `org-clock-get-clocked-time'
           (floor (org-time-convert-to-integer
                   (org-time-since org-clock-start-time))
                  60)
         0))))

(defun db/org-mark-current-default-task ()
  "Mark current task as default when equal to work task or home task.
Work task and home task are determined by the current values of
`org-working-task-id’ and `org-home-task-id’, respectively."
  (let ((current-id (org-id-get org-clock-marker)))
    (when (member current-id (list org-working-task-id
                                   org-home-task-id))
      (org-clock-mark-default-task))))


;;; Workload Reports

(defun db/org-planned-tasks-in-range (start-date end-date &optional org-ql-match)
  "Return list of tasks planned between START-DATE and END-DATE.

This function will search through the files returned by calling
the function `org-agenda-files' for all tasks that are scheduled,
have an active timestamp, or are deadline in the given time
range.

The result has the form (TOTAL-TIME . TASKS), where TASKS is a
list of cons cells (ID . REMAINING-EFFORT).  REMAINING-EFFORT is
computed as per `db/org-remaining-effort-of-current-item', which
see.  The total time is given as an Org mode time string of the
form hh:mm, as is REMAINING-EFFORT entries.

When ORG-QL-MATCH, an org-ql sexp, is given, filter the list of
tasks in range by this expression.  When ORG-QL-MATCH is not
given, default to `(todo)'.

START-DATE and END-DATE must be strings formatted such that
`org-read-date' can parse a date from them.  In particular,
everything understood by `parse-time-string' should be fine.
When START-DATE or END-DATE (or both) are nil, no constraints are
imposed on the respective time range."

  (unless (or (null start-date)
              (stringp start-date))
    (user-error "START-DATE must be nil or a string, but it's %s" start-date))

  (unless (or (null end-date)
              (stringp end-date))
    (user-error "END-DATE must be nil or a string, but it's %s" end-date))

  (let* ((start-date-expr (--when-let (and start-date (org-read-date nil nil start-date))
                            (list :from it)))
         (end-date-expr (--when-let (and end-date (org-read-date nil nil end-date))
                          (list :to it)))
         (tasks (org-ql-query
                  :from (org-agenda-files)
                  :select '(cons
                            (org-id-get-create)
                            (db/org-remaining-effort-of-current-item))
                  :where `(and ,(or org-ql-match '(todo))
                               ;; Is this redundant?  Could we just stick with `ts-active'?
                               (or (scheduled ,@start-date-expr ,@end-date-expr)
                                   (deadline  ,@start-date-expr ,@end-date-expr)
                                   (ts-active ,@start-date-expr ,@end-date-expr)))))
         (total-time (->> tasks
                          (-map #'(lambda (task)
                                    (let ((effort (cdr task)))
                                      (if (null effort)
                                          0
                                        (org-duration-to-minutes effort)))))
                          -sum
                          org-duration-from-minutes)))
    (cons total-time tasks)))

(defun org-dblock-write:db/org-workload-report (params)
  "Write workload report for all tasks.

Tasks are read from files in the variable `org-agenda-files'

PARAMS is a property list of the following parameters:

`:start-date':

  Start date for the workload report.  Leave out if no constraint
  on the start date is necessary.

`:end-date':

  End date of the workload report.  Leave out if no constraint on
  the start date is necessary.

`:org-ql-match'

  `org-ql' expression (in sexp syntax) to filter the list of tasks.

`:sort-column'

  Specify the column to sort by.  Can be any of `task', `effort',
  `timestamp', `scheduled', or `deadline'.

All tasks between `:start-date' and `:end-date' will be collected
and their remaining effort summed up.  The date format is
everything understood by `org-read-date'."
  (let* ((start-date (plist-get params :start-date))
         (end-date (plist-get params :end-date))
         (org-ql-match (plist-get params :org-ql-match))
         (sort-column-count  (cl-case (plist-get params :sort-column)
                               ((nil) 2) ; sort by effort by default
                               ((task) 1)
                               ((effort) 2)
                               ((timestamp) 3)
                               ((scheduled) 4)
                               ((deadline) 5)
                               (otherwise (user-error ":sort-column value invalid, see docstring of `org-dblock-write:db/org-workload-report' for options"))))
         (task-summary (db/org-planned-tasks-in-range start-date end-date org-ql-match)))

    ;; Create table
    (cl-flet ((ts-property-from-id (task-id property)
                ;; Retrieve PROPERTY from task given by TASK-ID.  If found,
                ;; assume PROPERTY to be a timestamp and unconditionally convert
                ;; it into an inactive timestamp.  If PROPERTY is not found,
                ;; return the empty string.
                (--if-let (org-entry-get (org-id-find task-id 'marker)
                                         property)
                    (->> it
                         (string-replace "<" "[")
                         (string-replace ">" "]"))
                  "")))

      (insert (format "#+CAPTION: Workload Report at %s for [%s]--[%s] .\n"
                      (with-temp-buffer
                        ;; Is there an easier way to get the current time as an
                        ;; inactive timestamp?
                        (org-insert-time-stamp (current-time) t t)
                        (buffer-string))
                      (org-read-date nil nil start-date)
                      (org-read-date nil nil end-date)))
      (insert "| Task | Effort | Timestamp | SCHEDULED | DEADLINE |\n|---|\n")
      (pcase-dolist (`(,task-id . ,effort-string) (cdr task-summary))
        (insert (format "| %s | %s | %s | %s | %s |\n"
                        (db/org--format-link-from-org-id task-id)
                        (or effort-string "")
                        (ts-property-from-id task-id "TIMESTAMP")
                        (ts-property-from-id task-id "SCHEDULED")
                        (ts-property-from-id task-id "DEADLINE"))))
      (insert (format "|---|\n| Total | %s |\n|---|" (car task-summary)))
      (org-table-align))

    ;; Sort table
    (forward-line -4)                   ; go back to actual data
    (beginning-of-line)
    (dotimes (_ sort-column-count)      ; move to column to sort
      (org-table-next-field))
    (org-table-sort-lines nil ?t)))

(defun db/org-insert-workload-report ()
  "Create dynamic block of planned tasks in given time range."
  (interactive)
  (org-create-dblock
   (list :name "db/org-workload-report"
         :start-date (read-string "Start date: ")
         :end-date (read-string "End date: ")))
  (org-update-dblock))

(org-dynamic-block-define "db/org-workload-report" #'db/org-insert-workload-report)

(defun org-dblock-write:db/org-workload-overview-report (params)
  "Write an overview workload report for all tasks.

Tasks are read from files in the variable `org-agenda-files'.

This overview report will list the amount of work planned for
increasing intervals of time until a given end date is reached.
For example, if the amount to increase the intervals is two
days (+2d) and the report is meant to start from today (.), then
this report will list the total amount of work planned for the
days .+2d, .+4d, .+6d, … until the end date is reached.

PARAMS is a property list of the following parameters:

`:start-date':

  Start date for the workload report.  When not provided, will
  default to today at 00:00.  When provided, must be in a format
  understood by `org-read-date'.

`:end-date':

  End date of the workload report.  Must be provided and provided
  in a format understood by `org-read-date'.  The end date is
  inclusive.

`:increment':

  Amount of days to increase the intervals.  Defaults to \"+1d\"
  and must be provided in a format understandable by
  `org-read-date'.

`:org-ql-match'

  `org-ql' expression (in sexp syntax) to filter the list of
  tasks to consider.  Defaults to (todo)."
  (let* ((start-date (org-read-date t t
                                    (or (plist-get params :start-date)
                                        "00:00")))
         (end-date (or (--if-let (plist-get params :end-date)
                           (org-read-date t t it))
                       (user-error "No valid end-date provided")))
         (increment (or (plist-get params :increment)
                        "+1d"))
         (org-ql-match (or (plist-get params :org-ql-match)
                           '(todo)))
         (timestamp-format "%Y-%m-%d %a %H:%M")
         date-range)

    ;; Check input
    (unless (string-match-p (rx bos "+" (+ digit) (in "dwmy") eos)
                            increment)
      (user-error "Increment must be of the form +1d, +2m, +3y, …, but it's %s" increment))

    ;; Compute range of dates to check; simple but potentially costly approach
    ;; taken from https://sachachua.com/blog/2015/08/org-mode-date-arithmetic/;
    ;; maybe consider `org-read-date-get-relative' as well?
    (let ((current start-date))
      (while (or (time-less-p current end-date)
                 (time-equal-p current end-date))
        (setq current (org-read-date t t
                                     ;; Add an extra + to ensure we increase the
                                     ;; amount of time relative to the given
                                     ;; default time string.
                                     (format "+%s" increment)
                                     nil current))
        (push current date-range)))
    (setq date-range (nreverse (cdr date-range)))

    (insert (format "#+CAPTION: Workload Overview Report at [%s] with start date [%s]\n"
                    (format-time-string timestamp-format (current-time))
                    (format-time-string timestamp-format start-date)))
    (insert "| End Time | Planned Total |\n| <r> | <r> |\n|---|\n")
    ;; Compute workload report for each date and record the total time;
    ;; XXX: this might be slow, try to reduce the calls to
    ;; `db/org-planned-tasks-in-range'.
    (dolist (interval-end-date date-range)
      (let ((total-time (car (db/org-planned-tasks-in-range (format-time-string timestamp-format start-date)
                                                            (format-time-string timestamp-format interval-end-date)
                                                            org-ql-match))))
        (insert (format "| [%s] | %s |\n"
                        (format-time-string timestamp-format interval-end-date)
                        total-time))))
    (insert "|--|")
    (org-table-align)))

(defun db/org-insert-workload-overview-report ()
  "Create dynamic block of planned tasks in given time range."
  (interactive)
  (org-create-dblock
   (list :name "db/org-workload-overview-report"
         :end-date (org-read-date nil nil nil "End date: ")
         :increment (read-string "Increment (default: +1d): " nil nil "+1d")))
  (org-update-dblock))

(org-dynamic-block-define "db/org-workload-overview-report"
                          #'db/org-insert-workload-overview-report)


;;; Fixes

(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))


;;; Clocking Hydra

(defun db/clock-in-task-by-id (task-id)
  "Clock in org mode task as given by TASK-ID."
  (let ((location (org-id-find task-id 'marker)))
    (if (null location)
        (user-error "Invalid location given: «%s»" task-id)
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
  "\nCurrent Task: %s(replace-regexp-in-string \"%\" \"%%\" (or org-clock-current-task \"\")); "
  ("w" (db/org-clock-in-work-task) "work")
  ("h" (db/org-clock-in-home-task) "home")
  ("b" (db/org-clock-in-break-task) "break")
  ("i" (lambda ()
         (interactive)
         (org-clock-in '(4)))
      "interactive")
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

(defun db/org-eval-subtree-no-confirm (&optional arg)
  "Evaluate subtree at point without asking for confirmation.

Use with care!

With given ARG, force reevaluation as described for
`org-babel-execute-src-block'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org buffer, aborting"))
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-execute-subtree arg)))


;;; Custom link handlers

(defun db/org-onenote-open (path)
  "Visit OneNote document on PATH."
  (unless (file-executable-p db/path-to-onenote)
    (user-error "Path for OneNote is not executable, please customize `db/path-to-onenote’"))
  (start-process "OneNote" nil db/path-to-onenote "/hyperlink" path))

(defun db/org-outlook-open (id)
  "Open Outlook item identified by ID.
ID should be an Outlook GUID."
  (unless (file-executable-p db/path-to-outlook)
    (user-error "Path for Outlook is not executable, please customize `db/path-to-outlook’"))
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
  "Interactively find CSV file and open it as Org mode table.
Default separator is \";\", but this can be changed interactively
by passing a non-nil value for ARG."
  (interactive "P")
  (let ((separator (if arg (read-from-minibuffer "Separator (regular expression): ")
                     ";")))
    (call-interactively #'find-file)
    (org-mode)
    (org-table-convert-region (point-min) (point-max) separator)))

(defun db/org-update-headline-log-note (&optional new-headline)
  "Replace headline of item at point with NEW-HEADLINE.

Interactively query for HEADLINE when not provided.  Clear refile
cache if that's in use."
  (interactive)

  (unless (derived-mode-p 'org-mode 'org-agenda-mode)
    (user-error "Neither in an Org mode nor Org agenda buffer, aborting"))

  (unless new-headline
    (let ((default-value (cond
                          ((derived-mode-p 'org-mode)
                           (org-entry-get (point) "ITEM"))
                          ((derived-mode-p 'org-agenda-mode)
                           (org-agenda-with-point-at-orig-entry
                            nil (org-entry-get (point) "ITEM"))))))
      (setq new-headline (read-string "New Headline: "
                                      nil nil
                                      default-value))))

  (unless (stringp new-headline)
    (user-error "New headline must be string"))

  (when (string-match-p "\n" new-headline)
    (user-error "New headline contains newlines, aborting"))

  (save-window-excursion
    (save-mark-and-excursion
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-goto))

      (when (org-before-first-heading-p)
        (user-error "Point is before first headline, aborting"))

      (let ((old-headline (org-entry-get (point) "ITEM")))
        ;; Update headline
        (org-edit-headline new-headline)

        ;; Code to add note interactively taken from
        ;; https://sachachua.com/blog/2022/11/logging-sent-messages-to-org-mode-with-message-sent-hook/
        (move-marker org-log-note-return-to (point))
        (move-marker org-log-note-marker (point))
        (with-temp-buffer
          (insert (format "Changed headline from: %s\n" old-headline))
          (let ((org-log-note-purpose 'note))
            (org-store-log-note))))))

  (when org-refile-use-cache
    (org-refile-cache-clear))

  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-redo)))


;;; Checklist Handling

(defun db/org--find-template ()
  "Return marker to template item associated with item at point.

Return NIL if no template is associated with item at point.

See `db/org-insert-checklist' for how this template item is
determined."

  (let (template-marker)

    ;; Check for TEMPLATE_ID property
    (when-let ((template-id (org-entry-get (point) "TEMPLATE_ID")))
      (setq template-marker (org-id-find template-id :get-marker))
      (unless template-marker
        (warn "TEMPLATE_ID is set, but could not be resolved: %s"
              template-id)))

    ;; If no template has been found so far, search for top-most sibling and
    ;; whether its headline starts with “Template”; use that when found.
    (unless template-marker
      (save-excursion
        (widen)
        (let ((top-most-sibling (condition-case _
                                    (save-mark-and-excursion
                                      (outline-up-heading 1 'invisible-ok)
                                      (outline-next-heading)
                                      (point))
                                  (t nil))))
          (when (and top-most-sibling
                     (integerp top-most-sibling) ; just to make sure we have a point here
                     (string-match-p "^Template.*"
                                     (org-entry-get top-most-sibling "ITEM")))
            (setq template-marker (org-with-point-at top-most-sibling
                                    (point-marker)))))))

    ;; Return `template-marker', which is either `nil' or a marker.
    template-marker))

(defun db/org-insert-checklist (arg)
  "Insert checklist for Org Mode item at point.

Checklists are inserted before the first child, if existent, or
at the end of the subtree.

After inserting a checklist, add the property
CHECKLIST_INSERTED_P with value t to item at point.  Checklists
are not inserted if this property with this value is already
present, to avoid double insertions of checklists.

The checklist consists of a listing of relevant backlinks of the
current item and its parents (without archives) as well as a
template.

Relevant backlinks are Org items and are determined as follows:

- for an Org item to be considered as backlink item, it must
  reference the item at point directly, or one of its parents,
  via an Org link using the id: link type (also see the
  `db/org-backlinks' dynamic block);

- the backlink item must not be done and must not be tagged with
  TEMPLATE;

- the backlink item must not be scheduled in the future;

- the backlink item must be contained in a file in the variables
  `org-agenda-files' or `org-agenda-text-search-extra-files', but
  not in an archive file (i.e., archives are excluded from the
  search);

- the backlink item must not have the CHECKLIST_NO_BACKLINK
  property set to nil (with inheritance not being considered,
  i.e., the property must be set directly at the item to exclude
  it as backlink).

The depth to which backlinks to parents are considered can be
configured via the CHECKLIST_BACKLINK_DEPTH property at the item
at point.  This property is looked up only at the current item,
i.e., again no inheritance is considered.  If this property is
not set, the depth to which backlinks to parents is considered is
unlimited by default (i.e., nil).

After the table of backlinks, a template is inserted.  This
templates is usually a checklist copied from another Org item
tagged with :TEMPLATE:.  The item to copy the template from is
determined by the TEMPLATE_ID property, which must be an ID
referencing the proper template item.  If that property is not
set, search for the topmost sibling of the current item is
conducted to see whether its headline is matching
\"^Template.*\"; if so, its body is used as template.

When ARG is given, jump to the current template instead of
inserting the checklist."
  (interactive "P")

  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode, aborting"))

  (cond (arg
         ;; Universal argument given, just jump to the checklist of the item at
         ;; point.
         (db/org-goto-checklist-item-of-point))

        ((string= (org-entry-get (point) "CHECKLIST_INSERTED_P")
                  "t")
         (message "Checklist already inserted, not inserting again."))

        (t ;; Default action: insert complete checklist.

         (let (point-before-template
               point-after-template)

           ;; Let's remember where we are, so that latter on CHECKLIST_INSERTED_P
           ;; will be inserted at the original heading (where we are now) and not
           ;; at possible new subtrees coming from the template.
           (save-mark-and-excursion

             ;; Checklists are inserted directly before first child, if existent, or
             ;; at end of subtree
             (org-show-entry)
             (or (org-goto-first-child)
                 (org-end-of-subtree 'invisible-ok 'to-heading))
             ;; Move back from heading, unless we are at the end of the buffer
             (when (org-at-heading-p)
               ;; Go to end of line before heading
               (forward-char -1))

             ;; Insert relevant backlinks, when available.
             (let ((parent-depth (--when-let (org-entry-get (point) "CHECKLIST_BACKLINK_DEPTH" nil)
                                   (string-to-number it)))
                   number-of-backlinks
                   point-before-backlinks)

               ;; Insert blank line, but only if the previous line is not blank
               ;; already.
               (unless (save-mark-and-excursion
                         (forward-line -1)
                         (looking-at (rx bol (* space) eol)))
                 (insert "\n"))

               (insert (format "Relevant backlinks (%s):\n\n"
                               (if parent-depth
                                   (format "parent-depth %d" parent-depth)
                                 "all parents")))

               ;; Store where we are (minus the two newlines) so we can delete the
               ;; checklist in case it's empty.
               (setq point-before-backlinks (- (point) 2))

               (setq number-of-backlinks
                     (org-dblock-write:db/org-backlinks (list
                                                         :org-ql-match '(and
                                                                         (not (done))
                                                                         (not (ltags "TEMPLATE"))
                                                                         (not (scheduled :from 1))
                                                                         (not (property "CHECKLIST_NO_BACKLINK" "t" :inherit nil)))
                                                         :parent-depth (--when-let (org-entry-get (point) "CHECKLIST_BACKLINK_DEPTH" nil)
                                                                         (string-to-number it))
                                                         :archive nil)))

               ;; When no backlinks have been found, remove the empty table head and just
               ;; print "none".
               (when (zerop number-of-backlinks)
                 (delete-region point-before-backlinks (point))
                 (insert " none.")))

             ;; Insert template, when avilable.
             (let ((template-marker (db/org--find-template)))
               (insert "\n\nTemplate:")
               (setq point-before-template (point))
               (if (not template-marker)
                   (insert " none.\n")
                 (db/org-copy-body-from-item-to-point template-marker))
               (setq point-after-template (point))))

           (org-entry-put (point) "CHECKLIST_INSERTED_P" "t")
           (org-update-statistics-cookies nil)

           ;; Remove any existing ID properties, as they would be duplicates
           ;; now.  Only do this in the part inserted with template, though, and
           ;; leave previously existing child items and the item itself as they
           ;; are.
           (save-mark-and-excursion
             (set-mark point-before-template)
             (goto-char point-after-template)
             (org-map-entries #'(lambda ()
                                  (org-entry-delete (point) "ID")
                                  (org-entry-delete (point) "CUSTOM_ID"))
                              nil
                              'region))

           (db/org-goto-first-open-checkbox-in-subtree)))))

(define-obsolete-function-alias 'db/org-copy-template
  'db/org-insert-checklist
  "2022-10-09")

(defun db/org-goto-checklist-item-of-point ()
  "Go to template item associated with current item.

Error out if no such template item exists.

See `db/org-insert-checklist' for how this template item is
determined."
  (interactive)
  (--if-let (db/org--find-template)
      (progn
        (push-mark)
        (org-goto-marker-or-bmk it))
    (user-error "No template associated with item at point")))

(defun db/org-copy-body-from-item-to-point (pom)
  "Copy body from item given by POM to point.
This can be used to copy checklists from templates to the current
item, which might be an instance of a periodic task.  If POM is
not given, use `db/org-get-location' to interactively query for
it.  Ensures that there are newlines before and after the
inserted template."
  (interactive (list (db/org-get-location t)))
  (unless (number-or-marker-p pom)
    (user-error "Argument is neither point nor mark: %s" pom))
  (let ((body (save-mark-and-excursion
                (let ((template-element (org-with-point-at pom
                                          (org-element-at-point))))
                  (with-current-buffer (if (markerp pom) (marker-buffer pom) (current-buffer))
                    (let ((content-end (org-element-property :contents-end template-element))
                          current-element
                          content-begin)
                      ;; Start finding the beginning of the template contents from the top …
                      (goto-char (org-element-property :contents-begin template-element))
                      ;; … but skip any drawers we may find.
                      (setq current-element (org-element-at-point))
                      (while (memq (org-element-type current-element)
                                   '(drawer property-drawer))
                        (goto-char (org-element-property :end current-element))
                        (setq current-element (org-element-at-point)))
                      ;; Now we are at the beginning of the contents, let's copy
                      ;; that, but only if it exists and is not empty.
                      (setq content-begin (org-element-property :begin current-element))
                      (unless (and content-begin
                                   (< content-begin content-end))
                        (user-error "Cannot find content in template, or content is empty"))
                      (string-trim-right
                       (buffer-substring-no-properties content-begin content-end))))))))

    (cond
      ;; Open next line if the current line is not blank
      ((not (looking-at (rx bol eol)))
       (insert "\n\n"))

      ;; Add newline, but only if the previous line is not blank already.
      ((not (save-mark-and-excursion
              (forward-line -1)
              (looking-at (rx bol (* space) eol))))
       (insert "\n")))

    (insert body)

    ;; Insert final newline, but only when no blank line follows.
    (unless (save-mark-and-excursion
              (forward-line 1)
              (looking-at (rx bol (* space) eol)))
      (insert "\n"))))

(defun db/org-goto-first-open-checkbox-in-subtree (&optional silent)
  "Jump to first open checkbox in the current subtree.

First search for started checkboxes, i.e. [-], and if those are
not found, go to the first open checkbox, i.e. [ ].

If there's no such open checkbox, emit a message (unless SILENT
is non-nil) and stay put.

Note: when lists are nested, those are not (yet) descended into
to find the logically first open checkbox.  This should be fixed
somewhen, though."

  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org buffer, exiting"))

  (save-restriction
    (let ((original-point (point)))
      (widen)
      (org-back-to-heading 'invisible-ok)
      (org-narrow-to-subtree)
      (unless
          ;; Yes, those `progn's are not strictly necessary, but it feels
          ;; cleaner this way.
          (or (progn
                (goto-char (point-min))
                (re-search-forward " \\[-\\] " nil 'no-error))
              (progn
                (goto-char (point-min))
                (re-search-forward " \\[ \\] " nil 'no-error)))
        (unless silent
          (message "No open checkbox in subtree"))
        (goto-char original-point)))))

(defun db/org-clock-goto-first-open-checkbox (&optional select)
  "Go to the currently clocked-in item or most recently clocked item.

Move point to first open checkbox there, if there's one.  See
`db/org-goto-first-open-checkbox-in-subtree' for details.

If SELECT is non-nil, offer a choice of the most recently
clocked-in tasks to jump to."
  (interactive "@P")
  ;; This codes comes from `org-clock-goto', but has been adapted slightly to
  ;; use `display-buffer' instead of `pop-to-buffer-same-window', and to jump to
  ;; the first checklist item.
  (let* ((recent nil)
	 (m (cond
	     (select
	      (or (org-clock-select-task "Select task to go to: ")
		  (user-error "No task selected")))
	     ((org-clocking-p) org-clock-marker)
	     ((and org-clock-goto-may-find-recent-task
		   (car org-clock-history)
		   (marker-buffer (car org-clock-history)))
	      (setq recent t)
	      (car org-clock-history))
	     (t (user-error "No active or recent clock task")))))
    (if-let ((target-window (display-buffer (marker-buffer m))))
        (progn
          (select-window target-window)
          (if (or (< m (point-min)) (> m (point-max))) (widen))
          (goto-char m)
          (org-show-entry)
          (db/org-goto-first-open-checkbox-in-subtree :silent)
          (recenter org-clock-goto-before-context)
          (org-reveal)
          (if recent
	      (message "No running clock, this is the most recently clocked task"))
          (run-hooks 'org-clock-goto-hook))
      (user-error "Cannot display target buffer"))))


;;; Calendar

(defun db/export-diary ()
  "Export diary.org as ics file to `org-icalendar-combined-agenda-file’.
This is done only if the value of this variable is not null."
  (interactive)
  (cond
   ((null org-icalendar-combined-agenda-file)
    (message "`org-icalendar-combined-agenda-file’ not set, not exporting diary."))
   ((not (file-name-absolute-p org-icalendar-combined-agenda-file))
    (user-error "`org-icalendar-combined-agenda-file’ not an absolute path, aborting"))
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
                ((and id custom-id) (format "{\\[\\[id:%s\\]\\|\\[\\[file:[^]]*::#%s\\]\\|\\[#%s\\]}"
                                            id custom-id custom-id))
                (id (format "[[id:%s]" id))
                (custom-id (format "{\\[file:[^]]*::#%s\\]\\|\\[#%s\\]}"
                                   custom-id custom-id))
                (t (user-error "Neither ID nor CUSTOM_ID given")))))
    (org-search-view nil query)))

(defun db/org-get-location (&optional arg)
  "Interactively query for location and return mark.

When ARG is nil, this functions by default searches through the
current buffer if that one is an Org buffer and is associated
with a file, and `db/org-default-org-file' otherwise.  If the
current buffer is associated with a file from the variable
`org-agenda-files', though, the search is extended through all
agenda files (the rationale being that Org agenda files are
always considered to be one large data collection).

When ARG is non-nil, search through all files in the variables
`org-agenda-files', `org-agenda-text-search-extra-files', and the
current file or `db/org-default-org-file' as described above.

Search is always conducted up to level 9.  If the selected
location does not have an associated point or mark, error out.
Disable refile cache and any active refile filter hooks to allow
linking to any item."
  (let ((org-refile-target-verify-function nil)
        (org-refile-use-cache nil)
        ;; If the current buffer is an Org buffer and is associated with a file,
        ;; search through it; otherwise, use the default Org Mode file as
        ;; default buffer
        (default-buffer (if (and (buffer-file-name) (derived-mode-p 'org-mode))
                            (current-buffer)
                          (find-file-noselect db/org-default-org-file))))

    (when (null default-buffer)
      (user-error "Current buffer is not associated with a file and `db/org-default-org-file' does not exist; nothing to search through"))

    (let* ((current-buffer-is-in-org-agenda-files? (--when-let (buffer-file-name)
                                                     (-any (-partial #'file-equal-p it)
                                                           org-agenda-files)))

           ;; Default file(s) to search through; note that `default-buffer' is
           ;; provided later to `org-refile-get-location' as additional argument
           (org-refile-targets (append (if current-buffer-is-in-org-agenda-files?
                                           '((org-agenda-files :maxlevel . 9))
                                         '((nil :maxlevel . 9)))

                                       ;; When ARG is non-nil, add all agenda
                                       ;; files, but only if not already done
                                       ;; so.
                                       (and arg
                                            (not current-buffer-is-in-org-agenda-files?)
                                            '((org-agenda-files :maxlevel . 9)))

                                       ;; When ARG is non-nil, add extra file
                                       ;; files to search though.
                                       (and arg
                                            `((,(cl-remove-if-not #'stringp
                                                                  org-agenda-text-search-extra-files)
                                               :maxlevel . 9)))))

           (target-pointer (org-refile-get-location nil default-buffer))
           (pom (nth 3 target-pointer)))
      (cond
       ((markerp pom) pom)
       ((integerp pom)
        ;; Convert point to marker to ensure we are always in the correct
        ;; buffer; the second element of `target-pointer' contains the path to
        ;; the target file
        (save-mark-and-excursion
          (with-current-buffer (find-file-noselect (nth 1 target-pointer))
            (goto-char pom)
            (point-marker))))
       (t (user-error "Invalid location"))))))

(defun db/org-find-links-to-current-item (arg)
  "Find links to current item.
Only links using the ID or CUSTOM_ID property are considered.

If ARG is given, or if neither in an Org Mode buffer nor on a
headline in an Org Agenda buffer, interactively prompt for an
item using `db/org-get-location', which see."
  (interactive "P")
  (apply #'db/org-find-items-linking-by-id
         ;; Determine the current item interactively based on where we are: when
         ;; in an Org buffer or in Org agenda view, indeed use the item at
         ;; point; otherwise, and when ARG is given, query the user for the item
         ;; to look for.
         (org-with-point-at (cond ((and (not arg)
                                        (derived-mode-p 'org-mode))
                                   (point))
                                  ((and (not arg)
                                        (derived-mode-p 'org-agenda-mode)
                                        (org-get-at-bol 'org-hd-marker))
                                   (org-get-at-bol 'org-hd-marker))
                                  (t
                                   (db/org-get-location)))
           (list (org-id-get) (org-entry-get nil "CUSTOM_ID")))))

(defun db/org--format-link-from-pom (pom)
  "Return Org link pointing to Org item at POM.

POM must be point or mark to a valid Org item.  The link will be
of the format [[id][item-headline]], where `id' is the value of
the ID property of the item.  If the item does not have such a
property, is is generated automatically.

If `item-headline' contains any links itself, those will be
replaced by the description when available, and otherwise by
their plain link part."
  (unless (or (markerp pom) (integerp pom))
    (user-error "POM must be point or mark"))

  (let (item id)
    (org-with-point-at pom
      (setq item (org-entry-get (point) "ITEM")
            id (org-id-get-create)))

    (org-link-make-string (format "id:%s" id)
                          (org-link-display-format item))))

(defun db/org--format-link-from-org-id (id)
  "Format ID as an Org mode link [[ID][item-headline]].

If the headline of the item pointed to by ID contains any links,
those are replaced by their description before formatting."
  (db/org--format-link-from-pom (org-id-find id 'marker)))

(defun db/org-insert-link-to-pom (pom)
  "Insert an Org link to headline at POM.

If headline consists of a link with description, only the
description of that link will be included in the description of
the newly inserted link instead of the complete headline.  This
avoids containing a link in the description of the newly inserted
link."
  (insert (db/org--format-link-from-pom pom)))

(defun db/org-add-link-to-other-item (arg)
  "Interactively query for item and add link to it at point.

Search through all items of the current buffer, or
`db/org-default-org-file' if the current buffer is not associated
with a file.  If ARG is non-nil, include all files in the
variables `org-agenda-files' and
`org-agenda-text-search-extra-files' in this search."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org Mode"))
  (db/org-insert-link-to-pom (db/org-get-location arg)))

(defun db/org-add-link-to-current-clock ()
  "Insert link to currently clocked-in item at point.
Error out when the clock is not active."
  (interactive)
  (unless org-clock-marker
    (user-error "No clocked-in task, aborting"))
  (db/org-insert-link-to-pom org-clock-marker))

(defun db/org-add-link-to-org-clock-select-task ()
  "Insert link to Org item that was recently associated with clocking.

Interactively query for such an item and insert link to current
buffer at point."
  (interactive "")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org Mode, will not insert link"))
  (let ((pom (org-clock-select-task "Select item to link to: ")))
    (if (null pom)
        (error "Invalid choice")
      (db/org-insert-link-to-pom pom))))

(defhydra hydra-org-linking (:color blue :hint none)
  "
Add link at point to …
 … _c_urrent clock
 … _s_elect clock item from the recent clock history
 … _o_ther item (from current file buffer or default Org file)
 … _O_ther item (from all Org mode text search files)

Show _b_acklinks to current item."
  ("c" db/org-add-link-to-current-clock)
  ("s" db/org-add-link-to-org-clock-select-task)
  ("o" (db/org-add-link-to-other-item nil))
  ("O" (db/org-add-link-to-other-item t))
  ("b" db/org-find-links-to-current-item))

(defun db/org--backlinks-for-id (item-id &optional org-ql-match archives)
  "Return list of ID properties of Org Mode items linking to ITEM-ID.

If the optional ORG-QL-MATCH is given and is a valid `org-ql' query in
sexp syntax, filter the list for all items matching this query.
If ARCHIVES is given, also include archive files.

The search is conducted over all files returned by calling the
function `org-agenda-files', including archives, as well as all
files referenced in `org-agenda-text-search-extra-files'."

  (let ((extra-files org-agenda-text-search-extra-files)
        files)

    ;; Determine files to search through; ignore `agenda-archive' in
    ;; `org-agenda-text-search-extra-files', as we already handle this when
    ;; calling `org-agenda-files'.
    (setq files (org-agenda-files t archives))
    (when (eq (car extra-files) 'agenda-archives)
      (pop extra-files))
    (setq files (append files extra-files))

    ;; Search directly for “[[id:ITEM-ID]” instead of using the regular
    ;; expression for links, as the latter seems to be broken (as of
    ;; [2022-06-09] when descriptions contain brackets
    (org-ql-query :select '(org-id-get-create)
                  :from files
                  :where (let ((link-expression `(regexp ,(format "\\[\\[id:%s\\]" item-id))))
                           (if org-ql-match
                               `(and ,link-expression ,org-ql-match)
                             link-expression)))))

(defun db/org--find-parent-marks (&optional depth)
  "Return list of markers of all parent headings of Org item at point.

The list will include a marker to the current headline as well.
The order of the list will be in ascending order of
positions (i.e., the marker for the headline with the lowest
level/position comes first).

When optional parameter DEPTH is given, at most check only that
many parents.  If DEPTH is zero, only return a list of a single
marker pointing to the current headline."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode buffer, cannot determine parent items"))

  (let ((depth depth))                  ; do not modify argument
    (save-mark-and-excursion
      (save-restriction
        (widen)

        ;; Start at headline of current item
        (or (org-at-heading-p)
            (org-back-to-heading t))

        ;; Iterate over parents until at top-level
        (let ((parent-markers (list (point-marker))))
          (while (and (org-up-heading-safe)
                      (or (null depth)
                          (<= 0 (cl-decf depth))))
            (push (point-marker) parent-markers))
          parent-markers)))))

(defun org-dblock-write:db/org-backlinks (params)
  "Write table of backlinks for current item and its parent items as Org table.

Returns the number of backlinks.

PARAMS may contain the following values:

  :org-ql-match   An org-ql-match expression in sexp syntax to filter
                  the resulting backlinks

  :archives       If non-nil, include archives

  :parent-depth   How many parents to check for backlinks; value of nil means
                  unrestricted, a value of 0 means only consider current item."

  (let* ((org-ql-match (plist-get params :org-ql-match))
         (archives (plist-get params :archives))
         (parent-depth (plist-get params :parent-depth))
         headlines output-lines)

    (when (and (not (null parent-depth))
               (not (integerp parent-depth)))
      (user-error ":parent-depth is not an integer: %s" parent-depth))

    ;; Get all backlinks as list of Org mode IDs.  Each list consists of the ID
    ;; of the headline (current or partent), followed by the IDs linking back to
    ;; that headline.  If any of the headlines (current or parent) does not have
    ;; an ID, it will not be included in that list.
    (setq headlines
          (->> (db/org--find-parent-marks parent-depth)
               (mapcar #'(lambda (mark)
                           (org-with-point-at mark
                             (when-let ((id-at-point (org-id-get)))
                               (cons id-at-point
                                     (db/org--backlinks-for-id id-at-point
                                                               org-ql-match
                                                               archives))))))
               (cl-remove-if #'null)))

    ;; Change entries in headlines from the format (headline-id backlink-ids...)
    ;; to (backlink-id headline-ids ...) for grouping them in the output later.
    (setq headlines
          (->> headlines
               ;; Transform (headline-id backlink-ids) to pairs
               ;; (headline-id . backlink-id)
               (-mapcat (pcase-lambda (`(,headline . ,backlinks))
                          (mapcar #'(lambda (backlink)
                                      (cons backlink headline))
                                  backlinks)))
               ;; Group by backlinks (first entry), returns alist of
               ;; backlink-ids and list of pairs (backlink-id . headline-id)
               (-group-by #'car)
               ;; Flatten list, to get a list of (backlink-id headline-ids...)
               (-map (pcase-lambda (`(,backlink . ,backlink-headline-conses))
                       (cons backlink (-map #'cdr backlink-headline-conses))))))

    ;; Replace IDs by headlines and add priority for sorting
    (setq output-lines
          (->> headlines
               (-map (pcase-lambda (`(,backlink-id . ,headline-ids))
                       (list (db/org--format-link-from-org-id backlink-id)
                             (org-entry-get (org-id-find backlink-id 'marker)
                                            "PRIORITY")
                             (-map #'db/org--format-link-from-org-id headline-ids))))
               (-sort (pcase-lambda (`(_ ,prio-1 _) `(_ ,prio-2 _))
                        (string< prio-1 prio-2)))))

    ;; Format output-lines as Org table
    (insert (format "| Backlink | Prio | Backlink Target(s) |\n|---|"))
    (when output-lines
      (let (pp) ; pervious-priority, to draw hlines between groups of same priority
        (pcase-dolist (`(,backlink ,priority ,backlink-targets) output-lines)
          (when (and pp (not (equal pp priority)))
            (insert "\n|--|"))
          (setq pp priority)
          (insert
           (format "\n| %s | %s | %s |"
                   backlink
                   priority
                   (apply #'concat (-interpose ", " backlink-targets)))))
        (insert "\n|---|")))
    (org-table-align)

    (length output-lines)))

(defun db/org-insert-backlink-block ()
  "Create dynamic block of backlinks to current item or any of its parents."
  (interactive)
  (org-create-dblock
   (list :name "db/org-backlinks"
         :org-ql-match '(not (done))
         :parent-depth nil
         :archives nil))
  (org-update-dblock))

(org-dynamic-block-define "db/org-backlinks" #'db/org-insert-backlink-block)


;;; End

(provide 'db-org)

;;; db-org.el ends here
