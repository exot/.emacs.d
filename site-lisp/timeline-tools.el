;;; timeline-tools.el -- Utilities to manipulate org-mode timelines -*- lexical-binding: t -*-

;;; Commentary:

;; XXX: give brief overview, explain nomenclature (timelines, clock-lines,
;; ...), then list main functionality
;; XXX: This needs some tests

;;; Code:

(require 'dash)
(require 'org)
(require 'org-clock)


;; Customization

(defgroup timeline-tools nil
  "Functionality for manipulating timelines."
  :tag "Timeline Tools"
  :group 'applications)

(defcustom timeline-tools-short-task-threshold 300
  "Duration of task to be considered as short."
  :group 'timeline-tools
  :type 'integer)


;; Utilities

(defun timeline-tools-map-clocklines (clockline-fn headline-fn)
  "Iterate point over all clocklines and headlines of the current buffer.

For each clockline, call CLOCKLINE-FN with the starting and
ending time as arguments and point on the beginning of the line.
For each headline, call HEADLINE-FN with no arguments and point
on the start of the headline.  Traversal will be done from the
end of the file upwards.  If the buffer is narrowed, only this
region will be traversed."
  (when (eq major-mode 'org-mode)
    (let* ((re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
                       org-clock-string
                       "[ \t]*\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)")))
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward re nil t)
          (cond
           ((match-end 2)
            ;; Two time stamps.
            (save-mark-and-excursion
             (funcall clockline-fn (match-string 2) (match-string 3))))
           (t
            ;; A headline
            (save-mark-and-excursion
             (funcall headline-fn)))))))))

(defun timeline-tools-get-headline (marker)
  "Get headline of task at MARKER."
  (cl-assert (markerp marker))
  (save-match-data
    (let* ((heading (save-mark-and-excursion
                     (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (thing-at-point 'line t)))))
      (string-match (format "^\\(\\*+\\)\\(?: +%s\\)?\\(?: %s\\)? +\\(.*?\\)[ \t]*\\(?::\\(?:[A-Za-z_]+:\\)+\\)?$"
                            (regexp-opt org-todo-keywords-1)
                            org-priority-regexp)
                    heading)
      (match-string 4 heading))))

(defvar timeline-tools-org-inactive-timestamp-format
  (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")
  "Format of inactive `org-mode’ timestamps.
Can be used as format string for `format-time’,")

(defun timeline-tools-insert-clockline (time-1 time-2)
  "Insert new clock line from TIME-1 to TIME-2.

Insertion will be done at the beginning of the current line.
TIME-1 and TIME-2 must be given in a format understandable by
`format-time-string’, which see.  Saves mark and point."
  (save-mark-and-excursion
   (beginning-of-line)
   (indent-according-to-mode)
   (insert "CLOCK: ")
   (insert (format-time-string timeline-tools-org-inactive-timestamp-format
                               time-1))
   (insert "--")
   (insert (format-time-string timeline-tools-org-inactive-timestamp-format
                               time-2))
   (org-clock-update-time-maybe)))

(defun timeline-tools-clocklines-of-task (marker)
  "Return list of all clock lines of task under MARKER.

Each clock line is represented as a cons cell (START . END),
where both START and END are the starting and ending times of the
corresponding clock lines, encoded as a float denoting the
seconds since the epoch.  Includes clock lines of all subtrees as
well.  The order of the entries in the resulting list will be
reversed of what it is in the subtree of MARKER."
  (when (not (markerp marker))
    (user-error "Marker not valid"))
  (let ((clock-lines nil))
    (save-mark-and-excursion
     (org-with-point-at marker
       (org-narrow-to-subtree)
       (timeline-tools-map-clocklines
        (lambda (start end)
          (push (cons (org-time-string-to-seconds start)
                      (org-time-string-to-seconds end))
                clock-lines))
        (lambda ()))))
    clock-lines))


;; Reporting

;; XXX: Find actual org-mode functions that do the stuff we are doing here

(defun timeline-tools-clocklines-in-range (tstart tend)
  "Return tasks in current buffer between TSTART and TEND.

The resulting list consists of elements of the form

  (MARKER . CLOCK-TIMES)

where MARKER is a marker to the beginning of the corresponding
heading and CLOCK-TIMES is a list of cons cells of the
form (START . END), where START and END are the starting and
ending times of a clock line for this task.  START and END are
given as seconds since the epoch, as a floating point number.  No
truncation with respect to TSTART and TEND is done, i.e., START
or END may occassionally lie outside of these limits, but it is
always true that TSTART ≤ END ≤ TEND or TSTART ≤ START ≤ TEND."
  ;; adapted from `org-clock-sum’
  (when (eq major-mode 'org-mode)
    (let* ((tstart (cond ((stringp tstart) (org-time-string-to-seconds tstart))
                         ((consp tstart) (float-time tstart))
                         (t tstart)))
           (tend (cond ((stringp tend) (org-time-string-to-seconds tend))
                       ((consp tend) (float-time tend))
                       (t tend)))
           task-clock-times times)
      (timeline-tools-map-clocklines
       ;; when on clock line, collect times
       #'(lambda (start end)
           (let* ((ts (float-time
                       (apply #'encode-time (org-parse-time-string start))))
                  (te (float-time
                       (apply #'encode-time (org-parse-time-string end))))
                  (dt (- (if tend (min te tend) te)
                         (if tstart (max ts tstart) ts))))
             (when (> dt 0)
               (push (cons ts te) times))))
       ;; when on headlines, store away collected clocklines
       #'(lambda ()
           (when (and org-clock-report-include-clocking-task
                      (eq (org-clocking-buffer) (current-buffer))
                      (eq (marker-position org-clock-hd-marker) (point))
                      (or (and tstart
                               (<= tstart (float-time org-clock-start-time) tend))
                          (and tend
                               (<= tstart (float-time) tend))))
             (push (cons (float-time org-clock-start-time) (float-time))
                   times))
           (when (not (null times))
             (push (cons (point-marker) times) task-clock-times)
             (setq times nil))))
      task-clock-times)))

(defun timeline-tools-timeline (tstart tend &optional files)
  "Return list of clocked times between TSTART and TEND from FILES.

Each element in this list is of the form

  (START END MARKER),

where START, END, MARKER are as returned by
`timeline-tools-clocklines-in-range’, which see.  Entries in the
resulting list are sorted by START, ascending."
  (let (timeline-of-files turned-around-timeline)
    (setq timeline-of-files
          (->> (or files org-agenda-files)
               (cl-remove-if-not #'file-exists-p)
               (cl-mapcan #'(lambda (file)
                              (with-current-buffer (or (get-file-buffer file)
                                                       (find-file-noselect file))
                                (timeline-tools-clocklines-in-range tstart tend))))))
    (dolist (entry timeline-of-files)
      (dolist (clock-time (cdr entry))
        (push (list (car clock-time) (cdr clock-time) (car entry))
              turned-around-timeline)))
    (sort turned-around-timeline
          (lambda (entry-1 entry-2)
            (< (car entry-1) (car entry-2))))))

(defun timeline-tools-cluster-same-category (timeline)
  "Cluster TIMELINE into consecutive entries with equal category.
Markers to org mode tasks are combined into a list."
  (let ((new-timeline (-partition-by (lambda (entry)
                                       (let ((marker (third entry)))
                                         (org-entry-get marker "CATEGORY")))
                                     timeline)))
    (mapcar (lambda (cluster)
              (list (first (first cluster))       ; start of first entry
                    (second (car (last cluster))) ; end of last entry
                    (mapcar #'third cluster)))
            new-timeline)))

(defun timeline-tools-skip-short-entries (threshold timeline)
  "Skip entries shorter than THRESHOLD in TIMELINE.

A slot is short if it is not longer than THRESHOLD seconds.
Resulting gaps are distributed evenly among adjacent slots."
  (let ((start (first (first timeline)))
        (end (second (car (last timeline))))
        new-timeline)
    ;; remove all slots that are too short
    (setq new-timeline
          (cl-remove-if (lambda (entry)
                          (<= (- (second entry) (first entry))
                              threshold))
                        timeline))

    ;; reset start and end times
    (setf (first (first new-timeline)) start)
    (setf (second (car (last new-timeline))) end)

    ;; distribute gaps evenly among adjacent slots
    (do ((sub-timeline new-timeline (cdr sub-timeline)))
        ((null (cdr sub-timeline)))
      (let* ((entry-1 (first sub-timeline))
             (entry-2 (second sub-timeline))
             (end-1 (second entry-1))
             (start-2 (first entry-2)))
        (when (not (= end-1 start-2))
          (let ((middle (/ (+ end-1 start-2) 2)))
            (setf (second entry-1) middle)
            (setf (first entry-2) middle)))))
    new-timeline))

;;;###autoload
(defun timeline-tools-format-timeline (tstart tend &optional files)
  "Display timeline of tasks between TSTART and TEND from FILES.

When not given, FILES defaults to `org-agenda-files’.  Short
slots are removed, and afterwards slots are clusted by category.
When called interactively, START and END are queried with
`org-read-date’."
  (interactive (list (org-read-date nil nil nil "Start time: ")
                     (org-read-date nil nil nil "End time: ")))
  (let ((timeline (->> (timeline-tools-timeline tstart tend files)
                       ;; XXX: make these modifiers customizable
                       timeline-tools-cluster-same-category
                       (timeline-tools-skip-short-entries
                        timeline-tools-short-task-threshold)
                       timeline-tools-cluster-same-category)))
    (let ((target-buffer (get-buffer-create " *Org Timeline*")))
      (with-current-buffer target-buffer
        (erase-buffer)
        (org-mode)
        (insert "|--|\n")
        (insert "| Category | Start | End | Duration | Task |\n")
        (insert "|--|\n")
        (dolist (cluster timeline)
          (cl-destructuring-bind (start end markers) cluster
            (insert (format "| %s | %s | %s | %s min | "
                            (org-entry-get (first markers) "CATEGORY")
                            (format-time-string "%Y-%m-%d %H:%M" start)
                            (format-time-string "%Y-%m-%d %H:%M" end)
                            (floor (/ (- end start) 60))))
            ;; insert headline line by line, but only once
            (dolist (headline (->> (mapcar #'timeline-tools-get-headline markers)
                                   -uniq
                                   (-interpose "|\n |||||")))
              (insert headline))
            (insert "\n")))
        (insert "|--|\n")
        (goto-char (point-min))
        (org-table-align))
      (display-buffer target-buffer)
      t)))

;;;###autoload
(defun timeline-tools-format-timeline-of-day (date &optional files)
  "Format timeline of given DATE.

DATE should be a string of the form %Y-%m-%d.  When called
interactively, this date will be queried with `org-read-date’.
The timeline will be formatted for DATE starting at 00:00 and
ending at 23:61.  When not given, FILES defaults to
`org-agenda-files’."
  (interactive (list (org-read-date nil nil)))
  (timeline-tools-format-timeline (concat date " 00:00")
                                  (concat date " 23:61")
                                  files))


;;; Manipulating Clocklines

(defun timeline-tools-add-clockline-to-marker (target-marker start end)
  "Add clock line to task under TARGET-MARKER from START to END.

START and END must be given as time objects as returned by
`encode-time’, or as an integer or float denoting seconds since
1970-01-01.  TARGET-MARKER must be positioned on the task where
the clock line is to be added to."
  (when (not (markerp target-marker))
    (user-error "Marker not valid"))
  (let ((new-start (float-time start))
        (new-end   (float-time end)))
    (with-current-buffer (marker-buffer target-marker)
      (timeline-tools-map-clocklines
       (lambda (timestamp-1 timestamp-2)
         (let ((current-start (float-time
                               (apply #'encode-time
                                      (org-parse-time-string timestamp-1))))
               (current-end   (float-time
                               (apply #'encode-time
                                      (org-parse-time-string timestamp-2))))
               (kill-whole-line nil)    ; don’t delete newlines if not asked to
               )
           (cond
            ;; if the current clock line is completely contained within the
            ;; given period, delete it
            ((and (<= new-start current-start current-end new-end))
             (kill-whole-line))
            ;; if the current clock line completely contains the given one,
            ;; split it
            ((and (<= current-start new-start new-end current-end))
             (beginning-of-line)
             (kill-line)
             (timeline-tools-insert-clockline current-start new-start)
             (open-line 1)
             (timeline-tools-insert-clockline new-end current-end))
            ;; New interval overlaps beginning of current line
            ((<= new-start current-start new-end current-end)
             (beginning-of-line)
             (kill-line)
             (timeline-tools-insert-clockline new-end current-end))
            ;; New interval overlaps at end of current line
            ((<= current-start new-start current-end new-end)
             (beginning-of-line)
             (kill-line)
             (timeline-tools-insert-clockline current-start new-start)))))

       ;; Keep headline as they are, i.e., do nothing
       (lambda ())))

    ;; Finally add the new clock line
    (org-with-point-at target-marker
      (org-clock-find-position nil)
      (open-line 1)
      (timeline-tools-insert-clockline new-start new-end))))

(defun timeline-tools-copy-clocklines (source-id target-id)
  "Copy clock lines from SOURCE-ID to TARGET-ID.

Both SOURCE-ID and TARGET-ID must designate known `org-mode’
tasks by their ID.  Copies all clock lines attached to SOURCE-ID
or to one of its subtree, and adapts the clock lines in the file
of TARGET-ID accordingly."
  (let ((source-marker (org-id-find source-id :get-marker))
        (target-marker (org-id-find target-id :get-marker)))
    (cl-assert (markerp source-marker)
               "Source task %s not found" source-id)
    (cl-assert (markerp target-marker)
               "Target task %s not found" target-id)

    ;; We first fetch the relevant clock-lines into memory, and then add them to
    ;; the target task one by one, adjusting the other clock lines in between;
    ;; this is rather inefficient, but we will fix this only when we need it.
    (dolist (clock-line (timeline-tools-clocklines-of-task source-marker))
      (timeline-tools-add-clockline-to-marker target-marker
                                       (car clock-line) (cdr clock-line)))))

;; XXX: This needs some autoloadable frontend

(provide 'timeline-tools)
;;; timeline-tools.el ends here
