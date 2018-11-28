;;; timeline-tools.el -- Utilities to manipulate org-mode timelines -*- lexical-binding: t -*-

;;; Commentary:

;; XXX: give brief overview, explain nomenclature (timelines, clock-lines,
;; entries, ...), then list main functionality

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

(defcustom timeline-tools-filter-functions
  (list #'timeline-tools-remove-short-entries
        #'timeline-tools-cluster-same-entries)
  "List of functions to apply when formatting timelines.
Filter are applied in the order they are given in this list."
  :group 'timeline-tools
  :type '(list function))

(defcustom timeline-tools-short-task-threshold 300
  "Duration of task to be considered as short."
  :group 'timeline-tools
  :type 'integer)


;; Mode definition

(defvar timeline-tools--current-time-start nil
  "Current start time of the displayed timeline.")

(defvar timeline-tools--current-time-end nil
  "Current end time of the displayed timeline.")

(defvar timeline-tools--current-files nil
  "Files from which the current timeline has been extracted.")

(defvar timeline-tools--current-timeline nil
  "Currently displayed timeline in abstract form.")

(defvar timeline-tools-headline-time-format "%Y-%m-%d %H:%M"
  "Format of time used in the headline of a timeline.")

(defvar timeline-tools-time-format "%Y-%m-%d %H:%M"
  "Format of time used inside a timeline")

(defvar timeline-tools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'undefined)
    (define-key map "r" #'timeline-tools-redraw-timeline)
    (define-key map "R" #'timeline-tools-reparse-timeline)
    (define-key map "f" #'timeline-tools-forward-day)
    (define-key map "b" #'timeline-tools-backward-day)
    (define-key map "s" #'timeline-tools-skip-short-entries)
    (define-key map (kbd "RET") #'timeline-tools-jump-to-headline)
    (define-key map "q" #'quit-window)
    (define-key map (kbd "C-k") #'timeline-tools-kill-line)
    (define-key map "k" #'timeline-tools-kill-line)
    (define-key map (kbd "C-n") #'timeline-tools-next-line)
    (define-key map "n" #'timeline-tools-next-line)
    (define-key map (kbd "C-p") #'timeline-tools-previous-line)
    (define-key map "p" #'timeline-tools-previous-line)
    map))

(define-derived-mode timeline-tools-mode
  org-mode "Timeline"
  "Major mode to display org-mode timelines.")


;; Model

(defalias 'timeline-tools-entry-start-time 'car
  "Start time of ENTRY.")

(defalias 'timeline-tools-entry-end-time 'cadr
  "End time of ENTRY.")

(defalias 'timeline-tools-entry-marker 'caddr
  "Marker to org task of ENTRY.")

(defun timeline-tools-make-entry (start-time end-time marker)
  "Return a timeline entry made up of START-TIME, END-TIME, and MARKER.
MARKER must be a single marker."
  (unless (markerp marker)
    (user-error "No marker given."))
  (list start-time end-time marker))

(defun timeline-tools-entry-duration (entry)
  "Returns the duration of ENTRY, in minutes."
  (floor (/ (- (timeline-tools-entry-end-time entry)
               (timeline-tools-entry-start-time entry))
            60)))

(defun timeline-tools-entry-category (entry)
  "Return ARCHIVE_CATEGORY or CATEGORY at position given by MARKER.
Return whatever is found first."
  (let ((marker (timeline-tools-entry-marker entry)))
    (or (org-entry-get marker "ARCHIVE_CATEGORY")
        (org-entry-get marker "CATEGORY"))))

(defun timeline-tools-entry-headline (entry)
  "Return the headline associated with ENTRY."
  (let* ((marker (timeline-tools-entry-marker entry)))
    (plist-get (cadr (org-with-point-at marker
                       (org-element-headline-parser (point-max))))
               :raw-value)))


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
    ;; Make sure everything is visible, as otherwise editing may produce odd
    ;; results
    (org-cycle '(64))

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

(defvar timeline-tools-org-inactive-timestamp-format
  (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")
  "Format of inactive `org-mode’ timestamps.
Can be used as format string for `format-time’.")

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

(defun timeline-tools-clocklines-of-task (pom)
  "Return list of all clock lines of task under POM.

Each clock line is represented as a cons cell (START . END),
where both START and END are the starting and ending times of the
corresponding clock lines, encoded as a float denoting the
seconds since the epoch.  Includes clock lines of all subtrees as
well.  The order of the entries in the resulting list will be
reversed of what it is in the subtree of POM."
  (let ((clock-lines nil))
    (save-mark-and-excursion
     (org-with-point-at pom
       (org-narrow-to-subtree)
       (timeline-tools-map-clocklines
        (lambda (start end)
          (push (cons (org-time-string-to-seconds start)
                      (org-time-string-to-seconds end))
                clock-lines))
        #'ignore)))
    clock-lines))

(defun timeline-tools-format-entry-time (entry type)
  "Return time of ENTRY of type TYPE formatted as string.

The format used is specified by the value of `timeline-tools-time-format’."
  (format-time-string
   timeline-tools-time-format
   (cl-ecase type
     ((start) (timeline-tools-entry-start-time entry))
     ((end) (timeline-tools-entry-end-time entry)))))


;; Reporting

(defun timeline-tools-clocklines-in-range (tstart tend)
  "Return tasks in current buffer between TSTART and TEND.

The resulting list consists of elements of the form

  (MARKER . CLOCK-TIMES)

where MARKER is a marker to the beginning of the corresponding
heading and CLOCK-TIMES is a list of cons cells of the
form (START . END), where START and END are the starting and
ending times of a clock line for this task.  START and END are
given as seconds since the epoch, as a floating point number.
Truncation with respect to TSTART and TEND is done, i.e., START
or END will always be in the interval [TSTART,TEND]."
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
           (let* ((ts (org-time-string-to-seconds start))
                  (te (org-time-string-to-seconds end)))
             (when (or (<= tstart te tend)
                       (<= tstart ts tend)
                       (<= ts tstart tend te))
               (push (cons (max ts tstart)
                           (min te tend))
                     times))))
       ;; when on headlines, store away collected clocklines
       #'(lambda ()
           ;; add currently running clock if wanted
           (when (and org-clock-report-include-clocking-task
                      (eq (org-clocking-buffer) (current-buffer))
                      (eq (marker-position org-clock-hd-marker) (point)))
             (let ((current-clock-start (float-time org-clock-start-time))
                   (current-clock-end (float-time)))
               (when (or (<= tstart current-clock-start tend)
                         (<= tstart current-clock-end tend)
                         (<= current-clock-start
                             tstart tend
                             current-clock-end))
                 (push (cons (max current-clock-start tstart)
                             (min current-clock-end tend))
                       times))))
           ;; store away clocklines of current headline
           (when (not (null times))
             (push (cons (point-marker) times) task-clock-times)
             (setq times nil))))
      task-clock-times)))

(defun timeline-tools-timeline (tstart tend &optional files)
  "Return list of timeline entries between TSTART and TEND from FILES.

Each entry consists of a START-TIME, END-TIME, and MARKER are as
returned by `timeline-tools-clocklines-in-range’, which see.
Entries in the resulting list are sorted by START, ascending.  If
not given, FILES defaults to `org-agenda-files’ including all
archives."
  (let (timeline-of-files turned-around-timeline)
    (setq timeline-of-files
          (->> (or files (org-agenda-files t t))
               (cl-remove-if-not #'file-exists-p)
               (cl-mapcan #'(lambda (file)
                              (with-current-buffer (or (get-file-buffer file)
                                                       (find-file-noselect file))
                                (timeline-tools-clocklines-in-range tstart tend))))))
    ;; collect clock-lines in timeline and convert them to proper entries
    (dolist (entry timeline-of-files)
      (dolist (clock-time (cdr entry))
        (push (timeline-tools-make-entry (car clock-time) (cdr clock-time) (car entry))
              turned-around-timeline)))

    ;; sort timeline
    (sort turned-around-timeline
          (lambda (entry-1 entry-2)
            (< (timeline-tools-entry-start-time entry-1)
               (timeline-tools-entry-start-time entry-2))))))

(defun timeline-tools-cluster-same-entry (timeline)
  "Cluster TIMELINE into consecutive entries with equal marker.
This only works if every entry in timeline consists of a
singleton marker only.  In case this is not satisfied, this
function will throw an error."
  (let ((new-timeline (-partition-by #'(lambda (entry)
                                         (timeline-tools-entry-marker entry))
                                     timeline)))
    (mapcar (lambda (cluster)
              (timeline-tools-make-entry
               (timeline-tools-entry-start-time (-first-item cluster))
               (timeline-tools-entry-end-time (-last-item cluster))
               (timeline-tools-entry-markers (-first-item cluster))))
            new-timeline)))

(defun timeline-tools-remove-short-entries (timeline &optional threshold)
  "Remove entries from TIMELINE shorter than THRESHOLD.

A slot is short if it is not longer than THRESHOLD seconds.
Resulting gaps are distributed evenly among adjacent slots.
THRESHOLD defaults to the value of
`timeline-tools-short-task-threshold’ if not supplied."
  (let ((start (timeline-tools-entry-start-time (-first-item timeline)))
        (end   (timeline-tools-entry-end-time (-last-item timeline)))
        (threshold (or threshold timeline-tools-short-task-threshold))
        new-timeline)

    ;; remove all slots that are too short
    (setq new-timeline
          (cl-remove-if (lambda (entry)
                          (<= (- (timeline-tools-entry-end-time entry)
                                 (timeline-tools-entry-start-time entry))
                              threshold))
                        timeline))

    ;; reset start and end times
    (setf (timeline-tools-entry-start-time (-first-item new-timeline)) start)
    (setf (timeline-tools-entry-end-time (-last-item new-timeline)) end)

    ;; distribute gaps evenly among adjacent slots
    (cl-do
        ((sub-timeline new-timeline (cdr sub-timeline)))
        ((null (cdr sub-timeline)))
      (let* ((entry-1 (-first-item sub-timeline))
             (entry-2 (-second-item sub-timeline))
             (end-1   (timeline-tools-entry-end-time entry-1))
             (start-2 (timeline-tools-entry-start-time entry-2)))
        (when (not (= end-1 start-2))
          (let ((middle (/ (+ end-1 start-2) 2)))
            (setf (timeline-tools-entry-end-time entry-1) middle)
            (setf (timeline-tools-entry-start-time entry-2) middle)))))

    new-timeline))

(defun timeline-tools-transform-timeline (timeline)
  "Return timeline from files, after application of
`timeline-tools-filter-functions’."
  (-reduce-from (lambda (tl f)
                  (funcall f tl))
                timeline
                timeline-tools-filter-functions))

;;;###autoload
(defun timeline-tools-format-timeline (tstart tend &optional files)
  "Display timeline of tasks between TSTART and TEND from FILES.

When not given, FILES defaults to `org-agenda-files’ including
archives.  The timeline is transformed as given by the current
value of `timeline-tools-filter-functions’.  When called
interactively, START and END are queried with `org-read-date’."
  (interactive (list (org-read-date nil nil nil "Start time: ")
                     (org-read-date nil nil nil "End time: ")))
  (let* ((timeline (timeline-tools-transform-timeline
                    (timeline-tools-timeline tstart tend files))))
    (let ((target-buffer (get-buffer-create " *Org Timeline*")))
      (with-current-buffer target-buffer
        (timeline-tools-mode)
        (setq-local timeline-tools--current-time-start (org-time-string-to-seconds tstart))
        (setq-local timeline-tools--current-time-end (org-time-string-to-seconds tend))
        (setq-local timeline-tools--current-files files)
        (setq-local timeline-tools--current-timeline timeline)
        (setq-local timeline-tools-time-format timeline-tools-time-format)
        (setq-local timeline-tools-headline-time-format timeline-tools-headline-time-format)
        (hl-line-mode)
        (timeline-tools-redraw-timeline))
      (pop-to-buffer target-buffer)
      t)))

;;;###autoload
(defun timeline-tools-format-timeline-of-day (date &optional files)
  "Format timeline of given DATE.

DATE should be a string of the form %Y-%m-%d.  When called
interactively, this date will be queried with `org-read-date’.
When not given, FILES defaults to `org-agenda-files’ including
archives."
  (interactive (list (org-read-date nil nil)))
  (let ((timeline-tools-time-format "%H:%M")
        (timeline-tools-headline-time-format "%Y-%m-%d"))
   (timeline-tools-format-timeline (concat date " 00:00")
                                   (org-read-date nil nil "++1d" nil
                                                  (org-time-string-to-time date))
                                   files)))


;; Interactive functions

(defun timeline-tools-redraw-timeline ()
  "Redraw timeline of current buffer"
  (interactive)
  (if (not (eq major-mode 'timeline-tools-mode))
      (user-error "Not in Timeline buffer")
    (let ((timeline timeline-tools--current-timeline))
      (erase-buffer)
      (insert (format "* Timeline from [%s] to [%s]\n\n"
                      (format-time-string timeline-tools-headline-time-format
                                          timeline-tools--current-time-start)
                      (format-time-string timeline-tools-headline-time-format
                                          timeline-tools--current-time-end)))
      (insert "|--|\n")
      (insert "| Category | Start | End | Duration | Task |\n")
      (let ((last-category nil))
        (dolist (line timeline)
          (when (not (equal last-category (timeline-tools-entry-category line)))
            (insert "|--|\n")
            (setq last-category (timeline-tools-entry-category line)))
          (insert (format "| %s | %s | %s | %s min | %s | \n"
                          (timeline-tools-entry-category line)
                          (timeline-tools-format-entry-time line 'start)
                          (timeline-tools-format-entry-time line 'end)
                          (timeline-tools-entry-duration line)
                          (propertize (timeline-tools-entry-headline line)
                                      'marker (timeline-tools-entry-marker line)
                                      'entry line)))))
      (insert "|--|\n")
      (org-table-align)
      (goto-char (point-min))
      (timeline-tools-next-line))))

(defun timeline-tools-reparse-timeline ()
  "Parse timeline from files again and redraws current display
Updates category properties before constructing the new timeline."
  (interactive)
  (dolist (file timeline-tools--current-files)
    (with-current-buffer (get-file-buffer file)
      (org-refresh-category-properties)))
  (setq-local timeline-tools--current-timeline
              (timeline-tools-transform-timeline
               (timeline-tools-timeline
                timeline-tools--current-time-start
                timeline-tools--current-time-end
                timeline-tools--current-files)))
  (timeline-tools-redraw-timeline))

(defun timeline-tools-forward-day ()
  "Display timeline of next day."
  (interactive)
  (if (not (eq major-mode 'timeline-tools-mode))
      (user-error "Not in Timeline buffer")
    (setq-local timeline-tools--current-time-start (+ 86400 timeline-tools--current-time-start))
    (setq-local timeline-tools--current-time-end (+ 86400 timeline-tools--current-time-end))
    (setq-local timeline-tools--current-timeline
                (timeline-tools-transform-timeline
                 (timeline-tools-timeline
                  timeline-tools--current-time-start
                  timeline-tools--current-time-end
                  timeline-tools--current-files)))
    (timeline-tools-redraw-timeline)))

(defun timeline-tools-backward-day ()
  "Display timeline of next day."
  (interactive)
  (if (not (eq major-mode 'timeline-tools-mode))
      (user-error "Not in Timeline buffer")
    (setq-local timeline-tools--current-time-start
                (- timeline-tools--current-time-start 86400))
    (setq-local timeline-tools--current-time-end
                (- timeline-tools--current-time-end 86400))
    (setq-local timeline-tools--current-timeline
                (timeline-tools-transform-timeline
                 (timeline-tools-timeline
                  timeline-tools--current-time-start
                  timeline-tools--current-time-end
                  timeline-tools--current-files)))
    (timeline-tools-redraw-timeline)))

(defun timeline-tools-skip-short-entries ()
  "Skip entries in current timeline that are too short.
Interactively query for the exact value of \"short\"."
  (interactive)
  (when (not (eq major-mode 'timeline-tools-mode))
    (user-error "Not in Timeline buffer"))
  (let ((threshold (string-to-number
                    (read-from-minibuffer "Maximum time for short entries (in seconds): "))))
    (setq-local timeline-tools--current-timeline
                (timeline-tools-remove-short-entries
                 timeline-tools--current-timeline threshold))
    (timeline-tools-redraw-timeline)))

(defun timeline-tools-jump-to-headline ()
  "Jump to headline of current entry, if available."
  (interactive)
  (unless (eq major-mode 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))
  (let ((marker (save-mark-and-excursion
                 (end-of-line)
                 (org-table-previous-field)
                 (get-text-property (point) 'marker))))
    (unless marker
      (user-error "Not on headline to jump to"))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-reveal)))

(defun timeline-tools-kill-line ()
  "Delete line at point from the current timeline."
  (interactive)
  (unless (eq major-mode 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))
  (save-mark-and-excursion
   ;; get actual entry from headline of line
   (end-of-line)
   (org-table-previous-field)
   (let ((entry (get-text-property (point) 'entry)))
     (unless entry
       (user-error "Not on valid row in timeline."))
     (unless (< 1 (length timeline-tools--current-timeline))
       (user-error "Cannot delete last line."))
     (setq-local timeline-tools--current-timeline
                 (timeline-tools-transform-timeline
                  (delq entry timeline-tools--current-timeline))))
   (timeline-tools-redraw-timeline)))

(defun timeline-tools-next-line ()
  "Move point to next line in timetable, if possible."
  (interactive)
  (unless (eq major-mode 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))
  (beginning-of-line)
  (let ((point (point)))
    (when (looking-at "^| ")
      (forward-line))
    (unless (re-search-forward "^| " nil 'no-error)
      (goto-char point)
      (user-error "No next line"))
    (beginning-of-line)))

(defun timeline-tools-previous-line ()
  "Move point to previous line in timetable, if possible."
  (interactive)
  (unless (eq major-mode 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))
  (beginning-of-line)
  (let ((point (point)))
    (unless (re-search-backward "^| " nil 'no-error)
      (goto-char point)
      (user-error "No previous line"))
    (beginning-of-line)))


;;; Manipulating Clocklines

;; XXX: All this needs some autoloadable frontend

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
         (let ((current-start (org-time-string-to-seconds timestamp-1))
               (current-end   (org-time-string-to-seconds timestamp-2))
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
       #'ignore))

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

(defun timeline-tools-copy-inverted-clocklines (source-id target-id)
  "Copy clock lines from SOURCE-ID to TARGET-ID.

Both SOURCE-ID and TARGET-ID must designate known `org-mode’
tasks by their ID.  Considers all clock lines attached to
SOURCE-ID or to one of its subtree, and generates clock lines
starting at an end time of one clock line and ending at the start
time of the consecutive clock line.  These inverted clock lines
are then copied to TARGET-ID and clock lines in the file of
TARGET-ID are adapted accordingly."
  (let ((source-marker (org-id-find source-id :get-marker))
        (target-marker (org-id-find target-id :get-marker)))
    (cl-assert (markerp source-marker)
               "Source task %s not found" source-id)
    (cl-assert (markerp target-marker)
               "Target task %s not found" target-id)

    (let (inverted-timeline)
      (dolist (clock-line (timeline-tools-clocklines-of-task source-marker))
        (push (cdr clock-line) inverted-timeline)
        (push (car clock-line) inverted-timeline))

      (setq inverted-timeline (-partition 2 (rest (reverse inverted-timeline))))

      ;; This is inefficient, but see comment in
      ;; `timeline-tools-copy-clocklines’ for rationale.
      (dolist (clock-line inverted-timeline)
        (timeline-tools-add-clockline-to-marker target-marker
                                                (cadr clock-line) (car clock-line))))))

(provide 'timeline-tools)
;;; timeline-tools.el ends here
