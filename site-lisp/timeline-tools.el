;;; timeline-tools.el -- Utilities to manipulate org-mode timelines -*- lexical-binding: t -*-

;;; Commentary:

;; This package is a home-grown solution for displaying easily readable
;; overviews of the clock times in my org-agenda files.  I find the display
;; easier to read than the clock entries as shown in the agenda with log mode
;; enabled.  Some additional editing functions help my with bringing the
;; displayed timelines into the shape as required for work.

;; The main entry point to this package is
;; `timeline-tools-format-timeline-of-day’, which when called interactively
;; queries for a date to display, and then formats all clocked times in a nicely
;; aligned org-mode table.  Navigation in these tables as well as going forward
;; and backward by day are implemented as shortcuts; see the mode description
;; for more.  The clock times are assumed to be non-overlapping, i.e., no point
;; in time is contained in the clock of more than one entry.

;; The main data structures used in this package are as follows:
;;
;; - Timelines: these are lists of entries (see next).
;; - Timeline Entries: structures consisting of start times,
;;   end times, durations, headlines, and categories.
;;
;; To retrieve data from timeline entries, corresponding functions are provided.

;; XXX: talk about the way the timeline is generated
;; XXX: talk about filters
;; XXX: This needs some tests
;; XXX: timelines should have some metadata on their own (start time, end time,
;; files)

;;; Code:

(require 'dash)
(require 'org)
(require 'org-clock)
(require 'org-id)
(require 'org-element)


;; Customization

(defgroup timeline-tools nil
  "Functionality for manipulating timelines."
  :tag "Timeline Tools"
  :group 'applications)

(defcustom timeline-tools-filter-functions
  (list #'timeline-tools-fill-gaps
        #'timeline-tools-cluster-same-entries)
  "List of functions to apply when formatting timelines.
Filter are applied in the order they are given in this list."
  :group 'timeline-tools
  :type '(list function))

(defcustom timeline-tools-short-task-threshold 300
  "Duration of task to be considered as short."
  :group 'timeline-tools
  :type 'integer)

(defcustom timeline-tools-headline-time-format "%Y-%m-%d %a %H:%M"
  "Format of time used in the headline of a timeline."
  :group 'timeline-tools
  :type 'string)

(defcustom timeline-tools-time-format "%Y-%m-%d %H:%M"
  "Format of time used inside a timeline."
  :group 'timeline-tools
  :type 'string)

(defcustom timeline-tools-category-function 'timeline-tools-entry-category
  "Function for extracting the category of an entry when formatting a timeline.

This function is supposed to return a string representing the
category of a timeline entry, as it is used when formatting the
timeline with `timeline-tools-redraw-timeline’.  It receives
three arguments, namly the entry itself, the start date, and the
end date of the timeline."
  :group 'timeline-tools
  :type 'function)

(defcustom timeline-tools-extra-properties nil
  "Extra properties to display with the headline.

This must be a list of Org properties (given as strings) that are
extracted from the each heading to be displayed after the headline.
Properties are extracted with inheritance enabled."
  :group 'timeline-tools
  :type '(repeat string))


;; Mode definition

(defvar timeline-tools--current-time-start nil
  "Current start time of the displayed timeline.")

(defvar timeline-tools--current-time-end nil
  "Current end time of the displayed timeline.")

(defvar timeline-tools--current-files nil
  "Files from which the current timeline has been extracted.")

(defvar timeline-tools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'undefined)
    (define-key map "r" #'timeline-tools-redraw-timeline)
    (define-key map "g" #'timeline-tools-redraw-timeline)
    (define-key map "R" #'timeline-tools-reparse-timeline)
    (define-key map "f" #'timeline-tools-forward-day)
    (define-key map "b" #'timeline-tools-backward-day)
    (define-key map (kbd "RET") #'timeline-tools-jump-to-headline)
    (define-key map "q" #'quit-window)
    (define-key map (kbd "C-k") #'timeline-tools-kill-line)
    (define-key map "k" #'timeline-tools-kill-line)
    (define-key map (kbd "C-n") #'timeline-tools-next-line)
    (define-key map "n" #'timeline-tools-next-line)
    (define-key map "N" #'timeline-tools-swap-line-with-next)
    (define-key map (kbd "C-p") #'timeline-tools-previous-line)
    (define-key map "p" #'timeline-tools-previous-line)
    (define-key map "P" #'timeline-tools-swap-line-with-previous)
    (define-key map "o" #'delete-other-windows)
    map))

(define-derived-mode timeline-tools-mode
  org-mode "Timeline"
  "Major mode to display `org-mode' timelines."
  (hl-line-mode)
  (buffer-enable-undo))


;; Model

(defun timeline-tools-entry-start-time (entry)
  "Start time of ENTRY."
  (car entry))

(defun timeline-tools-entry-end-time (entry)
  "End time of ENTRY."
  (cadr entry))

(defun timeline-tools-entry-marker (entry)
  "Marker to org task of ENTRY."
  (let ((mrk (caddr entry)))
    (unless (markerp mrk)
      (error "Not an entry marker for a timeline: %s" mrk))
    (unless (marker-buffer mrk)
      (error "Buffer of marker does not exist anymore: %s" mrk))
    mrk))

(gv-define-setter timeline-tools-entry-start-time
    (time entry) `(setcar ,entry ,time))
(gv-define-setter timeline-tools-entry-end-time
    (time entry) `(setcar (cdr ,entry) ,time))

(defun timeline-tools-make-entry (start-time end-time marker)
  "Return a timeline entry made up of START-TIME, END-TIME, and MARKER.
MARKER must be a single marker."
  (unless (markerp marker)
    (user-error "No marker given"))
  (list start-time end-time marker))

(defun timeline-tools-entry-duration (entry)
  "Return the duration of ENTRY, in minutes."
  (floor (/ (- (timeline-tools-entry-end-time entry)
               (timeline-tools-entry-start-time entry))
            60)))

(defun timeline-tools-entry-category (entry &rest _)
  "Return ARCHIVE_CATEGORY or CATEGORY at position given by marker of ENTRY.
Return whatever is found first."
  (let ((marker (timeline-tools-entry-marker entry)))
    (or (org-entry-get marker "ARCHIVE_CATEGORY")
        (org-entry-get marker "CATEGORY"))))

(defun timeline-tools-entry-headline (entry)
  "Return the headline associated with ENTRY."
  (let ((mrk (timeline-tools-entry-marker entry)))
    (concat (org-entry-get mrk "ITEM")
            (--when-let (->> (-map #'(lambda (property)
                                       (--when-let (org-entry-get mrk property :inherit)
                                         (format "%s: %s" property it)))
                                   timeline-tools-extra-properties)
                             (-remove #'null))
              (format " (%s)" (apply #'concat (-interpose ", " it)))))))


;; Utilities

(defun timeline-tools-map-clocklines (clockline-fn headline-fn)
  "Iterate point over all clocklines and headlines of the current buffer.

For each clockline, call CLOCKLINE-FN with the starting and
ending time as arguments and point on the beginning of the line.
For each headline, call HEADLINE-FN with no arguments and point
on the start of the headline.  Traversal will be done from the
end of the file upwards.  If the buffer is narrowed, only this
region will be traversed.  If either CLOCKLINE-FN or HEADLINE-FN
edit the current buffer, make sure to use `org-show-all' to show
all insivible elements; otherwise editing may result in
unpredictable behavior."
  (unless (eq major-mode 'org-mode)
    (user-error "Not in Org mode buffer, cannot parse clocklines"))

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
            (funcall headline-fn))))))))

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
    task-clock-times))

(defun timeline-tools-timeline (tstart tend &optional files-or-buffers)
  "Return timeline between TSTART and TEND from FILES-OR-BUFFERS.

Each entry consists of a START-TIME, END-TIME, and MARKER are as
returned by `timeline-tools-clocklines-in-range’, which see.
Entries in the resulting list are sorted by START, ascending.
TSTART and TEND must be valid time specifiers for
`timeline-tools-clocklines-in-range’.  If not given,
FILES-OR-BUFFERS defaults to `org-agenda-files’ without archives."
  (let (timeline-of-files turned-around-timeline)
    (setq timeline-of-files
          (->> (or files-or-buffers (org-agenda-files t nil))
               (cl-mapcan #'(lambda (file-or-buffer)
                              (let ((buffer (cond
                                             ((bufferp file-or-buffer)
                                              file-or-buffer)
                                             ((not (stringp file-or-buffer))
                                              (warn "Neither valid file nor buffer: %s" file-or-buffer)
                                              nil)
                                             ((not (file-exists-p file-or-buffer))
                                              (warn "File does not exist: %s" file-or-buffer)
                                              nil)
                                             (t (or (get-file-buffer file-or-buffer)
                                                    (find-file-noselect file-or-buffer))))))
                                (when buffer
                                  (with-current-buffer buffer
                                    (timeline-tools-clocklines-in-range tstart tend))))))))

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

(defun timeline-tools-cluster-same-entries (timeline)
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
               (timeline-tools-entry-marker (-first-item cluster))))
            new-timeline)))

(defun timeline-tools-remove-short-entries (timeline &optional threshold)
  "Remove entries from TIMELINE shorter than THRESHOLD.

A slot is short if it is not longer than THRESHOLD seconds.
Resulting gaps are distributed evenly among adjacent slots.
THRESHOLD defaults to the value of
`timeline-tools-short-task-threshold’ if not supplied.

This function destructively modifies its first argument."
  (unless (null timeline)
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
      (timeline-tools-fill-gaps new-timeline))))

(defun timeline-tools-fill-gaps (timeline)
  "Fill gaps in TIMELINE evenly.

This is achieved by extending the start time and the end time of
the surrounding entries equally.

This function destructively modifies its first argument."
  (cl-do
      ((sub-timeline timeline (cdr sub-timeline)))
      ((null (cdr sub-timeline)) timeline)
    (let* ((entry-1 (-first-item sub-timeline))
           (entry-2 (-second-item sub-timeline))
           (end-1   (timeline-tools-entry-end-time entry-1))
           (start-2 (timeline-tools-entry-start-time entry-2)))
      (when (not (= end-1 start-2))
        (let ((middle (/ (+ end-1 start-2) 2)))
          (setf (timeline-tools-entry-end-time entry-1) middle)
          (setf (timeline-tools-entry-start-time entry-2) middle))))))

(defun timeline-tools-transform-timeline (timeline)
  "Return result of filtering TIMELINE.
Filtering is done by applying all functions from
`timeline-tools-filter-functions’, in order."
  (-reduce-from (lambda (tl f)
                  (funcall f tl))
                timeline
                timeline-tools-filter-functions))


;; Interactive functions

;;;###autoload
(defun timeline-tools-format-timeline (tstart tend &optional files)
  "Display timeline of tasks between TSTART and TEND from FILES.

When not given, FILES defaults to `org-agenda-files’ without
archives.  The timeline is transformed as given by the current
value of `timeline-tools-filter-functions’.  When called
interactively, START and END are queried with `org-read-date’."
  (interactive (list (org-read-date nil nil nil "Start time: ")
                     (org-read-date nil nil nil "End time: ")))
  (let ((target-buffer (get-buffer-create " *Org Timeline*")))
    (with-current-buffer target-buffer
      (timeline-tools-mode)
      (setq-local timeline-tools--current-time-start (org-time-string-to-seconds tstart))
      (setq-local timeline-tools--current-time-end (org-time-string-to-seconds tend))
      (setq-local timeline-tools--current-files files)
      (setq-local timeline-tools-time-format timeline-tools-time-format)
      (setq-local timeline-tools-headline-time-format timeline-tools-headline-time-format)
      (timeline-tools-redraw-timeline 'force))
    (pop-to-buffer target-buffer)
    t))

;;;###autoload
(defun timeline-tools-format-timeline-of-day (date &optional files)
  "Format timeline of given DATE.

DATE should be a string of the form %Y-%m-%d.  When called
interactively, this date will be queried with `org-read-date’.
When not given, FILES defaults to `org-agenda-files’ without
archives."
  (interactive (list (org-read-date nil nil)))
  (let ((timeline-tools-time-format "%H:%M"))
   (timeline-tools-format-timeline (concat date " 00:00")
                                   (org-read-date nil nil "++1d" nil
                                                  (org-time-string-to-time date))
                                   files)))

(defun timeline-tools-redraw-timeline (&optional force)
  "Redraw timeline of current buffer.

If FORCE is non-nil, reparse the timeline using
`timeline-tools-timeline' within the time span given by the
current values of the relevant buffer local variables."
  (interactive)
  (if (not (eq major-mode 'timeline-tools-mode))
      (user-error "Not in Timeline buffer")

    (let ((timeline (timeline-tools-transform-timeline
                     (if force
                         (timeline-tools-timeline
                          timeline-tools--current-time-start
                          timeline-tools--current-time-end
                          timeline-tools--current-files)
                       (timeline-tools--get-timeline-from-buffer)))))

      ;; Update categories in all affected buffers to retrieve up-to-date information; find all
      ;; relevant buffers by checking the markers of all entries.
      (dolist (buffer (->> timeline
                           (mapcar #'timeline-tools-entry-marker)
                           (mapcar #'marker-buffer)
                           (cl-remove-duplicates)))
        (with-current-buffer buffer
          (org-refresh-category-properties)))

      (erase-buffer)

      ;; Header
      (insert (format "Timeline from [%s] to [%s]\n\n"
                      (format-time-string timeline-tools-headline-time-format
                                          timeline-tools--current-time-start)
                      (format-time-string timeline-tools-headline-time-format
                                          timeline-tools--current-time-end)))

      ;; Actual timeline
      (insert "|--|\n")
      (insert "| Category | Start | End | Duration | Task |\n")
      (let ((last-category nil)
            (current-category nil))
        (dolist (line timeline)
          (setq current-category (funcall timeline-tools-category-function
                                          line
                                          timeline-tools--current-time-start
                                          timeline-tools--current-time-end))
          (when (not (equal last-category current-category))
            (insert "|--|\n")
            (setq last-category current-category))
          (insert
           (propertize (format "| %s | %s | %s | %s min | %s | \n"
                               current-category
                               (timeline-tools-format-entry-time line 'start)
                               (timeline-tools-format-entry-time line 'end)
                               (timeline-tools-entry-duration line)
                               (timeline-tools-entry-headline line))
                       'entry line))))
      (insert "|--|\n")
      (org-table-align)

      (insert "\n")

      ;; Clocktime summary: booked categories, their total times, and their relative amount
      (let* ((data (->> timeline
                        (-group-by #'(lambda (entry) ; group by category
                                       (funcall timeline-tools-category-function
                                                entry
                                                (timeline-tools-entry-start-time entry)
                                                (timeline-tools-entry-end-time entry))))
                        (-map #'(lambda (category-lines) ; sum durations by category
                                  (list (car category-lines)
                                        (->> category-lines
                                             cdr
                                             (-map #'timeline-tools-entry-duration)
                                             -sum))))))
             (total-time (float (-sum (-map #'-second-item data)))))
        (insert "| Category | Total | Amount |\n|--|\n")
        (dolist (category-lines (-sort #'(lambda (entry-1 entry-2) ; sort output by booked time
                                           (>= (-second-item entry-1) (-second-item entry-2)))
                                       data))
          (insert (format "| %s | %s | %.2f%% |\n"
                          (-first-item category-lines)
                          (org-duration-from-minutes (-second-item category-lines))
                          (* 100 (/ (-second-item category-lines) total-time)))))
        (insert (format "|--|\n| | %s | |\n" (org-duration-from-minutes total-time)))
        (org-table-align)
        (insert "\n"))

      (goto-char (point-min))
      (timeline-tools-next-line))))

(defun timeline-tools--get-entry-from-point (&optional noerror)
  "Return timeline entry of point when in timeline buffer.

Errors out if there is no entry on the current line, unless
NOERROR is non-nil; in that case, return nil when no text
property could be found (note that this can also happen if point
is outside of the current table)."
  (unless (derived-mode-p 'timeline-tools-mode))
  (save-mark-and-excursion
    (if (not (org-at-table-p))
        (if noerror nil (user-error "Not in table"))
      (beginning-of-line)
      (if (not (looking-at "^| "))
          (if noerror nil (user-error "Not on valid row in timeline"))
        (org-table-next-field)
        (if-let ((entry (get-text-property (point) 'entry)))
            entry
          (if noerror nil (user-error "Not on valid row in timeline")))))))

(defun timeline-tools-next-line ()
  "Move point to next line in timetable, if possible.

Otherwise don't move point and error out."
  (interactive)
  (unless (derived-mode-p 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))
  (let ((point (point))
        found)
    (end-of-line)
    (while (and (setq found (re-search-forward "^| " nil t))
                (not (timeline-tools--get-entry-from-point 'noerror))))
    ;; Check whether we've found something when leaving the while-loop; if not,
    ;; go back and error out, as promised in the docstring.
    (unless found
      (goto-char point)
      (user-error "No next line")))
  (beginning-of-line))

(defun timeline-tools-previous-line ()
  "Move point to previous line in timetable, if possible.

Otherwise don't move point and error out."
  (interactive)
  (unless (eq major-mode 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))
  (beginning-of-line)
  (let ((point (point))
        found)
    (while (and (setq found (re-search-backward "^| " nil t))
                (not (timeline-tools--get-entry-from-point 'noerror))))
    ;; Check whether we've found something when leaving the while-loop; if not,
    ;; go back and error out, as promised in the docstring.
    (unless found
      (goto-char point)
      (user-error "No previous line")))
  (beginning-of-line))

(defun timeline-tools--get-timeline-from-buffer ()
  "Extract current timeline from buffer and return it.
This function expects the individual lines of a timeline to be
text properties under the keyword `entry' in the current buffer,
as it is done by `timeline-tools-redraw-timeline'."
  (if (not (eq major-mode 'timeline-tools-mode))
      (user-error "Not in Timeline buffer")
    (let (timeline)
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (zerop (forward-line))
          ;; scan line for a text property named `entry'
          (while (and (not (eolp))
                      (not (get-text-property (point) 'entry)))
            (forward-char))
          ;; if not at the end, we have found something … add it
          (unless (eolp)
            (push (get-text-property (point) 'entry) timeline)))
        (nreverse timeline)))))

(defun timeline-tools-reparse-timeline ()
  "Parse timeline from files again and redraw current display.
Updates category properties before constructing the new timeline."
  (interactive)
  (timeline-tools-redraw-timeline 'force))

(defun timeline-tools-forward-day ()
  "Display timeline of next day."
  (interactive)
  (if (not (eq major-mode 'timeline-tools-mode))
      (user-error "Not in Timeline buffer")
    ;; CAVEAT: this does not work properly when daylight time saving occured.
    ;; In that case, jump directly to the day you want to display.  This is not
    ;; going to be fixed.
    (setq-local timeline-tools--current-time-start (+ 86400 timeline-tools--current-time-start))
    (setq-local timeline-tools--current-time-end (+ 86400 timeline-tools--current-time-end))
    (timeline-tools-redraw-timeline 'force)))

(defun timeline-tools-backward-day ()
  "Display timeline of next day."
  (interactive)
  (if (not (eq major-mode 'timeline-tools-mode))
      (user-error "Not in Timeline buffer")
    (setq-local timeline-tools--current-time-start
                (- timeline-tools--current-time-start 86400))
    (setq-local timeline-tools--current-time-end
                (- timeline-tools--current-time-end 86400))
    (timeline-tools-redraw-timeline 'force)))

(defun timeline-tools-jump-to-headline ()
  "Jump to headline of current entry, if available."
  (interactive)
  (unless (eq major-mode 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))
  (let ((marker (timeline-tools-entry-marker (timeline-tools--get-entry-from-point))))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-reveal)))

(defun timeline-tools-kill-line ()
  "Delete line at point from the current timeline."
  (interactive)
  (unless (eq major-mode 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))

  ;; If there's an entry on the current line, delete it and redraw; the call to
  ;; `erase-buffer’ in `timeline-tools-redraw-timeline’ somehow makes
  ;; `save-mark-and-excursion’ meaningless; thus we save the number of the
  ;; current line by ourselves

  (if (timeline-tools--get-entry-from-point) ; barfs if there's no entry
      (kill-line))

  (let ((linenum (line-number-at-pos (point))))
    (timeline-tools-redraw-timeline)
    (goto-char (point-min))
    (forward-line (1- linenum))))

(defun timeline-tools-swap-line-with-next ()
  "Swap the current line with the next line in current timeline.

Durations of the entries are kept, the start and end times are
changed accordingly."
  (interactive)
  (unless (derived-mode-p 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))

  ;; The idea is to swap the two entries attached as text properties to the rows
  ;; and update the start end end times accordingly, and then redraw.

  (let (pos-entry-1 pos-entry-2 entry-1 entry-2)

    (save-mark-and-excursion
      (setq entry-1 (timeline-tools--get-entry-from-point)
            pos-entry-1 (point))
      (timeline-tools-next-line)
      (setq entry-2 (timeline-tools--get-entry-from-point)
            pos-entry-2 (point)))

    ;; Let's create new entries and overwrite the old ones.

    (let ((s1 (timeline-tools-entry-start-time entry-1))
          (m1 (timeline-tools-entry-marker entry-1))
          (s2 (timeline-tools-entry-start-time entry-2))
          (e2 (timeline-tools-entry-end-time entry-2))
          (m2 (timeline-tools-entry-marker entry-2))
          entry-1-new entry-2-new)

      (setq entry-1-new (timeline-tools-make-entry s1 (+ s1 (- e2 s2)) m2)
            entry-2-new (timeline-tools-make-entry (+ s1 (- e2 s2)) e2 m1))

      (save-mark-and-excursion
        (goto-char pos-entry-1)
        (put-text-property (line-beginning-position)
                           (line-end-position)
                           'entry entry-1-new)
        (goto-char pos-entry-2)
        (put-text-property (line-beginning-position)
                           (line-end-position)
                           'entry entry-2-new))

      (timeline-tools-redraw-timeline)

      ;; Stay on line we were previously, by searching for an entry that has
      ;; the same starting time as `entry-2-new', going there.

      ;; XXX: problem is that entries may get transformed by functions from
      ;; `timeline-tools-transform-timeline', so this approach is not correct
      ;; in general.

      (text-property-search-forward 'entry
                                    (timeline-tools-entry-start-time entry-2-new)
                                    #'(lambda (x y)
                                        (equal x (timeline-tools-entry-start-time y)))))))

(defun timeline-tools-swap-line-with-previous ()
  "Swap the current line with the previous line in current timeline.

See `timeline-tools-swap-line-with-next' for more details."
  (interactive)
  (unless (derived-mode-p 'timeline-tools-mode)
    (user-error "Not in Timeline buffer"))

  ;; Go to the previous line and swap those.
  (timeline-tools-previous-line)
  (timeline-tools-swap-line-with-next)
  (timeline-tools-previous-line))


;;; Manipulating Clocklines in Org Files

(defun timeline-tools-insert-clockline (time-1 time-2)
  "Insert new clock line from TIME-1 to TIME-2.

Insertion will be done at the beginning of the current line.
TIME-1 and TIME-2 must be given in a format understandable by
`format-time-string’, which see.  Saves mark and point.  If
TIME-2 is nil, insert dangling clock line."
  (save-mark-and-excursion
   (beginning-of-line)
   (indent-according-to-mode)
   (insert "CLOCK: ")
   (insert (format-time-string (org-time-stamp-format :with-time :inactive)
                               time-1))
   (unless (null time-2)
    (insert "--")
    (insert (format-time-string (org-time-stamp-format :with-time :inactive)
                                time-2))
    (org-clock-update-time-maybe))))

;; XXX: All this needs some autoloadable frontend

(defun timeline-tools-clockline-no-conflict (start end &rest buffers)
  "Return clock line string from START to END.

START and END must be suitable arguments for `float-time’.
Update conflicting clock lines in BUFFERS before returning the
clock line."
  (let ((new-start (float-time start))
        (new-end   (float-time end)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        ;; Make sure everything is visible, as otherwise editing may produce odd
        ;; results
        (org-fold-show-all)

        (timeline-tools-map-clocklines
         (lambda (timestamp-1 timestamp-2)
           (let ((current-start (org-time-string-to-seconds timestamp-1))
                 (current-end   (org-time-string-to-seconds timestamp-2))
                 (kill-whole-line nil)  ; don’t delete newlines if not asked to
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

         ;; Update current clock when on corresponding headline
         #'(lambda ()
             (when (and (eq (org-clocking-buffer) (current-buffer))
                        (eq (marker-position org-clock-hd-marker) (point)))
               (let ((current-start (float-time org-clock-start-time))
                     (kill-whole-line nil) ; don’t delete newlines if not asked to
                     )
                 (when (< current-start new-end)
                   (save-mark-and-excursion
                     (org-clock-find-position t)
                     (beginning-of-line)
                     (kill-line)
                     (when (< current-start new-start)
                       ;; Insert gap as separate clock line
                       (timeline-tools-insert-clockline current-start new-start)
                       (open-line 1))
                     (timeline-tools-insert-clockline new-end nil)
                     (setq org-clock-start-time (seconds-to-time new-end))))))))))

    ;; Return valid clockline
    (with-temp-buffer
      (timeline-tools-insert-clockline new-start new-end)
      (buffer-string))))

(defun timeline-tools-add-clockline-to-marker
    (target-marker start end &rest buffers)
  "Add clock line from START to END to task under TARGET-MARKER.

START and END must be given as time objects as returned by
`encode-time’, or as an integer or float denoting seconds since
1970-01-01.  TARGET-MARKER must be positioned on the task where
the clock line is to be added to.  BUFFERS must be a list of
buffers where to look for conflicting clock lines.  Those
conflicting clock lines are updated accordingly.  If BUFFERS is
not given, update clock lines in the buffer of TARGET-MARKER."
  (when (not (markerp target-marker))
    (user-error "Marker not valid"))
  (let ((new-start   (float-time start))
        (new-end     (float-time end))
        (org-buffers (if buffers buffers (list (marker-buffer target-marker)))))
    (let ((clock-line (apply #'timeline-tools-clockline-no-conflict
                             new-start new-end org-buffers)))
      (org-with-point-at target-marker
        (org-clock-find-position t)
        ;; if there is an unclosed clock line, add new clock line after it
        (when (and (looking-at "^CLOCK:")
                   (not (looking-at ".* => ")))
          (forward-line 1))
        (open-line 1)
        (insert clock-line)))))

;;;###autoload
(defun timeline-tools-clockline-no-org-agenda-conflicts ()
  "Read clock line from user and return it.

Update all files in `org-agenda-files’ to update conflicting
clock lines, without restrictions.  If `org-agenda-archive-mode’
is set, also include archive files."
  (let* ((now (format-time-string "%H:%M"))
         (start (org-read-date t nil nil "Started: " (current-time) now))
         (end (org-read-date t nil nil "Ended: " (current-time) now)))
    (apply #'timeline-tools-clockline-no-conflict
           (org-time-string-to-seconds start)
           (org-time-string-to-seconds end)
           (mapcar #'find-file-noselect
                   (cl-remove-if-not #'file-exists-p
                                     (org-agenda-files t 'ifmode))))))

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

      (setq inverted-timeline (-partition 2 (cl-rest (reverse inverted-timeline))))

      ;; This is inefficient, but see comment in
      ;; `timeline-tools-copy-clocklines’ for rationale.
      (dolist (clock-line inverted-timeline)
        (timeline-tools-add-clockline-to-marker target-marker
                                                (cadr clock-line) (car clock-line))))))

(provide 'timeline-tools)
;;; timeline-tools.el ends here
