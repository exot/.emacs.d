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
(require 'org-capture)
(require 'org-clock)
(require 'hydra)
(require 'db-customize)
(require 'ox-icalendar)
(require 'org-ql)
(require 'holidays)
(require 'dired)
(require 'bookmark)
(require 'consult-org)

(autoload 'which-function "which-func")
(autoload 'org-element-property "org-element")
(autoload 'db/org-agenda "db-utils")

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

This is an adaption of `org-agenda-list' to only show deadline items and
to not print a date – including the final newline.

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

(defun db/org-agenda-insert-active-filters (&optional _match)
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
The clocked time is computed over the complete subtree of the Org
item at point, and not only from the item itself.

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

(defun db/org-convert-item-to-headline ()
  "Convert list item around point to headline.

Search for the outermost list item enclosing point and convert it
to a Org headline.  Use the first line of the item as actual
headline, and move the rest to the body of the headline.  A
property CREATED is added with an inactive timestamp capturing
the current time.

The converted headline will be shown in a capture buffer for
further editing.  After finishing this buffer, the final headline
will be stored in `db/org-default-refilefile'.

Note that the original list item will be deleted before the
capture buffer will open.  If the buffer is aborted, the original
list item is _not_ restored.  This has to be done using the undo
history of the buffer."
  (interactive)

  (let ((element (org-element-at-point))
        last-seen-item)

    ;; Get outermost list item
    (while element
      (when (eq (org-element-type element) 'item)
        (setq last-seen-item element))
      (setq element (org-element-parent element)))

    (unless last-seen-item
      (user-error "Cannot find enclosing list item at point to convert to headline"))

    ;; Generate headline and store it somewhere
    (let* ((body (buffer-substring-no-properties (org-element-contents-begin last-seen-item)
                                                 (org-element-end last-seen-item)))
           (first-line-of-body (seq-take-while #'(lambda (x) (not (= x ?\n))) body))
           (rest-of-body (string-trim (seq-drop-while #'(lambda (x) (not (= x ?\n))) body))))

      ;; Remove old entry first
      (delete-region (org-element-begin last-seen-item)
                     (org-element-end last-seen-item))

      ;; Set up capture buffer
      (org-capture-put :key "")
      (org-capture-put :description "")
      (org-capture-put :target '(file db/org-default-refile-file))
      (org-capture-put :empty-lines-before 1)
      (org-capture-put :empty-lines-after 1)
      (org-capture-put :template (org-capture-fill-template
                                  (format "* TODO [#B] %s
:PROPERTIES:
:CREATED: %%U
:END:

Via %%(with-temp-buffer (db/org-add-link-to-current-clock) (string-trim (buffer-string))).

%s

%%?"
                                          ;; Quote % to prevent unintented expansion by
                                          ;; `org-capture-fill-template'.
                                          (replace-regexp-in-string "%" "\\\\%" first-line-of-body)
                                          (replace-regexp-in-string "%" "\\\\%" (org-remove-indentation rest-of-body)))))
      (org-capture-set-target-location)
      (org-capture-place-template)

      ;; Ensure that two line breaks are placed at the end of the heading
      (goto-char (point-max))
      (while (looking-back "\n" 1)
        (delete-char -1))
      (insert "\n\n"))))

(defun db/goto-quick-notes ()
  "Go to position where next quick note should be inserted.

Quick notes are inserted into `db/org-default-refile-file', directly
before the first page feed character.  Quick notes are expected to be
inserted as list items."
  (unless (file-writable-p db/org-default-refile-file)
    (user-error "Default refile file «%s» not writable" db/org-default-refile-file))

  (set-buffer (find-file db/org-default-refile-file))

  (goto-char (point-min))
  (unless (search-forward-regexp "^\f" nil t)
    (user-error "No page feed found in default refile file"))

  (beginning-of-line)
  (open-line 2))


;;; Refiling

(defun db/verify-refile-target ()
  "Verify that a certain location is eligible as a refile target."
  (and
   ;; Exclude DONE state tasks from refile targets (from bh)
   (not (member (nth 2 (org-heading-components))
                org-done-keywords))
   ;; Exclude HOLD items to prevent accidental refiling to such headings (they will not appear on
   ;; any agenda when a super-item is tagged with :HOLD:.
   (not (member "HOLD" (org-get-tags (point))))))

(defun db/org-refile-get-location (&optional prompt default-buffer new-nodes)
  "Replacement function for `org-refile-get-location' using `consult'.

This function can be used instead of `org-refile-get-location',
e.g. using `define-advice'.  The parameters PROMPT, DEFAULT-BUFFER, and
NEW-NODES have the same meaning.  However, setting NEW-NODES to a
non-nil value will result in a warning, as creating new headlings is not
supported with this function.

Also note that the usual variables governing the behavior of
`org-refile' do not have any effect here.  In particular,
`org-refile-verify-target-function' is not (yet) considered."
  (when new-nodes
    (warn "Cannot create new nodes (yet) with consult interface for `org-refile'"))
  (let ((pom (with-current-buffer (or default-buffer (current-buffer))
               (db/org-get-location t nil (concat prompt " "))))) ; TODO: incorporate verify
                                                                  ; function, use direct call to
                                                                  ; consult--read for this and
                                                                  ; replace the :predicate key?
    (list (buffer-name (marker-buffer pom))
          (buffer-file-name (marker-buffer pom))
          ;; The third entry is some regexp matching the headline, apparently?
          (format org-complex-heading-regexp-format (org-entry-get pom "ITEM"))
          (marker-position pom))))


;;; Helper Functions for Clocking

(defun db/find-parent-task ()
  ;; http://doc.norang.ca/org-mode.html#Clocking
  "Return point of the nearest parent task, and nil if no such task exists.

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
  "Try to find a clock to continue clocking and clock in there.

This functions tries to clock into the interrupted task, the
parent task, or the default task, in that order and only when
available, respectively.  Clock in to the interrupted task only
if it is not closed yet.

If none of the listed tasks is available, interactively query the
user for the next task to clock into."
  (when (and (not org-clock-clocking-in)
             (not org-clock-resolving-clocks-due-to-idleness))
    (let (parent-task)
      (save-mark-and-excursion
        (cond
          ((and (markerp org-clock-interrupted-task)
                (marker-buffer org-clock-interrupted-task)
                ;; Ensure that interrupted task is not the default task; in this case, we prefer to
                ;; continue with parent tasks instead.  We check equality of tasks by comparing
                ;; their ID properties, to avoid comparing misplaced marker that actually point to
                ;; the same heading.
                (or (not (markerp org-clock-default-task))
                    (not (marker-buffer org-clock-default-task))
                    (not (string= (org-with-point-at org-clock-interrupted-task (org-id-get-create))
                                  (org-with-point-at org-clock-default-task (org-id-get-create)))))
                (org-with-point-at org-clock-interrupted-task
                  (not (member (nth 2 (org-heading-components))
                               org-done-keywords))))
           ;; interrupted task is set and not closed yet, so let's clock in
           ;; there
           (org-with-point-at org-clock-interrupted-task
             (org-clock-in)))
          ((setq parent-task (db/find-parent-task))
           ;; found parent task
           (org-with-point-at parent-task
             (org-clock-in)))
          ((and (markerp org-clock-default-task)
                (marker-buffer org-clock-default-task))
           ;; default task is set
           (org-with-point-at org-clock-default-task
             (org-clock-in)))
          (t
           ;; We cannot determine what task to clock in next, so let the user choose a task to clock
           ;; in to; catch errors in case of typos and try again until a valid clock is running
           (while (not (org-clocking-p))
             (condition-case err
                 (org-clock-in '(4))
               (user-error              ; only handle user errors, to allow other errors to escape
                (message "Error: %s" (error-message-string err)) nil)))))))))

(defun db/save-current-org-task-to-file ()
  "Format currently clocked task and write it to`db/org-clock-current-task-file'."
  (with-temp-file db/org-clock-current-task-file
    (let ((clock-buffer (marker-buffer org-clock-marker)))
      (if (null clock-buffer)
          (insert "No running clock")
        (insert org-clock-heading)))))

(defun db/show-current-org-task ()
  "Show title of currently clock in task in minibuffer."
  (interactive)
  (message org-clock-current-task))

(defun db/org-clocked-time-for-current-item ()
  "Return overall clocked time for the subtree of the Org item at point.

The clocked time of the item itself is also included, as is the
time of the currently running clock, in case item at point is
clocked in."

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
                   (time-since org-clock-start-time))
                  60)
         0))))

(defun db/org-mark-current-default-task ()
  "Mark current task as default when equal to work task or home task.
Work task and home task are determined by the current values of
`org-working-task-id’ and `org-home-task-id’, respectively."
  (let ((current-id (org-id-get org-clock-marker)))
    (when (member current-id (list org-working-task-id
                                   org-home-task-id))
      (org-with-point-at org-clock-marker
        (message "Setting default task to: %s" (org-link-display-format (org-entry-get (point) "ITEM")))
        (org-clock-mark-default-task)))))

(defun db/org-clocktable-write-with-threshold (ipos tables params)
  "A clocktable formatter to filter entries by minimal clock time.

The arguments IPOS, TABLES, and PARAMS are as for the default
clocktable formatter `org-clocktable-write-default'.  The only
difference is that PARAMS may include an additional property
called `:clocktime-treshold' with a non-negative integer value.
If such a value is given, each table entry in TABLES whose TIME
value is below this threshold is removed before formatting the
clocktable (see `org-clock-get-table-data' for the meaning of
TIME)."
  (let ((tables tables))                ; necessary?
    (when-let ((threshold (plist-get params :clocktime-threshold)))
      (unless (and (integerp threshold) (>= threshold 0))
        (user-error "Clocktime threshold must be a non-negative integer, but is %s"
                    threshold))
      (setq tables (-map (pcase-lambda (`(,file-name ,file-time ,entries))
                           (list file-name
                                 file-time
                                 (-filter (pcase-lambda (`(_ _ _ _ ,time _))
                                            (>= time threshold))
                                          entries)))
                         tables)))
    (org-clocktable-write-default ipos tables params)))


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

      (insert (format "#+CAPTION: Workload Report at %s from %s until %s.\n"
                      (with-temp-buffer
                        ;; Is there an easier way to get the current time as an
                        ;; inactive timestamp?
                        (org-insert-time-stamp (current-time) t t)
                        (buffer-string))
                      (if start-date
                          (format "[%s]" (org-read-date nil nil start-date))
                        "The Beginning Of Time")
                      (if end-date
                          (format "[%s]" (org-read-date nil nil end-date))
                        "The End of Things")))
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

This overview report will list the amount of work planned for
increasing intervals from a fixed start time until a fixed end
date, devided in intervals of fixed increment.  The intervals are
determined in increments of +1d.  Tasks are read from files in
the variable `org-agenda-files'.

Next to the planned work, the accumulated available work hours
are displayed until the end of each interval, as well as the
utilization for each interval.  The available work hours are
computed by the value of `:work-hours' as described below.  Hours
already spent are subtracted from this value as computed as sum
of the clock time of all Org items as given by
`:work-items-match'.

PARAMS is a property list of the following parameters:

`:start-date':

  Start date for the workload report.  When not provided, will
  default to yesterday at 23:59.  When provided, must be in a format
  understood by `org-read-date'.

`:end-date':

  End date of the workload report.  Must be provided, in a format
  understood by `org-read-date'.  The end date is inclusive.

`:org-ql-match'

  `org-ql' expression (in sexp syntax) to filter the list of
  tasks to consider.  Defaults to (todo).

`:skip-matches'

  Regular expression to skip certain interval end dates that are
  not of interest.  The regular expression will be matched
  against the formatted timestamp.  For example, to skip
  weekends, use \"Sat\\|Sun\" as regular expression.

  This can also be a function taking a formatted timestamp as
  argument and returning non-nil when the date should be skipped.

`:work-hours'

  The time available per day for work, given as duration string,
  defauling to \"8:00\".  This can also be a function that
  returns the work hours for a given date.  In this case, this
  function receives the date formatted as an Org time stamp with
  time and without brackets.

`:work-items-match'

  An Org properties match that determines what constitutes when
  an Org item should be considered “work” and for which time
  spent today should be substracted from the available work hours."

  (let* ((start-date (org-read-date t t (or (plist-get params :start-date)
                                            "-1d 23:59")))
         (end-date (or (--if-let (plist-get params :end-date)
                           (org-read-date t t it))
                       (user-error "No valid end-date provided")))
         (increment "+1d")
         (org-ql-match (or (plist-get params :org-ql-match)
                           '(todo)))
         (timestamp-format "%Y-%m-%d %a %H:%M")
         (skip-date-p (pcase (plist-get params :skip-matches)
                        ((pred null) #'(lambda (_) nil))
                        ((and (pred stringp) arg)
                         #'(lambda (x) (string-match arg x)))
                        ((and (pred functionp) fun) fun)
                        (arg (user-error "Invalid argument to :skip-matches: %s" arg))))
         (work-items-match (plist-get params :work-items-match))
         (work-hours (pcase (plist-get params :work-hours)
                       ((pred null)
                        #'(lambda (date)
                            (db/remaining-work-hours-for-date date work-items-match "8:00")))
                       ((and (pred org-duration-p) arg)
                        #'(lambda (date)
                            (db/remaining-work-hours-for-date date work-items-match arg)))
                       ((and (pred functionp) fun)
                        #'(lambda (date)
                            (db/remaining-work-hours-for-date
                             date
                             work-items-match
                             (funcall fun (format-time-string (org-time-stamp-format t 'no-brackets)
                                                              date)))))
                       (arg (user-error "Invalid argument to :work-hours: %s" arg))))
         date-range)

    ;; Compute range of dates to check; simple but potentially costly approach taken from
    ;; https://sachachua.com/blog/2015/08/org-mode-date-arithmetic/; maybe consider
    ;; `org-read-date-get-relative' as well?
    (let ((current start-date)
          current-formatted)
      (while (or (time-less-p current end-date)
                 (time-equal-p current end-date))
        (setq current (org-read-date t t
                                     ;; Add an extra + to ensure we increase the amount of time
                                     ;; relative to the given default time string.
                                     (format "+%s" increment)
                                     nil current)
              current-formatted (format-time-string timestamp-format current))
        (unless (funcall skip-date-p current-formatted)
          (push (cons current current-formatted) date-range))))

    ;; Remove last day added when outside of range; reverse range afterwards to get correct sorting
    (setq date-range (nreverse (if (time-less-p end-date (caar date-range))
                                   (cdr date-range)
                                 date-range)))

    (insert (format "#+CAPTION: Workload Overview Report at [%s] with start date [%s]\n"
                    (format-time-string timestamp-format (current-time))
                    (format-time-string timestamp-format start-date)))
    (insert "| End Time | Planned Work | Work Hours | Utilization |\n| <r> | <r> | <r> | <r> |\n|---|\n")

    ;; Compute workload report for each date and record the total time
    (let ((total-work-hours 0))
      (pcase-dolist (`(,interval-end-date . ,interval-end-date-formatted) date-range)
        ;; XXX: repeatedly calling `db/org-planned-tasks-in-range' is a waste of resources
        (let* ((total-time-duration (car
                                     ;; Set start date to nil to also include tasks scheduled or
                                     ;; deadlined before `start-date', as those are also still open
                                     ;; and need to be done somewhen.
                                     (db/org-planned-tasks-in-range nil
                                                                    interval-end-date-formatted
                                                                    org-ql-match))))

          (cl-incf total-work-hours (org-duration-to-minutes (funcall work-hours interval-end-date)))

          (insert (format "| [%s] | %s | %s | %s |\n"
                          interval-end-date-formatted
                          total-time-duration
                          (org-duration-from-minutes total-work-hours)
                          ;; XXX: the following code might better go to a spearate function to
                          ;; increase comprehension
                          (let* ((total-time-minutes (org-duration-to-minutes total-time-duration)))
                            (cond
                             ((zerop total-time-minutes) "0.00%")
                             ((zerop total-work-hours) "*Inf*")
                             (t (let ((utilization (* (/ total-time-minutes total-work-hours) 100)))
                                  (if (<= 80 utilization)
                                      ;; When utilization is above 80%, mark entry in bold
                                      (format "*%.2f%%*" utilization)
                                    (format "%.2f%%" utilization)))))))))))

    (insert "|--|")
    (org-table-align)))

(defun db/org-insert-workload-overview-report ()
  "Create dynamic block of planned tasks in given time range."
  (interactive)
  (org-create-dblock
   (list :name "db/org-workload-overview-report"
         :end-date (org-read-date nil nil nil "End date: ")))
  (org-update-dblock))

(org-dynamic-block-define "db/org-workload-overview-report"
                          #'db/org-insert-workload-overview-report)

(defun db/spent-work-hours-on-date (date match)
  "Return the work time spent on DATE.

DATE must be given in a format understood by
`format-time-string'.  The return value is the number of minutes
clocked in on items on DATE from the files in `org-agenda-files'
that satisfy MATCH.  MATCH is a Org properties match expression."
  ;; Code inspired by `org-dblock-write:clocktable'.
  (->> org-agenda-files
       (-map #'(lambda (file)
                 (with-current-buffer (find-buffer-visiting file)
                   (save-mark-and-excursion
                     (save-restriction
                       (org-clock-get-table-data file
                                                 (list :match match
                                                       :tstart (format-time-string "%Y-%m-%d 00:00" date)
                                                       :tend (format-time-string "%Y-%m-%d 00:00" (+ 86400.0 (float-time date))))))))))
       (-map #'cl-second)
       -sum))

(defun db/remaining-work-hours-for-date (date work-items-match default-work-hours)
  "Return time left for work on DATE.

DATE must be given in a format understood by
`format-time-string'.  The return value is an Org duration
string.

The available work time is as given by DEFAULT-WORK-HOURS as Org
duration string.  The time already spent on work is computed from
the clock time on DATE (from midnight to midnight) of all items
matched by WORK-ITEMS-MATCH.  This time already spent is
subtracted from the available time to yield the final result.  If
the result is negative, \"0:00\" is returned.

As an optimization, if DATE is in the future at least one day
from now, the time spent on work is assumed to be zero."
  (let ((allotted-time (if (org-duration-p default-work-hours)
                           default-work-hours
                         (user-error "Invalid value for `default-work-hours' given: %s"
                                     default-work-hours)))
        ;; If is date is in the future, don't bother computing spent time.
        (worked-today (if (>= (float-time date)
                              (+ 86400.0 (float-time (current-time))))
                          0
                        (db/spent-work-hours-on-date date work-items-match))))
    (org-duration-from-minutes (max (- (org-duration-to-minutes allotted-time)
                                       worked-today)
                                    0))))

(defun db/org-home-time-for-date (date-string)
  "Return planned time for DATE-STRING to spend at home.

DATE-STRING must be a date formatted as Org time stamp.  The
returned time is given as an Org duration string."
  ;; This is a simplification, as `date-string' might be the start of the day or the end.
  (pcase-let ((`(_ _ _ ,day ,month ,year ,dow _ _) (parse-time-string date-string)))
    (cond
     ((let ((calendar-holidays (append holiday-general-holidays
                                       holiday-christian-holidays)))
        (calendar-check-holidays (list month day year)))
      "8:00")
     ((= dow 6) "8:00")                 ; Saturday
     ((= dow 0) "6:00")                 ; Sunday, let's do not plan too much here
     ((= dow 4) "3:00")                 ; Wednesday
     (t "2:00"))))


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
  "Clock into the last task from the clocking history.

Skip the default task if it's at the top of the clocking history,
and get the next one.  If ARG is given, forces clocking in of the
default task, though."
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
  "
Current Task: %s(replace-regexp-in-string \"%\" \"%%\" (or org-clock-current-task \"\"));
- Clock in to [_w_]ork, [_h_]ome, [_b_]reak default task
- Clock in to [_l_]ast, or [_s_]elect task to clock in to
- [_j_]ump to current clock
- Clock [_o_]ut
"
  ("w" (db/org-clock-in-work-task) nil)
  ("h" (db/org-clock-in-home-task) nil)
  ("b" (db/org-clock-in-break-task) nil)
  ("s" (org-clock-in '(4)) nil)
  ("j" (db/org-clock-goto-first-open-checkbox) nil)
  ("o" org-clock-out nil)
  ("l" db/org-clock-in-last-task nil))


;;; Babel

(defun db/org-eval-subtree-no-confirm (&optional arg)
  "Evaluate subtree at point without asking for confirmation.

Use with care!

With given ARG, force reevaluation as described for
`org-babel-execute-src-block'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org buffer, aborting"))

  ;; Since Org 9.6, we expand all folding before evaluating the current subtree,
  ;; because `org-string-width' (called by `org-table-align') sometimes computes
  ;; the wrong cell width in tables if those contain links.  Is this a bug in
  ;; Org mode?  Maybe the input to `org-string-width' is not correct?
  (org-fold-core-save-visibility :use-markers
    (org-fold-show-all)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-execute-subtree arg))))


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
        (org-align-tags)

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

(defun db/org-ignore-insert-on-headline-start (keys)
  "Return an error function when point is at the start of a headline.

Point is at the start of a headline when it is at or before the first
space that separates the stars from the title of the headline.

This function is meant to be added to `org-speed-command-hook'.  KEYS is
ignored.

The implementat has been adapted from `org-speed-command-activate' to
ignore all keys pressed at the beginning of a headline."
  (ignore keys)
  (when (or (looking-at "^\\*")         ; bol with * as first entry
            (and (or (looking-at "\\*") ; at the start of a headline, but not at bol
                     (looking-at " "))
                 (looking-back "^\\*+ ?" nil)))
    #'(lambda ()
        (user-error "No direct input allowed in headline"))))

(defun org-password-manager-get-password-by-id (id &optional return-as-value)
  "Retrieve password from Org item identified by ID.

The password is assumed to be stored at the PASSWORD property.
When RETURN-AS-VALUE is nil, the password is copied to the
clipboard as with `org-password-manager-get-password', which see.
Otherwise, the password is returned as value from this function
and can be used for further processing."

  (eval-when-compile
    (require 'org-password-manager))
  (let ((pom (org-id-find id 'marker)))
    (unless (markerp pom)
      (user-error "Cannot find item with id %s" id))

    (let ((heading (org-entry-get pom "ITEM"))
          (pw (org-entry-get pom "PASSWORD")))
      (when (null pw)
        (user-error "PASSWORD property not set for “%s”" heading))

      (if return-as-value
          pw
        (funcall interprogram-cut-function pw)
        (run-at-time org-password-manager-default-password-wait-time
                     nil
                     (lambda () (funcall interprogram-cut-function "")))
        (message "Password for “%s” securly copied to system clipboard; will be overwritten in %s."
                 heading
                 org-password-manager-default-password-wait-time)))))

(defhydra hydra-org-jump (:color blue)
  ;; Quote %, as otherwise they would be misinterpreted as format characters
  "
Current Task: %s(replace-regexp-in-string \"%\" \"%%\" (or org-clock-current-task \"\"));
- Jump to [_c_]urrent clock
- Jump to [_a_]ny item
- Jump to item [_s_]elected from clock history
"
  ("c" (db/org-clock-goto-first-open-checkbox nil)
       nil)
  ("a" (consult-org-heading nil 'agenda)
       nil)
  ("s" (db/org-clock-goto-first-open-checkbox t)
       nil))

(defhydra hydra-org-custom (:foreign-keys warn
                            :exit t)
  "
Custom Org commands:
_c_ → Clocking commands
_j_ → Jumping commands
_l_ → Linking commands
_a_ → Open agenda
_q_ → Quit this hydra"
  ("c" hydra-org-clock/body nil)
  ("j" hydra-org-jump/body nil)
  ("l" hydra-org-linking/body nil)
  ("a" db/org-agenda nil)
  ("q" (message "Abort") nil)
  ("C-g" (message "Abort") nil))

(defun db/ledger-cli-to-org-table-list (command)
  "Run ledger COMMAND and convert the result to Org table list.

COMMAND is expected to be a call to ledger-cli and its `lisp'
subcommand, the result of which is a list of transactions.  This
list is converted in such a way that Org can display it as table,
where the rows are all accounts contained in the result of
COMMAND, and where the columns are the individual transactions,
headlined with their date."

  (let* (ledger-output
         ledger-output-parsed
         transactions
         dates
         accounts
         result)

    (condition-case err
        (setq ledger-output (shell-command-to-string command))
      ((error debug) (error "Failed to run ledger command: %s" err)))

    (setq ledger-output-parsed (read-from-string ledger-output))

    ;; Ensure we have parsed the complete output, i.e., `ledger-output' is a lisp form.
    (unless (= (1- (length ledger-output))
               (cdr ledger-output-parsed))
      (error "Failed to parse ledger output\nRead: %s\nGot: %s" ledger-output-parsed ledger-output))

    ;; Format time and remove unnecessary transactions
    (setq transactions (-map #'(lambda (row)
                                 (list (format-time-string "%F" (nth 2 row))
                                       (-map #'(lambda (entry)
                                                 (list (nth 1 entry)
                                                       (nth 2 entry)))
                                             (-drop 5 row))))
                             (car ledger-output-parsed)))

    (setq dates (-map #'-first-item transactions))

    ;; Transfer list output into alist for later direct access; `accounts' will map accounts to
    ;; alists mapping dates to values
    ;; FIXME: this implementation is certainly not ideal
    (dolist (row transactions)
      (let ((date (-first-item row)))
        (dolist (entry (-second-item row))
          (let ((fund (-first-item entry))
                (value (-second-item entry)))
            (setf (alist-get date (alist-get fund accounts nil nil #'string=) nil nil #'string=)
                  value)))))

    ;; Build up final result (in reverse order)
    (push (cl-list* "" dates) result)
    (push (cl-list* " " (-repeat (length transactions) "<r>")) result)
    (push 'hline result)

    (dolist (fund (-sort #'string< (-map #'-first-item accounts)))
      (push (cl-list* fund
                      (-map #'(lambda (date)
                                (alist-get date (alist-get fund accounts nil nil #'string=)
                                           "0.00€" nil #'string=))
                            dates))
            result))

    (reverse result)))

(defun db/org-execute-babel-in-buffer-and-iterate-tables ()
  "Update all babel source blocks in current buffer and iterate tables afterwards.

This is useful for updating complex reports that rely on a mix of
Org Babel source blocks and dependent tables."
  (interactive)
  (org-babel-execute-buffer)
  (org-table-iterate-buffer-tables))


;;; Checklist Handling

(defun db/org--find-template ()
  "Return marker to template item associated with item at point.

Return nil if no template is associated with item at point.

See `db/org-insert-checklist' for how this template item is
determined."

  (let (template-marker)

    ;; Check for TEMPLATE_ID property
    (when-let ((template-id (org-entry-get (point) "TEMPLATE_ID" :inherit)))
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

Checklists are inserted before the first child, if existent, or at the
end of the subtree.

After inserting a checklist, add the property CHECKLIST_INSERTED_P with
value t to item at point.  Checklists are not inserted if this property
with this value is already present, to avoid double insertions of
checklists.

The checklist consists of a listing of concurrent date entries, relevant
backlinks of the current item and its parents (without archives) as well
as a template.

Concurrent date entries are all Org items tagged with DATE and posessing
an active time range that encloses today.

Relevant backlinks are Org items and are determined as follows:

- for an Org item to be considered as backlink item, it must reference
  the item at point directly, or one of its parents, via an Org link
  using the id: link type (also see the `db/org-backlinks' dynamic
  block);

- the backlink item must not be done, must not be tagged locally with
  TEMPLATE and must not be tagged with HOLD nor SOMEWHEN (neither
  locally nor inherited);

- the backlink item must not be scheduled in the future;

- the backlink item must be contained in a file in the variables
  `org-agenda-files' or `org-agenda-text-search-extra-files', but not in
  an archive file (i.e., archives are excluded from the search);

- the backlink item must not have the CHECKLIST_NO_BACKLINK property set
  to nil (with inheritance not being considered, i.e., the property must
  be set directly at the item to exclude it as backlink).

The depth to which backlinks to parents are considered can be configured
via the CHECKLIST_BACKLINK_DEPTH property at the item at point.  This
property is looked up only at the current item, i.e., again no
inheritance is considered.  If this property is not set, the depth to
which backlinks to parents is considered is unlimited by default (i.e.,
nil).

After the table of backlinks, a template is inserted.  This templates is
usually a checklist copied from another Org item tagged with :TEMPLATE:.
The item to copy the template from is determined by the TEMPLATE_ID
property, which must be an ID referencing the proper template item.  If
that property is not set (either directly or via inheritance), search
for the topmost sibling of the current item is conducted to see whether
its headline is matching \"^Template.*\"; if so, its body is used as
template.

When ARG is given, jump to the current template instead of inserting the
checklist."
  (interactive "P")

  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-goto))

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

         (save-mark-and-excursion

           ;; Checklists are inserted directly before first child, if existent, or
           ;; at end of subtree
           (org-fold-show-entry)
           (or (org-goto-first-child)
               (org-end-of-subtree 'invisible-ok 'to-heading))
           ;; Move back from heading, unless we are at the end of the buffer
           (when (org-at-heading-p)
             ;; Go to end of line before heading
             (forward-char -1))

           ;; Insert blank line, but only if the previous line is not blank already.
           (unless (save-mark-and-excursion
                     (forward-line -1)
                     (looking-at (rx bol (* space) eol)))
             (insert "\n"))

           ;; Insert actual checklist consisting of concurrent dates, relevant backlinks, and a template
           (let (concurrent-dates
                 number-of-backlinks)

             ;; Insert links to concurrent DATEs, if any

             (setq concurrent-dates (org-ql-query :from (org-agenda-files)
                                                  :select '(cons
                                                            (org-entry-get (point) "ITEM")
                                                            (org-id-get-create))
                                                  :where `(and ; XXX: this is not quite right yet
                                                           (tags "DATE")
                                                           (not (done))
                                                           ;; XXX: calling `ts-now' twice might be stupid
                                                           (ts-active :from ,(ts-now))
                                                           (ts-active :to ,(ts-now)))))

             (when concurrent-dates
               (insert "Concurrent DATEs:\n")
               (dolist (date concurrent-dates)
                 (insert "- " (org-link-make-string (format "id:%s" (cdr date)) (car date)) "\n"))
               (insert "\n"))

             ;; Insert relevant backlinks

             (let ((point-before-backlinks (point))
                   (parent-depth (--when-let (org-entry-get (point) "CHECKLIST_BACKLINK_DEPTH" nil)
                                   (string-to-number it))))

               (insert (format "Relevant backlinks (%s):\n\n"
                               (if parent-depth
                                   (format "parent-depth %d" parent-depth)
                                 "all parents")))

               (setq number-of-backlinks
                     (org-dblock-write:db/org-backlinks (list
                                                         :org-ql-match '(and
                                                                         (not (done))
                                                                         (not (ltags "TEMPLATE"))
                                                                         (not (tags "HOLD"))
                                                                         (not (tags "SOMEWHEN"))
                                                                         (not (scheduled :from 1))
                                                                         (not (property "CHECKLIST_NO_BACKLINK" "t" :inherit nil)))
                                                         :parent-depth (--when-let (org-entry-get (point) "CHECKLIST_BACKLINK_DEPTH" nil)
                                                                         (string-to-number it))
                                                         :archive nil)))

               ;; When no backlinks have been found, remove everything inserted so far.
               (if (zerop number-of-backlinks)
                   (delete-region point-before-backlinks (point))
                 (insert "\n\n")))

             ;; And finally insert a template

             (when-let ((template-marker (db/org--find-template)))
               (let (point-before-template
                     point-after-template)
                 (setq point-before-template (point))
                 (unless (and (zerop number-of-backlinks)
                              (null concurrent-dates))
                   ;; When there are no backlinks, there is no need to print “Template” as a
                   ;; separator
                   (insert "Template:"))
                 (db/org-copy-body-from-item-to-point template-marker)
                 (setq point-after-template (point))

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
                                    'region))))))

         (org-entry-put (point) "CHECKLIST_INSERTED_P" "t")
         (org-update-statistics-cookies nil)

         (db/org-goto-first-open-checkbox-in-headline))))

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

(defun db/org-goto-first-open-checkbox-in-headline (&optional silent)
  "Move point to first started checkbox in the current headline.

A started checkbox item is of the form “[-]”.  The idea to move
point there is to continue previously started work on the current
Org headline.

If no started checkbox is found, move point to the first
unstartetd checkbox “[ ]”.  The idea is that this is where work
should continue if no partially completed item is present.

If there's neither a started or an open checkbox, emit a
message (unless SILENT is non-nil) and stay put.

If a checkbox is found this way and the checkbox has non-empty
contents, search for started or open checkboxes is continued in
this contents recursively, in the same way as described above.

The search for started or open checkboxes is not done in branches
of the current headline, i.e., the search is stopped when the
first sub-headline is found."

  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org buffer, exiting"))

  (save-restriction
    (widen)
    (org-back-to-heading 'invisible-ok)
    (org-narrow-to-subtree)

    (let ((ast (-> (org-element-parse-buffer)
                   (org-element-contents)
                   (-first-item)        ; we skip the first headline
                   (org-element-contents)))
          checkbox-node                 ; node found in the current run
          checkbox-node-1               ; last node found, if any
          )

      (while ast

        (org-element-map (org-element-contents ast) '(item)
          (lambda (node)
            (when (and (null checkbox-node)
                       (not (memq (org-element-property :checkbox node)
                                  '(nil on))))
              (setq checkbox-node node))
            ;; Having this `when' separately and at the end of this function results in
            ;; `org-element-map' terminating the search as soon as a “[-]” is found.  If no such
            ;; checkbox is ever found, we stick with the first open checkbox found in the previous
            ;; `when'.
            (when (eq 'trans (org-element-property :checkbox node))
              (setq checkbox-node node)))
          ;; Additional arguments to `org-element-map': stop at first non-nil result returned by
          ;; FUN, and do not recurse into headlines.
          nil t '(headline))

        (setq checkbox-node-1 (or checkbox-node checkbox-node-1)
              ast checkbox-node
              checkbox-node nil))

      (if checkbox-node-1
          (goto-char (or (org-element-contents-begin checkbox-node-1)
                         (org-element-begin checkbox-node-1)))
        (unless silent
          (message "No open checkbox in subtree"))))))

(defun db/org-clock-goto-first-open-checkbox (&optional select)
  "Go to the currently clocked-in item or most recently clocked item.

Move point to first open checkbox there, if there's one.  See
`db/org-goto-first-open-checkbox-in-headline' for details.

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
	     ((org-clocking-p) org-clock-hd-marker)
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
          (org-fold-hide-drawer-all)
          (db/org-goto-first-open-checkbox-in-headline :silent)
          (org-fold-show-set-visibility 'ancestors)
          (if recent
	      (message "No running clock, this is the most recently clocked task"))
          (run-hooks 'org-clock-goto-hook))
      (user-error "Cannot display target buffer"))))


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

(defun db/org-get-location (&optional use-all-org-files initial-input prompt)
  "Interactively query for location and return mark.

Use INITIAL-INPUT as initial input when filtering available
locations.  Use PROMPT as prompt when given.

When USE-ALL-ORG-FILES is nil, this functions by default searches
through the current buffer if that one is an Org buffer and is
associated with a file, and `db/org-default-org-file' otherwise.
However, if the current buffer is associated with a file from the list
returned by the function `org-agenda-files', the search is extended
through all agenda files (the rationale being that Org agenda files are
always considered to be one large data collection).

When USE-ALL-ORG-FILES is non-nil, search through all files in
the variables `org-agenda-files',
`org-agenda-text-search-extra-files', and the current file or
`db/org-default-org-file' as described above."
  (let ((default-buffer (if (and (buffer-file-name) (derived-mode-p 'org-mode))
                            (current-buffer)
                          (find-file-noselect db/org-default-org-file))))

    (when (null default-buffer)
      (user-error "Current buffer is not associated with a file and `db/org-default-org-file' does not exist; nothing to search through"))

    (let* ((current-buffer-is-in-org-agenda-files? (--when-let (buffer-file-name)
                                                     (-any (-partial #'file-equal-p it)
                                                           org-agenda-files)))

           (scope (cond (use-all-org-files
                         (->> (append (unless current-buffer-is-in-org-agenda-files? ; avoid duplicate entries
                                        (list (buffer-file-name default-buffer)))
                                      (org-agenda-files)
                                      (cl-remove-if-not #'stringp
                                                        org-agenda-text-search-extra-files))
                              (-map #'file-truename)
                              (-uniq)))
                        (current-buffer-is-in-org-agenda-files?
                         (org-agenda-files))
                        (t
                         (list (buffer-file-name default-buffer)))))

           (pom (with-current-buffer default-buffer
                  (consult--read (consult--slow-operation "Collecting headings..."
                                   (or (consult-org--headings t nil scope)
                                       (user-error "No headings")))
                                 :prompt (or prompt "Choose heading: ")
                                 :category 'org-heading
                                 :sort nil
                                 :initial initial-input
                                 :require-match t
                                 :history '(:input consult-org--history)
                                 :narrow (consult-org--narrow)
                                 :annotate #'consult-org--annotate
                                 :group #'consult-org--group
                                 :lookup (apply-partially #'consult--lookup-prop 'org-marker)
                                 :preview-key nil))))
      (if (markerp pom)
          pom
        (user-error "Invalid location")))))

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

(defun db/org-add-link-to-other-item (&optional use-all-org-files initial-input)
  "Interactively query for item and add link to it at point.

Search through all items of the current buffer, or
`db/org-default-org-file' if the current buffer is not associated
with a file.  If USE-ALL-ORG-FILES is non-nil, include all files
in the variables `org-agenda-files' and
`org-agenda-text-search-extra-files' in this search.

Use INITIAL-INPUT as initial string to narrow down all available
items during interactive selection."
  (interactive "P")
  (db/org-insert-link-to-pom (db/org-get-location use-all-org-files initial-input)))

(defun db/org-add-link-to-current-clock ()
  "Insert link to currently clocked-in item at point.
Error out when the clock is not active."
  (interactive)
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
 … _o_ther item (from all Org mode text search files)

Show _b_acklinks to current item."
  ("c" db/org-add-link-to-current-clock)
  ("s" db/org-add-link-to-org-clock-select-task)
  ("o" (db/org-add-link-to-other-item t))
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
    ;; [2022-06-09]) when descriptions contain brackets
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
               ;; (backlink-id . headline-id)
               (-mapcat (pcase-lambda (`(,headline . ,backlinks))
                          (mapcar #'(lambda (backlink)
                                      (cons backlink headline))
                                  backlinks)))
               ;; Group by backlinks (first entry), returns alist of
               ;; backlink-ids and list of pairs (backlink-id . headline-id)
               (-group-by #'car)
               ;; Flatten list, to get a list of (backlink-id headline-ids...);
               ;; remove dupliate headline-id entries
               (-map (pcase-lambda (`(,backlink . ,backlink-headline-conses))
                         (cons backlink (-distinct (-map #'cdr backlink-headline-conses)))))))

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


;;; Adding links to bookmarks

;; The code in this section is based version 1.0 of `ol-bookmark.el' by Tokuya Kameshima;
;; see https://github.com/emacsmirror/org-contrib/blob/bd39cca48b1c4a8a1cdfb1cdd6be2ce700acdd97/lisp/ol-bookmark.el.

(defun db/org-bookmark-open (bookmark _)
  "Visit the bookmark BOOKMARK."
  (bookmark-maybe-historicize-string bookmark)
  (bookmark-maybe-load-default-file)
  (bookmark-jump bookmark))

(defun db/org-bookmark-store-link ()
  "Store a link to the bookmark at point."
  (let (bookmark)
    (when (eq major-mode 'bookmark-bmenu-mode)
      (setq bookmark (bookmark-bmenu-bookmark))
      (org-link-store-props :link (concat "bookmark:" bookmark)
			    :description bookmark))))

(defun db/org-bookmark-export (path description backend)
  "Export Org bookmark links by resolving them to their target path.

PATH denotes the bookmark name, while DESCRIPTION is the (optional)
description of that link.  BACKEND denotes the target format for export."
  (condition-case err
      (let* ((bmk-target (or (bookmark-prop-get path 'location)
                             (bookmark-prop-get path 'filename)
                             (user-error "Cannot resolve bookmark for export: %s"
                                         path))))
        ;; TODO: the following might be quite heavy, as it invokes the Org parser again.  Maybe
        ;; exporting links can be done more easily?
        (org-export-string-as (org-link-make-string bmk-target description)
                              backend))
    (error (error "Error of type “%s” while exporting bookmark “%s”: “%s”"
                  (car err) path (cadr err)))))

(defun db/org-lint-invalid-bookmark-link (ast)
  "Org lint checker to verify bookmark links in AST point to known bookmarks."
  (bookmark-maybe-load-default-file)
  (org-element-map ast 'link
    (lambda (link)
      (let ((bmk (org-element-property :path link)))
	(and (equal (org-element-property :type link) "bookmark")
             (not (assoc-string bmk bookmark-alist))
	     (list (org-element-begin link)
		   (format "Unknown bookmark link \"%s\"" bmk)))))))

(defun db/org-lint-possible-bookmark-link (ast)
  "Org lint checker to point out links in AST that could be replaced by bookmarks."
  (bookmark-maybe-load-default-file)
  (org-element-map ast 'link
    (lambda (link)
      (when-let* ((link-path (org-element-property :raw-link link))
                  (known-bookmark (-find #'(lambda (bmk)
                                             (string= (bookmark-get-filename bmk)
                                                      link-path))
                                         bookmark-alist)))
        (list (org-element-begin link)
	      (format "Link to \"%s\" can be replaced by bookmark \"%s\""
                      link-path
                      (car known-bookmark)))))))


;;; End

(provide 'db-org)

;;; db-org.el ends here
