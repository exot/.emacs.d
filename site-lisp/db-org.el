;;; org.el -- Daniel's org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file defines functions used in the main configuration of org-mode and
;; it’s subpackages.  Nothing here changes the behavior of org-mode per se, as
;; loading this file only defines a couple of functions.

;;; Code:

(require 'subr-x)
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

;; Show sum of daily efforts in agenda, the following two functions are from
;; anpandey,
;; cf. https://emacs.stackexchange.com/questions/21380/show-sum-of-efforts-for-a-day-in-org-agenda-day-title#21902

(defun db/org-agenda-calculate-efforts (limit)
  "Sum efforts of day entries up to LIMIT in the agenda buffer.
Entries included are those scheduled for that day, scheduled at
some past day (and still on display) and active timestamps (appointments)."
  (let (total)
    (save-excursion
      (while (< (point) limit)
        (when (member (org-get-at-bol 'type)
                      '("scheduled" "past-scheduled" "timestamp"))
          (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
        (forward-line)))
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


;;; Reset checklists

;; from `org-checklist’ by James TD Smith (@ ahktenzero (. mohorovi cc)),
;; version: 1.0
(defun org-reset-checkbox-state-maybe ()
  "Reset all checkboxes in an entry if `RESET_CHECK_BOXES' property is set."
  (interactive "*")
  (when (org-entry-get (point) "RESET_CHECK_BOXES")
    (warn "Using the RESET_CHECK_BOXES property is deprecated, user periodic tasks instead")
    (org-reset-checkbox-state-subtree)))


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

(defun db/org-clear-stored-links ()
  "Clear list of stored links by setting `org-stored-links' to NIL.
This might be handy when links are kept by setting
`org-link-keep-stored-after-insertion' to T, but too many links
have accumulated over time."
  (interactive)
  (setq org-stored-links nil))


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

(defun db/org-mark-current-default-task ()
  "Mark current task as default when equal to work task or home task.
Work task and home task are determined by the current values of
`org-working-task-id’ and `org-home-task-id’, respectively."
  (let ((current-id (org-id-get org-clock-marker)))
    (when (member current-id (list org-working-task-id
                                   org-home-task-id))
      (org-clock-mark-default-task))))

(defun db/org-copy-template-for-periodic-task ()
  "Copy body of the enclosing periodic task to item at point.
The body must be placed into an item titled 'Template',
called the body item.  The body item must be the first
headline of the periodic task, i.e., of the parent of the current
item at point.  The body of the body item, without any
drawers, will be copied to point."
  (interactive)
  (let ((template-pom (save-restriction
                        (save-mark-and-excursion
                          ;; Navigate to the body, which is supposed to be
                          ;; the first item of the periodic task.  One could
                          ;; think about putting the body also directly
                          ;; below the periodic task, but this is not supported
                          ;; yet.
                          (outline-up-heading 1 'invisible-ok)
                          (outline-next-heading)
                          (point)))))

    (unless (string-equal (org-element-property
                           :title
                           (org-with-point-at template-pom
                             (org-element-at-point)))
                          "Template")
      (user-error "Template must be first headline in periodic task"))

    (db/org-copy-body-from-item-to-point template-pom)))

(defun db/org-copy-template-from-id ()
  "Copy template given by current value of TEMPLATE_ID property to point.
The TEMPLATE_ID property must be an ID property of another item
from which the contents is supposed to be copied to point."
  ;; This function might be obsoleted by `db/org-copy-template'.
  (interactive)
  (let ((template-id (org-entry-get (point) "TEMPLATE_ID"))
        template-pom)
    (unless template-id
      (user-error "Property TEMPLATE_ID not set, cannot copy from there"))
    (setq template-pom (org-id-find template-id :get-marker))
    (unless template-pom
      (user-error "Cannot find item with id %s" template-id))
    (db/org-copy-body-from-item-to-point template-pom)))

(defun db/org-copy-template ()
  "Copy template for the current Org Mode item to point.
The template is determined by the TEMPLATE_ID property, which
must be an ID referencing the proper template item.  If that
property is not set, search for the topmost sibling of the
current item and see whether its headline is matching
\"^Template.*\"; if so, use its body as template, and barf
otherwise."
  (interactive)

  (let (template-pom)

    ;; Check for TEMPLATE_ID property
    (when-let ((template-id (org-entry-get (point) "TEMPLATE_ID")))
      (setq template-pom (org-id-find template-id :get-marker))
      (unless template-pom
        (warn "TEMPLATE_ID is set, but could not be resolved: %s"
              template-id)))

    ;; If no template has been found so far, search for top-most sibling and
    ;; whether its headline starts with “Template”; use that when found.
    (unless template-pom
      (let ((top-most-sibling (condition-case _
                                  (save-restriction
                                    (save-mark-and-excursion
                                      (outline-up-heading 1 'invisible-ok)
                                      (outline-next-heading)
                                      (point)))
                                (t nil))))
        (when (and top-most-sibling
                   (integerp top-most-sibling) ; just to make sure we have a
                                               ; point here
                   (string-match-p "^Template.*"
                                   (org-entry-get top-most-sibling "ITEM")))
          (setq template-pom top-most-sibling))))

    (unless template-pom
      (user-error "Cannot find template via TEMPLATE_ID property or top-most sibling"))

    (db/org-copy-body-from-item-to-point template-pom)))

(defun db/org-copy-body-from-item-to-point (pom)
  "Copy body from item given by POM to point.
This can be used to copy checklists from templates to the current
item, which might be an instance of a periodic task.  If POM is
not given, use `db/org-get-location' to interactively query for
it.  Adds newline before and after the template."
  (interactive (list (db/org-get-location t)))
  (unless (number-or-marker-p pom)
    (user-error "Argument is neither point nor mark: %s" pom))
  (let ((body (save-restriction
                (save-mark-and-excursion
                  (let ((template-element (org-with-point-at pom
                                            (org-element-at-point))))
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
    (insert "\n")
    (insert body)
    (insert "\n")
    (org-update-statistics-cookies nil)))

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

        ;; Store note manually (I tried using `org-add-log-note', but did not succeed …)
        (goto-char (org-log-beginning 'create))
        (indent-according-to-mode)
        (insert "- Note taken on ")
        (org-insert-time-stamp (current-time) t t)
        (insert " \\\\\n")
        (indent-according-to-mode)
        (insert (format "  Changed headline from: %s\n" old-headline)))))

  (when org-refile-use-cache
    (org-refile-cache-clear))

  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-redo)))


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

Searches through the current buffer if that one is an Org buffer
and is associated with a file, or `db/org-default-org-file'.
When ARG is non-nil, search through all files in the variables
`org-agenda-files', `org-agenda-text-search-extra-files', and the
current file or `db/org-default-org-file'.

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
    (let* ((org-refile-targets (append (and arg
                                            `((org-agenda-files :maxlevel . 9)
                                              (,(cl-remove-if-not #'stringp
                                                                  org-agenda-text-search-extra-files)
                                               :maxlevel . 9)))
                                       '((nil :maxlevel . 9))))
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

    ;; When item is a link, only use it's description when available; otherwise
    ;; use the link part
    (save-match-data
      (while (string-match org-link-bracket-re item)
        (let ((desc (or (match-string-no-properties 2 item)
                        (match-string-no-properties 1 item))))
          (setq item (concat (substring item 0 (match-beginning 0))
                             desc
                             (substring item (match-end 0)))))))

    (org-link-make-string (format "id:%s" id) item)))

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

The search is conducted over all files returned by
`org-agenda-files' including archives, as well as all files
referenced in `org-agenda-text-search-extra-files'."

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
      (user-error ":parent-depth is not an integer"))

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
    (org-table-align)))

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
