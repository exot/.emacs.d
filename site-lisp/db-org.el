;;; org.el -- Daniel's org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file defines functions used in the main configuration of org-mode and
;; it’s subpackages.  Nothing here changes the behavior of org-mode per se, as
;; loading this file only defines a couple of functions.

;;; Code:

(require 'db-customize)


;;; Agenda Customization

(defun db/check-special-org-files-in-agenda (&rest args)
  "Check whether the special org-mode files are part of `org-agenda-files', ignoring ARGS.
The special org-mode files are `db/org-default-work-file',
`db/org-default-home-file', `db/org-default-notes-files', and
`db/org-default-refile-file'."
  (ignore args)
  (require 'org)
  (let ((agenda-files (mapcar #'file-truename (org-agenda-files t))))
    (dolist (file '(db/org-default-home-file
                    db/org-default-work-file
                    db/org-default-notes-file
                    db/org-default-refile-file))
      (when (and (symbol-value file)
                 (not (member (file-truename (symbol-value file))
                              agenda-files)))
        (warn "File %s is not part of `org-agenda-files'."
              file)))))

(defun db/org-agenda-list-deadlines (&optional match)
  ;; XXX org-agenda-later does not work, fix this
  "Prepare agenda view that only lists upcoming deadlines.

Ignores MATCH."
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
         (has-tag          (member tag (org-get-tags-at current-headline))))
    (if (or (and others (not has-tag))
            (and (not others) has-tag))
        next-headline
      nil)))

(defun db/cmp-date-property (prop)
  ;; https://emacs.stackexchange.com/questions/26351/custom-sorting-for-agenda
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.

If a is before b, return -1. If a is after b, return 1. If they
are equal return nil."
  (lexical-let ((prop prop))
    #'(lambda (a b)
        (let* ((a-pos (get-text-property 0 'org-marker a))
               (b-pos (get-text-property 0 'org-marker b))
               (a-date (or (org-entry-get a-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (b-date (or (org-entry-get b-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (cmp (compare-strings a-date nil nil b-date nil nil)))
          (if (eq cmp t) nil (cl-signum cmp))))))

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

;; Exclude DONE state tasks from refile targets (from bh)
(defun db/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components))
               org-done-keywords)))


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
         (let ((tags (nth 5 (org-heading-components))))
           (unless (and tags (member "NOP" (split-string tags ":" t)))
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

(defun db/org-clock-current-task ()
  "Return currently clocked in task."
  (require 'org-clock)
  org-clock-current-task)

(defhydra hydra-org-clock (:color blue)
  "
Current Task: %s(db/org-clock-current-task); "
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


;;; End

(provide 'db-org)

;;; db-org.el ends here
