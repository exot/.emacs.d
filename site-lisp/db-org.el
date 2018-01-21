;;; org.el -- Daniel's org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;;; Basic Setup

(require 'org)

(setq org-deadline-warning-days 14
      org-read-date-popup-calendar t
      org-insert-heading-respect-content t
      org-list-description-max-indent 5
      org-adapt-indentation nil
      org-edit-timestamp-down-means-later t
      org-archive-location "%s_archive.gpg::"
      org-image-actual-width nil
      org-footnote-section nil
      org-log-into-drawer "LOGBOOK"
      org-log-reschedule 'time
      org-clone-delete-id t
      org-catch-invisible-edits 'error
      org-M-RET-may-split-line '((default . nil))
      org-highlight-latex-and-related '(latex))

(bind-key [remap org-return] 'org-return-indent org-mode-map)

(setq org-todo-keywords
      '((sequence "TODO(t)" "CONT(n!)" "|" "DONE(d@)")
        (sequence "GOTO(g)" "ATTN(a)" "|" "DONE(d@)")
        (sequence "READ(r)" "CONT(n!)" "|" "DONE(d@)")
        (sequence "DELG(e@/!)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANC(c@/!)" "PHONE" "MEETING")))

(setq org-todo-state-tags-triggers
      '(("WAIT" ("WAIT" . t))
        ("HOLD" ("HOLD" . t))
        (done ("HOLD") ("WAIT") ("DATE") ("NO_EXPORT" . t))
        ("TODO" ("HOLD") ("WAIT") ("NO_EXPORT"))
        ("READ" ("READ" . t) ("HOLD") ("WAIT"))
        ("GOTO" ("DATE" . t) ("HOLD") ("WAIT"))
        ("CONT" ("HOLD") ("WAIT"))
        ("ATTN" ("HOLD") ("WAIT"))))

(setq org-tag-alist
      '((:startgroup . nil)
        ("WORK" . ?w)
        ("HOME" . ?h)
        ("FUN" . ?f)
        ("UNTAGGED" . ?u)
        (:endgroup . nil)
        ("NOTE" . ?n)))

(setq org-treat-S-cursor-todo-selection-as-state-change nil
      org-fast-tag-selection-single-key 'expert)

(setq org-global-properties
      '(("Effort_ALL" . "0:00 0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00")))

(setq org-columns-default-format
      "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

;; open directory links in emacs itself
(add-to-list 'org-file-apps '(directory . emacs))

(add-to-list 'org-file-apps '("\\.docx\\'" . default))
(add-to-list 'org-file-apps '("\\.pptx\\'" . default))
(add-to-list 'org-file-apps '("\\.xlsx\\'" . default))


;;; Faces

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight normal)
        ("GOTO" :foreground "red" :weight normal)
        ("READ" :foreground "red" :weight normal)
        ("CONT" :foreground "DeepSkyBlue" :weight normal)
        ("ATTN" :foreground "DeepSkyBlue" :weight normal)
        ("DONE" :foreground "forest green" :weight normal)
        ("DELG" :foreground "dark orange" :weight normal)
        ("WAIT" :foreground "orange" :weight normal)
        ("HOLD" :foreground "magenta" :weight normal)
        ("CANC" :foreground "lime green" :weight normal)
        ("MEETING" :foreground "forest green" :weight normal)
        ("PHONE" :foreground "forest green" :weight normal)
        ("REPEAT" :foreground "indian red" :weight normal)))

(setq org-fontify-done-headline nil)

(setq org-priority-faces
      '((?A . (:foreground "Red" :weight bold))
        (?B . (:foreground "firebrick"))
        (?C . (:foreground "tomato"))))

(org-link-set-parameters
 "file"
 :face (lambda (path) (if (file-exists-p path) 'org-link 'org-warning)))


;;; Clocking

(setq org-clock-history-length 23
      org-clock-in-resume t
      org-clock-into-drawer t
      org-clock-idle-time nil
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done '("DONE" "CANC" "WAIT" "HOLD")
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-mode-line-total 'auto
      org-clock-report-include-clocking-task t
      org-clock-in-switch-to-state (lambda (_)
                                     (when (and (not
                                                 (and (boundp 'org-capture-mode)
                                                      org-capture-mode)))
                                       (cond
                                        ((member (org-get-todo-state)
                                                 (list "TODO" "READ"))
                                         "CONT")
                                        ((member (org-get-todo-state)
                                                 (list "GOTO"))
                                         "ATTN")))))

(org-clock-persistence-insinuate)

(setq org-clock-persist t
      org-clock-persist-query-resume nil)

(setq org-duration-format 'h:mm
      org-time-stamp-rounding-minutes '(1 1))

;; Default Tasks for Working, Home, Breaks

(defcustom org-working-task-id ""
  "Task ID of default working task."
  :group 'personal-settings
  :type 'string)

(defcustom org-break-task-id ""
  "Task ID of default break task."
  :group 'personal-settings
  :type 'string)

(defcustom org-home-task-id ""
  "Task ID of default home task."
  :group 'personal-settings
  :type 'string)

(add-hook 'org-clock-in-hook            ; mark current default task
          (lambda ()
            (let ((current-id (org-id-get org-clock-marker)))
              (when (member current-id (list org-working-task-id
                                             org-home-task-id))
                (org-clock-mark-default-task)))))

;; Clock in default task if no other task is given

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

(add-hook 'org-clock-out-hook #'db/ensure-running-clock 'append)

;; clock-in helpers

(defun clock-in-task-by-id (task-id)
  "Clock in org mode task as given by TASK-ID."
  (org-with-point-at (org-id-find task-id 'marker)
    (org-clock-in))
  (org-save-all-org-buffers))

(defun clock-out-task-by-id (task-id)
  "Clock out org mode task as given by TASK-ID."
  (org-with-point-at (org-id-find task-id 'marker)
    (org-clock-out))
  (org-save-all-org-buffers))

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

;; Communicate the currently clocked in task to the outside world

(defvar db/org-clock-current-task-file
  "~/.org-current-task")

(defun db/org-current-task ()
  "Format currently clocked task and write it to
`db/org-clock-current-task-file'."
  (with-temp-file db/org-clock-current-task-file
    (let ((clock-buffer (marker-buffer org-clock-marker)))
      (if (null clock-buffer)
          (insert "No running clock")
        (insert org-clock-heading)))))

(add-hook 'org-clock-in-hook #'db/org-current-task)

(defun db/select-clocking-task ()
  "Select task from recent clocked-in tasks."
  (interactive)
  (org-clock-in '(4)))


;;; Agenda Customization

(defun db/update-org-agenda-files (symbol value)
  "Set SYMBOL to VALUE and update `org-agenda-files’ afterwards."
  (set-default symbol value)
  (setq org-agenda-files (cl-remove-duplicates
                          (cl-remove-if #'string-empty-p
                                        (mapcar (lambda (symbol)
                                                  (when (boundp symbol)
                                                    (symbol-value symbol)))
                                                '(db/org-default-home-file
                                                  db/org-default-work-file
                                                  db/org-default-refile-file
                                                  db/org-default-notes-file)))
                          :test #'equalp)))

(defcustom db/org-default-work-file ""
  "Path to default org-mode file at work."
  :group 'personal-settings
  :type 'string
  :set #'db/update-org-agenda-files)

(defcustom db/org-default-home-file ""
  "Path to default org-mode file at home."
  :group 'personal-settings
  :type 'string
  :set #'db/update-org-agenda-files)

(defcustom db/org-default-notes-file ""
  "Path to default org-mode file for notes."
  :group 'personal-settings
  :type 'string
  :set #'db/update-org-agenda-files)

(defcustom db/org-default-refile-file ""
  "Path to default org-mode file for capturing."
  :group 'personal-settings
  :type 'string
  :set #'db/update-org-agenda-files)

(defcustom db/org-default-pensieve-file ""
  "Path to default org-mode file for private notes."
  :group 'personal-settings
  :type 'string
  :set #'db/update-org-agenda-files)

(setq org-agenda-include-diary nil
      org-agenda-span 1
      org-agenda-diary-file db/org-default-refile-file
      org-agenda-insert-diary-strategy 'top-level
      org-catch-invisible-edits 'show
      org-agenda-sorting-strategy '((agenda time-up habit-up priority-down)
                                    (todo category-keep)
                                    (tags category-keep)
                                    (search category-keep)))

(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-compact-blocks nil)

(setq org-agenda-todo-ignore-with-date nil
      org-agenda-todo-ignore-deadlines nil
      org-agenda-todo-ignore-scheduled nil
      org-agenda-todo-ignore-timestamp nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-start-with-log-mode nil
      org-agenda-log-mode-items '(closed state)
      org-agenda-remove-tags t
      org-agenda-sticky nil
      org-agenda-inhibit-startup t
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-dim-blocked-tasks nil
      org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-dependencies          t
      org-agenda-use-time-grid t
      org-agenda-persistent-filter t
      org-agenda-search-headline-for-time nil)

(setq org-agenda-clock-consistency-checks
      '(:max-duration 9999999
        :min-duration 0
        :max-gap 0
        :gap-ok-around nil
        :default-face ((:background "DarkRed") (:foreground "white"))
        :overlap-face nil :gap-face nil :no-end-time-face nil
        :long-face nil :short-face nil))

(add-hook 'org-agenda-mode-hook #'hl-line-mode 'append)

(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 4 :compact t :narrow 60 :fileskip0 t))

(setq org-stuck-projects
      '("-REGULAR-HOLD-NOTE+TODO=\"\""
        ("CONT" "TODO" "READ" "WAIT" "GOTO" "DELG")
        ("DATE" "NOP" "HOLD")
        ""))

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

(setq org-agenda-custom-commands
      `(("A" "Main List"
             ((agenda
               ""
               ((org-agenda-entry-types '(:timestamp :sexp :scheduled :deadline))
                (org-deadline-warning-days 0)))
              (db/org-agenda-list-deadlines
               ""
               ((org-agenda-overriding-header "Deadlines")
                (org-agenda-sorting-strategy '(deadline-up priority-down))
                (org-agenda-skip-deadline-prewarning-if-scheduled t)
                (org-deadline-warning-days 30)))
              (tags-todo "-NOAGENDA/WAIT|DELG"
                         ((org-agenda-overriding-header "Waiting-fors")
                          (org-agenda-todo-ignore-deadlines t)
                          (org-agenda-todo-ignore-scheduled t)))
              (tags "REFILE"
                    ((org-agenda-files (list db/org-default-refile-file))
                     (org-agenda-overriding-header "Things to refile")))))
        ("R" "Reading List"
             ((tags-todo "READ/-DONE-CANC"
                         ((org-agenda-overriding-header "To Read (unscheduled)")
                          (org-agenda-cmp-user-defined (db/cmp-date-property "CREATED"))
                          (org-agenda-sorting-strategy '(user-defined-up))
                          (org-agenda-todo-ignore-scheduled t)))))
        ("E" "Everything"
             ((tags-todo "/WAIT"
                         ((org-agenda-overriding-header "Tasks requiring response/input")))
              (tags-todo "-HOLD-READ-SOMEWHEN/-DONE"
                         ((org-agenda-overriding-header "Things not being scheduled or deadlined")
                          (org-tags-match-list-sublevels t)
                          (org-agenda-todo-ignore-with-date t)
                          (org-agenda-sorting-strategy
                           '(priority-down time-up category-keep))))
              (stuck ""
                     ((org-agenda-overriding-header "Stuck Tasks")))))
        ("S" "Somewhen"
             ((tags-todo "SOMEWHEN/-CANC-DONE"
                         ((org-agenda-overriding-header "Things to do somewhen")
                          (org-agenda-todo-ignore-with-date t)
                          (org-tags-match-list-sublevels nil)))
              (tags-todo "/HOLD"
                         ((org-agenda-overriding-header "Tasks on Hold")))))
        ("W" "Weekly Review"
             ((agenda ""
                      ((org-agenda-span 7)
                       (org-agenda-archives-mode t)
                       (org-agenda-dim-blocked-tasks nil)
                       (org-agenda-skip-deadline-prewarning-if-scheduled t)))))
        ("M" "Monthly Preview"
             ((db/org-agenda-list-deadlines
               ""
               ((org-agenda-overriding-header "Deadlines")
                (org-agenda-sorting-strategy '(deadline-up priority-down))
                (org-deadline-warning-days 90)))
              (agenda ""
                      ((org-agenda-span 'month)
                       (org-agenda-dim-blocked-tasks nil)
                       (org-deadline-warning-days 0) ; covered by display above
                       ))))
        ("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-use-tag-inheritance nil)
              (org-agenda-prefix-format '((tags . "  ")))))))

(defun db/org-add-clocking-time (starting-time ending-time)
  "Add \"CLOCK:\" line to the task under point in the current org-mode file."
  (interactive
   (list (org-read-date 4 'totime nil
                        "Start:" (current-time) nil t)
         (org-read-date 4 'totime nil
                        "End:" (current-time) nil t)))
  (if (not (eq major-mode 'org-mode))
      (user-error "Must be in org mode")
    (save-mark-and-excursion
     (org-clock-find-position nil)
     (open-line 1)
     (indent-according-to-mode)
     (insert "CLOCK: ")
     (org-insert-time-stamp starting-time t t)
     (insert "--")
     (org-insert-time-stamp ending-time t t)
     (org-clock-update-time-maybe))))

(bind-key "C-c C-x C-a" #'db/org-add-clocking-time org-mode-map)

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

(eval-after-load 'org-agenda
  '(bind-key "v" #'hydra-org-agenda-view/body org-agenda-mode-map))


;;; Capturing

;; disable usage of helm for `org-capture'
(eval-after-load 'helm-mode
  '(progn
     (defvar helm-completing-read-handlers-alist) ; for the byte compiler
     (add-to-list 'helm-completing-read-handlers-alist
                  '(org-capture . nil))))

(setq org-capture-use-agenda-date nil)

(setq org-capture-templates
      `(("t" "Todo"
             entry
             (file db/org-default-refile-file)
             ,(concat "* TODO %^{What}\n"
                      "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
                      ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                      "%?"))
        ("n" "Note"
             entry
             (file+olp db/org-default-notes-file "Notes")
             "* %^{About} :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
             :clock-in t :clock-resume t)
        ("d" "Date"
             entry
             (file db/org-default-refile-file)
             "* GOTO %^{What} :DATE:\n%^{When}t\n%a")
        ("i" "Interruptions")
        ("in" "Interruption now"
              entry
              (file db/org-default-refile-file)
              "* DONE %^{What}\n\n%?"
              :clock-in t :clock-resume t)
        ("ip" "Interruption previously" ; bad English vs mnemonics
              entry
              (file db/org-default-refile-file)
              ,(concat "* DONE %^{What}\n"
                       ":LOGBOOK:\n"
                       "%(db/read-clockline)\n" ; evaluated before above prompt?
                       ":END:\n"
                       "%?"))
        ("j" "journal entry"
             plain
             (file+datetree db/org-default-pensieve-file)
             "\n%i%U\n\n%?\n")
        ("r" "respond"
             entry
             (file db/org-default-refile-file)
             ,(concat "* TODO E-Mail: %:subject (%:from) :EMAIL:\n"
                      "SCHEDULED: %^{Reply when?}t\n"
                      ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                      "\n%a")
             :immediate-finish t)
        ("R" "read"
             entry
             (file db/org-default-refile-file)
             ,(concat "* READ %:subject :READ:\n"
                      ;; "DEADLINE: <%(org-read-date nil nil \"+1m\")>\n"
                      ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                      "\n%a"))
        ("U" "Read current content of clipboard"
             entry
             (file db/org-default-refile-file)
             ,(concat "* READ %^{Description} :READ:\n"
                      ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                      "\n%(current-kill 0)"))
        ("m" "Meeting"
             entry
             (file db/org-default-refile-file)
             ,(concat "* MEETING with %^{With}: %^{What} :MEETING:\n"
                      ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                      "\n%?")
             :clock-in t :clock-resume t)
        ("p" "Phone call"
             entry
             (file db/org-default-refile-file)
             ,(concat "* PHONE %^{Calling} :PHONE:\n"
                      ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                      "\n%?")
             :clock-in t :clock-resume t)
        ("w" "Weekly Summary"
             entry
             (file+datetree db/org-default-pensieve-file)
             "* Weekly Review\n\n%?")
        ("a" "Abkürzung"
             entry
             (file+olp db/org-default-notes-file "Abkürzungen")
             ,(concat "* %^{Abkürzung}\n"
                      ":PROPERTIES:\n"
                      ":FULL_NAME: %^{Full Name}\n"
                      ":END:\n\n"))))

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

(defun db/read-clockline ()
  "Read starting and ending time from user and return org mode
  clock line."
  (let* ((now      (format-time-string "%H:%M"))
         (starting (format "[%s]" (org-read-date t nil nil "Started: "
                                                 (current-time)
                                                 now)))
         (ending   (format "[%s]" (org-read-date t nil nil "Ended: "
                                                 (current-time)
                                                 now)))
         (difference (db/org-timestamp-difference starting ending)))
    (format "CLOCK: %s--%s => %s" starting ending difference)))


;;; Refiling

;; Refiling targets include this file and any file contributing to the agenda,
;; up to 9 levels deep
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))))

;; Use full outline paths for refile targets
(setq org-refile-use-outline-path 'file)

(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-indirect-buffer-display 'current-window)
(setq org-outline-path-complete-in-steps nil)


;;; Babel

(setq org-structure-template-alist
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("Q" "#+begin_equation\n?\n#+end_equation"
         "<equation>\n?\n</equation>")
        ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
        ("V" "#+begin_verbatim\n?\n#+end_verbatim" "<verbatim>\n?\n</verbatim>")
        ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
        ("l" "#+begin_latex\n?\n#+end_latex"
         "<literal style=\"latex\">\n?\n</literal>")
        ("l" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html"
         "<literal style=\"html\">\n?\n</literal>")
        ("h" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii" "")
        ("a" "#+ascii: " "")
        ("i" "#+index: ?" "#+index: ?")
        ("i" "#+include: %file ?"
         "<include file=%file markup=\"?\">")))

(setq org-src-fontify-natively t
      org-src-preserve-indentation t)


;;; Reset checklists

;; from `org-checklist’ by James TD Smith (@ ahktenzero (. mohorovi cc)),
;; version: 1.0

(defun org-reset-checkbox-state-maybe ()
  "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_CHECK_BOXES")
      (org-reset-checkbox-state-subtree)))

(add-hook 'org-after-todo-state-change-hook 'org-reset-checkbox-state-maybe)


;;; Calendar

(use-package ox-icalendar
  :commands (org-icalendar-combine-agenda-files)
  :config   (progn
              (setq org-icalendar-include-body nil
                    org-icalendar-store-UID t
                    org-icalendar-use-deadline nil
                    org-icalendar-use-scheduled nil
                    org-icalendar-include-todo nil
                    org-icalendar-exclude-tags '("NO_EXPORT"))))

(defun db/export-diary ()
  "Export diary.org as ics file to the current value of `org-icalendar-combined-agenda-file’.
This is done only if the value of this variable is not null."
  (interactive)
  (require 'ox-icalendar)
  (if (null org-icalendar-combined-agenda-file)
      (message "`org-icalendar-combined-agenda-file’ not set, not exporting diary.")
    (progn
      (org-save-all-org-buffers)
      (let ((org-agenda-files (cl-remove-if #'string-empty-p
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
          (org-icalendar-combine-agenda-files)
          (message "Exporting diary ... done."))))))

(defun db/ical-to-org (ical-file-name org-file-name category filetags)
  "Convert ICAL-FILE-NAME to ORG-FILE-NAME using ical2org.

CATEGORY and FILETAGS specify the category and the filetags of
the resulting org mode file, respectively."
  (when (string-match "^https?://" ical-file-name)
    (let ((tmp-file (make-temp-file "/tmp/emacs-ical-")))
      (url-copy-file ical-file-name tmp-file t)
      (setq ical-file-name tmp-file)))
  (unless (zerop (call-process "ical2org"
                               ical-file-name
                               `(:file ,org-file-name)
                               nil
                               "-c" category
                               "-f" filetags))
    (error (concat "Error in converting ical file «%s» into org file;"
                   " see «%s» for more information")
           ical-file-name org-file-name)))

(defvar db/ical-org-links nil
  "List of ical-file-names and their linked org mode files.")

(defun db/add-ical-org-link (ical-file-name org-file-name category filetags)
  "Add a new link from ICAL-FILE-NAME to ORG-FILE-NAME.

Resulting org mode file will have CATEGORY and FILETAGS set."
  (cl-pushnew (list ical-file-name org-file-name category filetags)
              db/ical-org-links
              :test #'equal))

(defun db/update-ical-org-files ()
  "Update all org mode files that are linked to some iCal file."
  (interactive)
  (dolist (entry db/ical-org-links)
    (cl-destructuring-bind (url target category filetags) entry
      (condition-case ex
          (progn
            ;; update TARGET
            (db/ical-to-org url target category filetags)
            ;; revert buffers visiting TARGET
            (with-current-buffer (find-buffer-visiting target)
              (let ((revert-without-query (list (regexp-quote (file-truename target)))))
                (revert-buffer))))
        (error
         (warn "Error in generating %s: %s" target (cadr ex)))))))

(defun db/sync-all-iCal ()
  "Sync all iCal sources."
  (interactive)
  (db/export-diary)
  (db/update-ical-org-files))


;;; Fixes

(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'endless/org-ispell)


;;; Exporting

(setq org-export-use-babel nil)

(eval-after-load 'ox
  '(progn
    (add-to-list 'org-latex-classes
     '("scrartcl" "\\documentclass[11pt]{scrartcl}\n\\usepackage{babel}\n"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (require 'ox-md)))


;;; Hydra

(require 'hydra)

(defhydra hydra-org-clock (:color blue)
  "
Current Task: %`org-clock-current-task; "
  ("w" (lambda ()
         (interactive)
         (clock-in-task-by-id org-working-task-id)))
  ("h" (lambda ()
         (interactive)
         (clock-in-task-by-id org-home-task-id)))
  ("b" (lambda ()
         (interactive)
         (clock-in-task-by-id org-break-task-id)))
  ("i" (lambda ()
         (interactive)
         (org-clock-in '(4))))
  ("a" counsel-org-goto-all)
  ("o" org-clock-out)
  ("l" db/org-clock-in-last-task)
  ("p" db/play-playlist)
  ("d" (lambda ()
         (interactive)
         (when (org-clock-is-active)
           (save-window-excursion
             (org-clock-goto)
             (let ((org-inhibit-logging 'note))
               (org-todo 'done)
               (org-save-all-org-buffers)))))))



;;; Custom links for Windows

(when (eq system-type 'windows-nt)
  (org-link-set-parameters "onenote" :follow #'db/org-onenote-open)

  (defun db/org-onenote-open (path)
    "Visit OneNote document on PATH."
    (w32-shell-execute "open" path)))


;;; Reporting

;; All of what follows should be available in org-mode somewhere, but doing it
;; myself was faster and also more fun :)

(defgroup timeline-reporting nil
  "Functionality for formatting timelines."
  :tag "Timeline Formatter"
  :group 'applications)

(require 'dash)

(defun db/org-map-clock-lines-and-entries (clockline-fn headline-fn)
  "Iterate point over all clocklines and headlines of the current buffer.
For each clockline, call CLOCKLINE-FN with the starting and
ending time as arguments and point on the beginning of the line.
For each headline, call HEADLINE-FN with no arguments and point
on the start of the headline.  Traversal will be done from the
end of the file upwards."
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
            (funcall clockline-fn (match-string 2) (match-string 3)))
           (t
            ;; A headline
            (funcall headline-fn))))))))

(defun db/org-clocking-time-in-range (tstart tend)
  "Return list of all tasks in the current buffer together with
their clocking times that are between TSTART and TEND.  The
resulting list consists of elements of the form

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
      (db/org-map-clock-lines-and-entries
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

(defun db/org-timeline-in-range (tstart tend &optional files)
  "Return list of clocked times from FILES between TSTART and
TEND.  Each element in this list is of the form

  (START END MARKER),

where START, END, MARKER are as returned by
`db/org-clocking-time-in-range’, which see.  Entries in the
resulting list are sorted by START, ascending."
  (let (timeline-of-files turned-around-timeline)
    (setq timeline-of-files
          (->> (or files org-agenda-files)
               (cl-remove-if-not #'file-exists-p)
               (cl-mapcan #'(lambda (file)
                              (with-current-buffer (or (get-file-buffer file)
                                                       (find-file-noselect file))
                                (db/org-clocking-time-in-range tstart tend))))))
    (dolist (entry timeline-of-files)
      (dolist (clock-time (cdr entry))
        (push (list (car clock-time) (cdr clock-time) (car entry))
              turned-around-timeline)))
    (sort turned-around-timeline
          (lambda (entry-1 entry-2)
            (< (car entry-1) (car entry-2))))))

(defun db/org-cluster-timeline-same-category (timeline)
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

(defun db/org-skip-short-entries-in-timeline (threshold timeline)
  "Skip short entries in TIMELINE.
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

(defun db/org-get-headline (marker)
  "Get headline of task at MARKER."
  (assert (markerp marker))
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

(defcustom timeline-short-task-threshold 300
  "Duration of task to be considered as short."
  :group 'timeline-reporting
  :type 'integer)

(defun db/org-format-timeline (tstart tend &optional files)
  "Display timeline of tasks in FILES between TSTART and TEND.
When not given, FILES defaults to `org-agenda-files’.  Short
slots are removed, and afterwards slots are clusted by category.
When called interactively, START and END are queried with
`org-read-date’."
  (interactive (list (org-read-date nil nil nil "Start time: ")
                     (org-read-date nil nil nil "End time: ")))
  (let ((timeline (->> (db/org-timeline-in-range tstart tend files)
                       (db/org-skip-short-entries-in-timeline
                        timeline-short-task-threshold)
                       db/org-cluster-timeline-same-category)))
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
            (dolist (headline (->> (mapcar #'db/org-get-headline markers)
                                   -uniq
                                   (-interpose "|\n |||||")))
              (insert headline))
            (insert "\n")))
        (insert "|--|\n")
        (goto-char (point-min))
        (org-table-align))
      (display-buffer target-buffer)
      t)))

(defun db/org-format-timeline-of-day (date &optional files)
  "Format timeline of given DATE.
DATE should be a string of the form %Y-%m-%d.  When called
interactively, this date will be queried with `org-read-date’.
The timeline will be formatted for DATE starting at 00:00 and
ending at 23:61.  When not given, FILES defaults to
`org-agenda-files’."
  (interactive (list (org-read-date nil nil)))
  (db/org-format-timeline (concat date " 00:00")
                          (concat date " 23:61")
                          files))


;;; Other Utilities

(defun db/bank-csv-to-org-table ()
  (interactive)
  (goto-char (point-min))
  (kill-line 8)
  (while (re-search-forward "^\"\\|\"$\\|\";\"" nil :no-error)
    (replace-match "|"))
  (goto-char (point-min))
  (org-mode)
  (org-table-align)
  ;; move columns around
  (cl-loop
   for (word . count) in '(("Wertstellung" . 6)
                           ("Umsatzart" . 6)
                           ("Buchungsdetails" . 3))
   do (progn (goto-char (point-min))
             (search-forward word)
             (dotimes (_ count)
               (org-table-move-column-right))))
  (goto-char (point-min)))

(defun db/org-cleanup-continuous-clocks ()
  "Join continuous clock lines in the current buffer."
  (interactive)
  (let* ((inactive-timestamp (org-re-timestamp 'inactive))
         (clock-line (concat "\\(^ *\\)CLOCK: "
                             inactive-timestamp
                             "--"
                             inactive-timestamp
                             " => .*\n *CLOCK: "
                             inactive-timestamp
                             "--\\[\\2\\] => .*$")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp clock-line nil t)
       (replace-match "\\1CLOCK: [\\4]--[\\3]")
       (org-clock-update-time-maybe)))))


;;; End

(provide 'db-org)

;;; db-org.el ends here
