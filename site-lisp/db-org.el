;;; org.el -- Daniel's org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Everything in here influences the standard commands coming with org-mode,
;; either by setting variables, adding hooks, or by overriding.

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
      org-highlight-latex-and-related '(latex)
      org-use-sub-superscripts '{}
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-ellipsis "⤵")

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


;;; How to open files

;; open directory links in emacs itself
(add-to-list 'org-file-apps '(directory . emacs))

;; use system default for Microsoft stuff
(add-to-list 'org-file-apps '("\\.docx?\\'" . default))
(add-to-list 'org-file-apps '("\\.pptx?\\'" . default))
(add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))

(when (eq system-type 'cygwin)
  (add-to-list 'org-file-apps '(t . "cygstart %s") t))


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
                          :test #'cl-equalp)))

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

(setq org-agenda-include-diary t
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
      '("-DATE-HOLD-REGULAR-HOLD-NOTE+TODO=\"\""
        ("CONT" "TODO" "READ" "WAIT" "GOTO" "DELG")
        ("NOP")
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

;; A Hydra for changing agenda appearance
;; http://oremacs.com/2016/04/04/hydra-doc-syntax/

(defun db/org-agenda-span ()
  "Return the display span of the current shown agenda."
  (let ((args (get-text-property
               (min (1- (point-max)) (point))
               'org-last-args)))
    (nth 2 args)))

(require 'hydra)

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

(with-eval-after-load 'org-agenda
  (bind-key "v" #'hydra-org-agenda-view/body org-agenda-mode-map))


;;; Capturing

;; disable usage of helm for `org-capture'
(with-eval-after-load 'helm-mode
  (defvar helm-completing-read-handlers-alist) ; for the byte compiler
  (add-to-list 'helm-completing-read-handlers-alist
               '(org-capture . nil)))

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
             (file db/org-default-refile-file)
             "* %^{About} :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
             :clock-in t :clock-resume t)
        ("d" "Date"
             entry
             (file db/org-default-refile-file)
             "* GOTO %^{What} :DATE:\n%^{When}t\n%a%?")
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
             ,(concat "* MEETING %^{What} :MEETING:\n"
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
        ("b" "Bookmark"
             entry
             (file+headline db/org-default-notes-file "Bookmarks")
             ,(concat "* [[%^{Link}][%^{Caption}]]\n"
                      ":PROPERTIES:\n:CREATED: %U\n:END:\n\n")
             :immediate-finish t)))

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

(add-to-list 'org-capture-templates
             '("s" "Code Snippet" entry (file db/org-default-refile-file)
               "* %?\n%(db/org-capture-code-snippet \"%F\")")
             t)


;;; Refiling

;; Refiling targets include this file and any file contributing to the agenda,
;; up to 9 levels deep
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))
                           (nil . (:maxlevel . 9))))

;; Use full outline paths for refile targets
(setq org-refile-use-outline-path 'file)

(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-indirect-buffer-display 'current-window)
(setq org-outline-path-complete-in-steps nil)

; Exclude DONE state tasks from refile targets (from bh)
(defun db/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'db/verify-refile-target)


;;; Reset checklists

;; from `org-checklist’ by James TD Smith (@ ahktenzero (. mohorovi cc)),
;; version: 1.0

(defun org-reset-checkbox-state-maybe ()
  "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_CHECK_BOXES")
      (org-reset-checkbox-state-subtree)))

(add-hook 'org-after-todo-state-change-hook 'org-reset-checkbox-state-maybe)



;;; Fixes

(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'endless/org-ispell)


;;; Hydra

(defun db/clock-in-task-by-id (task-id)
  "Clock in org mode task as given by TASK-ID."
  (org-with-point-at (org-id-find task-id 'marker)
    (org-clock-in))
  (org-save-all-org-buffers))

(defun db/clock-out-task-by-id (task-id)
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

(defhydra hydra-org-clock (:color blue)
  "
Current Task: %`org-clock-current-task; "
  ("w" (lambda ()
         (interactive)
         (db/clock-in-task-by-id org-working-task-id)))
  ("h" (lambda ()
         (interactive)
         (db/clock-in-task-by-id org-home-task-id)))
  ("b" (lambda ()
         (interactive)
         (db/clock-in-task-by-id org-break-task-id)))
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



;;; Drag-and-Drop images into org-mode buffer

(use-package org-download)


;;; Babel

(use-package ob-core
  :defer t
  :init (setq org-export-use-babel nil)
  :config (setf (alist-get :results org-babel-default-header-args)
                "output code replace"))

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


;;; Hacks

;; The default implementation is too slow, because it is parsing all properties
;; of an entry by default.  Let’s simplify this to only parse what we are
;; looking for.  This makes tag search *much* faster!

(with-eval-after-load 'org
  (defun org-cached-entry-get (pom property)
    (if (or (eq t org-use-property-inheritance)
            (and (stringp org-use-property-inheritance)
                 (let ((case-fold-search t))
                   (string-match-p org-use-property-inheritance property)))
            (and (listp org-use-property-inheritance)
                 (member-ignore-case property org-use-property-inheritance)))
        ;; Caching is not possible, check it directly.
        (org-entry-get pom property 'inherit)
      ;; This is different in the original implementation
      (org-entry-get pom property))))


;;; End

(provide 'db-org)

;;; db-org.el ends here
