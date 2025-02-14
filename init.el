;;; Init.el --- Daniel's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This is the main entry point for Emacs to load this configuration.  The
;; structure is roughly as follows:
;; - first comes some preliminary setup, mostly setting up `package’;
;; - the main activation of the configuration is done in the function
;;   `db/run-init’, which is installed in `after-init-hook’; it is thus run
;;   after init.el has been read
;; - then comes setting up all the packages that can be used by this
;;   configuration; most of these packages are not loaded however, and only
;;   configuration hooks are installed (using `use-package’); this way a user
;;   can choose in `db/run-init’ which configuration to activate without
;;   changing much of the rest of the file.
;; - this file also introduces a new customization group `personal-settings’
;;   containing variables (mostly file paths) that must be set to enable some
;;   of the provided functionality.

;;; Code:


;; * Preliminaries (constants and path settings)

(when (version< emacs-version "29")
  (error "Emacs version is too old!  We need at least Emacs 29, but this is %s"
         emacs-version))

(defconst emacs-d (file-name-directory
                   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

(add-to-list 'load-path (expand-file-name "site-lisp" emacs-d))

(defvar emacs-d-userdata (expand-file-name "private/" emacs-d)
  "Directory to store user data in.")

;; Ensure that `emacs-d-userdata' exists as a directory, because we want to
;; store data there.
(unless (file-directory-p emacs-d-userdata)
  (make-directory emacs-d-userdata))

(setq custom-file (expand-file-name "custom.el" emacs-d-userdata)
      custom-theme-directory (expand-file-name "themes/" emacs-d))

(defconst on-windows (memq system-type '(windows-nt cygwin))
  "Non-nil if and only if this instance of Emacs runs on Windows.")


;; * Packages

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(defvar use-package-enable-imenu-support t)

(eval-when-compile
  (dolist (package '(bind-key use-package))
    (unless (package-installed-p package)
      (package-install package))
    (require package)))

(setq use-package-always-defer t
      use-package-verbose t
      use-package-minimum-reported-time 0.01)

(add-to-list 'package-pinned-packages '(use-package . "melpa-stable"))
(add-to-list 'package-pinned-packages '(bind-key . "melpa-stable"))

(put 'use-package 'lisp-indent-function 1)
(put 'use-package 'common-lisp-indent-function 1)


;; * Personal Customization Variables

(use-package db-customize
  :demand t
  :defines (db/jabber-id
            db/important-documents-path
            db/path-to-onenote
            db/path-to-outlook
            db/cert-file-directory
            org-working-task-id
            org-break-task-id
            org-home-task-id
            db/org-clock-current-task-file
            db/org-default-org-file
            db/org-default-work-file
            db/org-default-home-file
            db/org-default-notes-file
            db/org-default-refile-file
            db/after-init-load-files))


;; * Core Configuration

(use-package cl-lib
  :demand t)

(use-package subr-x
  :demand t)

;; Encoding: use UTF-8 everywhere
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Startup configuration
(setq inhibit-startup-message t
      inhibit-default-init t
      frame-inhibit-implied-resize t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      ring-bell-function #'ignore
      load-prefer-newer nil             ; t breaks `org-reload'
      use-short-answers nil)

(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)

(setq frame-title-format "emacs")

(use-package help-fns
  :defines (help-enable-symbol-autoload))

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      scroll-conservatively 10
      scroll-preserve-screen-position 'always ; Make M-v undo C-v
      message-log-max t
      inhibit-eol-conversion nil
      tab-always-indent 'complete
      enable-recursive-minibuffers t
      set-mark-command-repeat-pop t
      echo-keystrokes 0.1
      delete-trailing-lines nil
      x-underline-at-descent-line t
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      history-delete-duplicates t
      track-eol t
      garbage-collection-messages nil
      read-process-output-max (* 1024 1024) ; 1mb
      next-error-message-highlight t
      help-enable-symbol-autoload t
      describe-bindings-outline t
      redisplay-skip-fontification-on-input t
      undo-limit 80000000
      async-shell-command-buffer 'new-buffer
      byte-compile-warnings '(not docstrings))

(put 'set-goal-column 'disabled nil)

(when (memq system-type '(gnu gnu/linux gnu/kfreebsd))
  (setq x-wait-for-event-timeout nil))

(when on-windows
  ;; Fix: treat memory for display time ... hey, this is Windows, memory doesn’t
  ;; matter!
  (setq inhibit-compacting-font-caches t))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties '(read-only t
                                     face minibuffer-prompt
                                     cursor-intangible t))

;; Fix: disable gconf settings, as it might interfere with ours, see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25228 and
;; https://emacs.stackexchange.com/questions/32641/something-changes-the-default-face-in-my-emacs.
(define-key special-event-map [config-changed-event] 'ignore)

;; Individual package configuration from here on.

(use-package appt
  :commands (appt-activate)
  :init (setq appt-display-mode-line nil))

(use-package auth-source
  :init (setq auth-sources '("~/.authinfo.gpg")
              auth-source-save-behavior nil))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package browser-url
  :init (setq browse-url-browser-function 'browse-url-generic
              browse-url-generic-program "firefox"))

(use-package calc
  ;; https://florian.adamsky.it/2016/03/31/emacs-calc-for-programmers-and-cs.html
  :defines (math-additional-units
            math-units-table)
  :init (setq math-additional-units
              '((bit nil "Bit")
                (byte "8 * bit" "Byte")
                (bps "bit / s" "Bit per second"))
              math-units-table nil))

(use-package calender
  :init (setq calendar-date-style 'iso
              calendar-time-zone-style 'numeric
              calendar-week-start-day 1 ; Monday
              calendar-bahai-all-holidays-flag nil
              calendar-chinese-all-holidays-flag nil
              calendar-christian-all-holidays-flag nil
              calendar-islamic-all-holidays-flag nil
              calendar-hebrew-all-holidays-flag nil
              ;; The following list should contain only holidays without work, as it is examined by
              ;; `db/org-home-time-for-date'.
              holiday-general-holidays '((holiday-fixed 1 1 "New Year's Day")
                                         (holiday-fixed 5 1 "Labour Day")
                                         (holiday-fixed 10 3 "German Unity Day")
                                         (holiday-fixed 10 31 "Reformation Day")
                                         (holiday-float 11 3 -1 "Day of Repentance and Prayer" 22))
              holiday-other-holidays '((holiday-fixed 2 13 "Jahrestag Zerstörung Dresden 1945")
                                       (holiday-fixed 2 14 "Valentine's Day")
                                       (holiday-fixed 4 1 "April Fools' Day")

                                       (holiday-fixed 5 25 "Towel Day")
                                       (holiday-fixed 6 4 "Tiananmen Massacre 1989")
                                       (holiday-fixed 6 5 "Snowden-Veröffentlichungen 2013")
                                       (holiday-fixed 6 6 "D-Day 1944")
                                       (holiday-fixed 6 8 "Yoneda Appreciation Day")
                                       (holiday-fixed 6 10 "Jahrestag Zerstörung von Oradour-sur-Glane 1944")
                                       (holiday-fixed 6 10 "Jahrestag Massaker von Distomo 1944")
                                       (holiday-fixed 6 16 "Bloomsday")
                                       (holiday-fixed 7 20 "Jahrestag Attentat auf Hitler 1944")
                                       (holiday-fixed 7 21 "Jahrestag der 1. Mondlandung 1969")
                                       (holiday-fixed 7 21 "Jahrestag Massaker von Vassieux-en-Vercors 1944")
                                       (holiday-fixed 7 28 "Start WWI 1914")
                                       (holiday-float 11 4 4 "Thanksgiving")
                                       (holiday-fixed 11 11 "End WWI 1918"))
              diary-show-holidays-flag t
              calendar-view-holidays-initially-flag nil))

(use-package grep
  :commands (rgrep zrgrep)
  :bind (:map grep-mode-map
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit))
  :config (progn

            ;; I am not quite sure why `grep-read-files' is prompting for file
            ;; names when asking for a file pattern, so let's just hook it up
            ;; and replace it with something more straightforward.
            (advice-add 'grep-read-files :around #'db/grep-read-files)))

(use-package multisession
  :init (setq multisession-directory (expand-file-name "multisession/" emacs-d-userdata)))

(use-package proced
  :custom ((proced-tree-flag t)
           (proced-auto-update-flag (not on-windows))
           (proced-format 'medium)
           (proced-auto-update-interval 1)
           (proced-goal-attribute nil)
           (proced-enable-color-flag t)))

(use-package project
  :init (setq project-list-file (expand-file-name "projects" emacs-d-userdata))
  :config (progn

            ;; Sort known projects before persisting, to reduce committer noise
            (define-advice project--write-project-list (:before
                                                        ()
                                                        sort-before-writing)
              (setq project--list (cl-sort project--list
                                           #'string<
                                           :key #'cl-first)))))

(use-package quail
  :init (setq default-input-method "TeX")
  :config (add-hook 'input-method-activate-hook
                    #'db/add-symbols-to-TeX-input-method))

(use-package savehist
  :commands (savehist-mode)
  :init (setq savehist-file (expand-file-name "savehist" emacs-d-userdata)))

(use-package server
  :commands (server-running-p server-start)
  :init (setq server-log t))

(use-package tab-bar
  :init (setq tab-bar-show t
              tab-bar-format '(tab-bar-format-tabs
                               tab-bar-separator
                               ;; tab-bar-format-align-right
                               ;; current-time-string
                               )))

(use-package url
  :init (setq url-configuration-directory (expand-file-name "url" emacs-d-userdata)))

(use-package window
  :init (setq switch-to-buffer-obey-display-actions t
              switch-to-buffer-in-dedicated-window 'pop
              recenter-positions '(top middle bottom)
              display-buffer-base-action '(display-buffer-reuse-window))
  :config (progn
            (add-to-list 'display-buffer-alist
                         '("^\\*Async Shell Command*"
                           (display-buffer-no-window)))
            (add-to-list 'display-buffer-alist
                         '("^\\*Warnings\\*"
                           (display-buffer-in-side-window)
                           (side . right)
                           (slot . 1)
                           (window-width . 0.33)))
            ;; Inspired by masteringemacs
            (add-to-list 'display-buffer-alist
                         '("^\\*Help\\*"
                           (display-buffer-reuse-window
                            display-buffer-pop-up-window)))
            ;; Inspired by masteringemacs and
            ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layouts-with-Side-Windows.html
            (add-to-list 'display-buffer-alist
                         '("^\\*eshell\\*"
                           display-buffer-in-side-window
                           (side . bottom)
                           (slot . -1)
                           (window-height . 0.33)
                           (window-parameters . ((no-other-window . t)))))
            (add-to-list 'display-buffer-alist
                         '("^\\*shell\\*"
                           display-buffer-in-side-window
                           (side . bottom)
                           (slot . 1)
                           (window-height . 0.33)
                           (window-parameters . ((no-other-window . t)))))))

(use-package winner
  :commands (winner-mode winner-undo winner-redo))


;; * Basic External Packages

(use-package crux
  :ensure t
  :commands (crux-eval-and-replace
             crux-smart-open-line-above
             crux-kill-whole-line
             crux-cleanup-buffer-or-region
             crux-delete-buffer-and-file))

(use-package dash
  :defer nil
  :autoload (-difference)
  :config (progn
            (global-dash-fontify-mode)
            (dash-register-info-lookup)))

(use-package db-utils
  :commands (endless/fill-or-unfill
             db/delete-trailing-whitespace-maybe
             db/run-or-hide-shell
             db/gnus
             db/org-agenda
             db/scratch
             db/find-user-init-file
             db/run-or-hide-ansi-term
             db/ement-connect
             db/hex-to-ascii
             db/text-to-hex
             turn-on-lispy-when-available
             turn-on-flycheck-when-file
             db/sort-nsm-permanent-settings
             endless/colorize-compilation
             db/turn-off-local-electric-pair-mode
             db/add-symbols-to-TeX-input-method
             db/two-monitors-xrandr
             db/one-monitor-xrandr
             db/pretty-print-xml
             db/bookmark-add-external
             db/bookmark-add-url
             db/lookup-smime-key
             db/dired-from-shell-command
             db/system-open
             db/switch-to-dark-theme
             db/switch-to-light-theme
             keyboard-quit-context+
             db/convert-lf-to-crlf-in-buffer
             db/convert-crlf-to-lf-in-buffer
             db/replace-variables-in-string
             db/dired-ediff-files
             db/grep-read-files
             db/make-selector-from-table-header
             db/get-library-version
             db/dired-back-to-top
             db/dired-jump-to-bottom
             db/dired-get-size))

(use-package db-hydras
  :commands (hydra-toggle/body
             hydra-zoom/body
             hydra-rectangle/body
             db/define-feature-shortcuts-hydra
             hydra-feature-shortcuts/body))

(use-package diminish
  :ensure t)

(use-package exec-path-from-shell
  :pin "melpa-stable"
  :commands (exec-path-from-shell-copy-envs))

(use-package helpful
  :pin "melpa-stable"
  :ensure t)

(use-package hydra
  :pin "melpa-stable"
  :ensure t)

;; `lv' is a dependency of `hydra'
(add-to-list 'package-pinned-packages '(lv . "melpa-stable"))

(use-package projectile
  :commands (projectile-mode)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init (setq projectile-switch-project-action 'projectile-dired
              projectile-completion-system 'helm
              projectile-ignored-project-function #'file-remote-p
              projectile-create-missing-test-files t
              projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld"
                                                               emacs-d-userdata)
              projectile-cache-file (expand-file-name "projectile.cache"
                                                      emacs-d-userdata)))


;; * Text editing

(setq sentence-end-double-space t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(use-package abbrev
  :init (setq save-abbrevs 'silently
              abbrev-file-name (expand-file-name "abbrev_defs" emacs-d-userdata))
  :diminish abbrev-mode)

(use-package dictionary
  :init (setq dictionary-server "dict.org"))

(use-package electric
  :commands (electric-quote-mode))

(use-package elec-pair
  :commands (electric-pair-mode)
  :config   (progn
              (add-to-list 'electric-pair-pairs '(?“ . ?”))
              (add-to-list 'electric-pair-text-pairs '(?“ . ?”))
              (add-to-list 'electric-pair-pairs '(?„ . ?“))
              (add-to-list 'electric-pair-text-pairs '(?„ . ?“))))

(use-package flyspell
  :commands (flyspell-mode turn-on-flyspell)
  :config (progn
            (unbind-key "C-M-i" flyspell-mode-map)
            (unbind-key "C-c $" flyspell-mode-map)))

(use-package key-chord
  :ensure t
  :commands (key-chord-mode)
  :config (progn
            (key-chord-define-global "``" "“")
            (key-chord-define-global "''" "”")
            (key-chord-define-global ",," "„")))

(use-package multiple-cursors
  :pin "melpa-stable"
  :ensure t
  :commands (mc/edit-lines
             mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this))

(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :preface (progn
             (defun turn-on-olivetti-mode ()
               "Turn on `olivetti-mode'."
               (interactive)
               (olivetti-mode 1))
             (defun turn-off-olivetti-mode ()
               "Turn off `olivetti-mode'."
               (interactive)
               (olivetti-mode -1)))
  :init (setq-default olivetti-body-width 0.618034))

(use-package undo-tree
  :ensure t
  :commands (global-undo-tree-mode
             undo
             undo-tree-redo)
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-auto-save-history nil
              undo-tree-history-directory-alist `(("." . ,(expand-file-name "ebackup/" emacs-d))))
  :diminish undo-tree-mode)

(use-package wgrep
  :ensure t
  :commands (wgrep-finish-edit
             wgrep-change-to-wgrep-mode))

(use-package yasnippet
  :commands (yas-minor-mode-on
             yas-minor-mode
             yas-global-mode
             yas-reload-all)
  :ensure t
  :diminish yas-minor-mode
  :config (yas-reload-all))


;; * Org

(use-package db-org
  :commands (db/check-special-org-files-in-agenda
             db/verify-refile-target
             db/find-parent-task
             db/ensure-running-clock
             db/save-current-org-task-to-file
             db/org-update-frame-title-with-current-clock
             db/org-clock-out
             db/org-clock-in-break-task
             db/org-clock-in-home-task
             db/org-clock-in-work-task
             db/show-current-org-task
             db/org-remaining-effort-of-current-item
             db/org-cmp-remaining-effort
             org-dblock-write:db/org-workload-report
             db/org-home-time-for-date
             endless/org-ispell
             db/org-agenda-list-deadlines
             db/org-agenda-insert-active-filters
             hydra-org-agenda-view/body
             db/org-agenda-insert-efforts
             db/org-eval-subtree-no-confirm
             db/org-timestamp-difference
             db/org-capture-code-snippet
             hydra-org-clock/body
             hydra-org-jump/body
             hydra-org-custom/body
             db/org-execute-babel-in-buffer-and-iterate-tables
             db/make-org-capture-frame
             db/org-onenote-open
             db/org-outlook-open
             db/org-rfc-open
             db/org-cleanup-continuous-clocks
             db/find-csv-in-org
             db/org-mark-current-default-task
             db/org-clocktable-write-with-threshold
             db/export-diary
             db/org-insert-checklist
             db/org-copy-body-from-item-to-point
             db/org-find-links-to-current-item
             db/org-add-link-to-other-item
             db/org-add-link-to-current-clock
             hydra-org-linking/body
             org-dblock-write:db/org-backlinks
             db/org-clock-goto-first-open-checkbox
             org-password-manager-get-password-by-id))

;; This is to make the byte-compiler happy about setting some variables later on
;; that are defined in those packages.
(use-package org-attach)
(use-package org-id)
(use-package org-goto)

(use-package org
  :pin "gnu"
  :bind (:map org-mode-map
              ([remap org-return] . (lambda () (interactive) (org-return :indent)))
              ([remap org-clock-goto] . db/org-clock-goto-first-open-checkbox))
  :autoload (org-get-todo-state
             org-entry-get)
  :commands (org-return)
  :init (progn
          (setq org-deadline-warning-days 14
                org-read-date-popup-calendar t
                org-insert-heading-respect-content t
                org-adapt-indentation nil
                org-edit-timestamp-down-means-later t
                org-archive-location "%s_archive.gpg::"
                org-image-actual-width nil
                org-imenu-depth 10
                org-footnote-section nil
                org-log-into-drawer "LOGBOOK"
                org-log-reschedule 'note
                org-log-redeadline 'note
                org-log-refile 'note
                org-log-note-clock-out nil
                org-log-done 'note
                org-clone-delete-id t
                org-fold-catch-invisible-edits 'show-and-error
                org-M-RET-may-split-line '((default . nil))
                org-highlight-latex-and-related '(latex)
                org-use-sub-superscripts '{}
                org-src-fontify-natively t
                org-src-preserve-indentation t
                org-src-tab-acts-natively nil
                org-ellipsis "↲"
                org-fontify-done-headline nil
                org-cycle-separator-lines 2
                org-special-ctrl-a/e t
                org-highlight-latex-and-related nil
                org-attach-store-link-p 'attached
                org-attach-auto-tag nil
                org-bookmark-names-plist nil
                org-goto-interface 'outline-path-completion
                org-id-link-to-org-use-id t

                org-blank-before-new-entry '((heading . t)
                                             (plain-list-item . auto))

                org-duration-format '(("y") ("w") ("d") (special . h:mm))

                org-treat-S-cursor-todo-selection-as-state-change nil

                org-global-properties
                '(("Effort_ALL" . "0:00 0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 6:00 8:00"))

                org-columns-default-format
                "%80ITEM(Task) %10Effort(Effort) %10CLOCKSUM")

          ;; Keywords and Tags

          (setq org-todo-keywords
                '((sequence "TODO(t@)" "CONT(n!)" "REFINE(f!)" "|" "DONE(d@)" "CANC(c@)" "MRGD(m@)")
                  (sequence "GOTO(g@)" "ATTN(a!)" "|" "DONE(d@)")
                  (sequence "READ(r@)" "CONT(n!)" "|" "DONE(d@)")
                  (sequence "DELG(e@)" "WAIT(w@)" "HOLD(h@)" "|" "CANC(c@)"))

                org-todo-state-tags-triggers
                '(("WAIT" ("HOLD") ("WAIT" . t))
                  ("DELG" ("HOLD") ("WAIT" . t))
                  ("HOLD" ("HOLD" . t) ("WAIT"))
                  (done ("HOLD") ("WAIT"))
                  ("TODO" ("HOLD") ("WAIT") ("DATE") ("READ"))
                  ("READ" ("READ" . t) ("DATE") ("HOLD") ("WAIT"))
                  ("GOTO" ("DATE" . t) ("READ") ("HOLD") ("WAIT"))
                  ("CONT" ("DATE") ("HOLD") ("WAIT") ("READ"))
                  ("REFINE" ("DATE") ("HOLD") ("WAIT") ("READ"))
                  ("ATTN" ("DATE" . t) ("READ") ("HOLD") ("WAIT"))
                  ("" ("HOLD") ("WAIT") ("DATE") ("READ")))

                org-tag-alist
                '((:startgroup)
                  ("WORK" . ?w)
                  ("HOME" . ?h)
                  ("FUN" . ?f)
                  ("UNTAGGED" . ?u)
                  (:endgroup)
                  ("NOTE" . ?n)
                  (:startgroup)
                  ("PERIODIC" . ?p)
                  ("NOP" . ?o)
                  ("TOPIC" . ?t)
                  ("TEMPLATE" . ?l)
                  ("GOAL" . ?g)
                  (:endgroup))

                org-tags-exclude-from-inheritance
                '("NOP" "TOPIC" "GOAL")
                org-tags-column -85

                org-fast-tag-selection-single-key 'expert)

          ;; Faces

          (setq org-todo-keyword-faces
                '(("TODO" :foreground "red" :weight normal)
                  ("GOTO" :foreground "red" :weight normal)
                  ("READ" :foreground "red" :weight normal)
                  ("CONT" :foreground "DeepSkyBlue" :weight normal)
                  ("ATTN" :foreground "DeepSkyBlue" :weight normal)
                  ("DONE" :foreground "forest green" :weight normal)
                  ("MRGD" :foreground "dark cyan" :weight normal)
                  ("DELG" :foreground "dark orange" :weight normal)
                  ("REFINE" :foreground "tomato" :weight normal)
                  ("WAIT" :foreground "orange" :weight normal)
                  ("HOLD" :foreground "magenta" :weight normal)
                  ("CANC" :foreground "lime green" :weight normal))

                org-priority-faces
                '((?A . (:foreground "Red" :weight bold))
                  (?B . (:foreground "firebrick"))
                  (?C . (:foreground "tomato"))))

          ;; Refiling

          (setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))
                                     (nil . (:maxlevel . 9))
                                     (db/org-default-notes-file . (:maxlevel . 9)))
                org-refile-use-outline-path 'buffer-name
                org-refile-use-cache nil
                org-refile-allow-creating-parent-nodes 'confirm
                org-indirect-buffer-display 'current-window
                org-outline-path-complete-in-steps nil
                org-refile-target-verify-function 'db/verify-refile-target)

          ;; Babel

          (setq org-babel-load-languages '((shell . t)
                                           (emacs-lisp . t)
                                           (sql . t)))

          ;; Link shortcuts (some taken from the documentation)

          (setq org-link-abbrev-alist
                '(("wpen" . "https://en.wikipedia.org/wiki/")
                  ("wpde" . "https://de.wikipedia.org/wiki/")
                  ("ddg" . "https://duckduckgo.com/?q=%s")
                  ("omap" . "https://nominatim.openstreetmap.org/search?q=%s&polygon=1")
                  ("github" . "https://github.com/")
                  ("gitlab" . "https://gitlab.com/"))))
  :config (progn

            ;; Some hooks from text-mode-hook, Org mode does not seem to run those?
            (add-hook 'org-mode-hook 'page-break-lines-mode)
            (add-hook 'org-mode-hook 'turn-on-auto-fill)

            ;; Hide drawers by default when cycling items
            (add-hook 'org-cycle-hook #'org-cycle-hide-drawers)

            ;; Statically color links indepedently of whether the file exists or
            ;; not (we could add different faces to highlight non-existing
            ;; files, but this turns out to be slow on Windows, and we can use
            ;; `org-lint' to check this when necessary)
            (org-link-set-parameters "file" :face 'org-link)

            ;; File Apps

            (add-to-list 'org-file-apps '(directory . emacs))

            (add-to-list 'org-file-apps '("\\.docx?\\'" . default))
            (add-to-list 'org-file-apps '("\\.pptx?\\'" . default))
            (add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))

            (when (eq system-type 'cygwin)
              (add-to-list 'org-file-apps '(t . "cygstart %s") t))

            ;; Custom link types
            (org-link-set-parameters "rfc" :follow #'db/org-rfc-open)
            (org-link-set-parameters "cve" :follow #'(lambda (number)
                                                       (browse-url
                                                        (format "https://www.cve.org/CVERecord?id=CVE-%s"
                                                                number))))
            (when (eq system-type 'windows-nt)
              (org-link-set-parameters "onenote" :follow #'db/org-onenote-open)
              (org-link-set-parameters "outlook" :follow #'db/org-outlook-open))

            ;; Mark some org mode regions to be skipped by ispell
            (add-hook 'org-mode-hook #'endless/org-ispell)

            ;; Some timers

            (unless (memq #'org-clock-save
                          (mapcar #'timer--function timer-list))
              (run-with-timer 0 3600 #'org-clock-save))

            ;; Hack: The default implementation is too slow, because it is
            ;; parsing all properties of an entry by default.  Let’s simplify
            ;; this to only parse what we are looking for.  This makes tag
            ;; search *much* faster!
            (when (version<= org-version "9.5.5")
              (with-eval-after-load 'org
                (define-advice org-cached-entry-get (:around
                                                     (orig-fun pom property)
                                                     speed-up-cache-lookup)
                  (ignore orig-fun)
                  (if (or (eq t org-use-property-inheritance)
                          (and (stringp org-use-property-inheritance)
                               (let ((case-fold-search t))
                                 (string-match-p org-use-property-inheritance property)))
                          (and (listp org-use-property-inheritance)
                               (member-ignore-case property org-use-property-inheritance)))
                      ;; Caching is not possible, check it directly.
                      (org-entry-get pom property 'inherit)
                    ;; This is different in the original implementation
                    (org-entry-get pom property)))))

            ;; Fix (Org 9.5.5): the variable `org-time-was-given' dynamically
            ;; bound to signify when `org-read-date-analyze' has found a hh:mm
            ;; part; it's only considered, however, when it has previously been
            ;; bound, resulting in some prior calls of `org-read-date' to ignore
            ;; the hh:mm part.  So let's bind this variable now to make things
            ;; work.
            (defvar org-time-was-given nil)

            ;; Completely redo agenda buffer when jumping to today; this ensures that agenda views
            ;; from previous days get updated as expected.
            (define-advice org-agenda-goto-today (:before () redo-all)
              (org-agenda-redo-all))))

(use-package org-cycle
  :autoload (org-cycle-hide-drawers)
  :init (setq org-cycle-include-plain-lists 'integrate))

(use-package org-lint
  :autoload (org-lint-checker-name)
  :config (progn
            ;; Yes, this is ugly, but I would like to get rid of checking for
            ;; obsolete percentage encoding in URLs without loosing the other
            ;; checkers.
            (setq org-lint--checkers (cl-remove-if #'(lambda (c)
                                                       (eq (org-lint-checker-name c)
                                                           'percent-encoding-link-escape))
                                                   org-lint--checkers))))

;; Drag-and-Drop images into org-mode buffer
(use-package org-download
  :commands (org-download-yank
             org-download-screenshot
             org-download-clipboard)
  :init (setq org-download-method 'attach))

;; Extended query language and dynamic blocks
(use-package org-ql
  :pin "melpa-stable"
  :ensure t
  :commands (org-ql-view
             org-ql-search
             org-dblock-write:org-ql)
  ;; `org-ql-search' defines the dblock features of `org-ql' but is not loaded
  ;; by default, so let's do this here; note that this implicitly loads
  ;; `org-ql-view' as well.
  :config (require 'org-ql-search))

(use-package ol
  :init (setq org-link-keep-stored-after-insertion t)
  :commands (org-store-link)
  :autoload (org-link-set-parameters)
  :config (progn

            ;; Allow additional link targets
            (require 'ol-man)           ; manpages
            (with-demoted-errors "Cannot load package: %s"
              (require 'orgit))         ; magit buffers

            (when (version< (org-version) "9.7")
              ;; Pushing already stored links to the front of `org-stored-links' became default
              ;; behavior in Org 9.7, the below code is thus only needed for older versions.

              (define-advice org-store-link (:around
                                             (orig-func &rest args)
                                             db/org--push-new-links-to-beginning)
                "Ensure that new links in `org-store-link' are always at the beginning."
                ;; The idea here is to store the list of already known links,
                ;; stored in `org-stored-links', and call `org-store-link' with an
                ;; empty value of `org-stored-links'.  This way, the link to the
                ;; current item is store in any case, and we prepend these new
                ;; values (can be more than one if CUSTOM_ID is set) to the old
                ;; list of links.  We also remove the newly prepended links from
                ;; the list of already known links.

                (let ((org-stored-links--original org-stored-links)
                      (org-stored-links--new nil)
                      (org-store-link--return-value nil))

                  (let ((org-stored-links nil))
                    ;; This is the actual call to `org-store-link', which may (and
                    ;; usually will) update `org-stored-links'.  Note that the new
                    ;; value of `org-stored-links' is only available after
                    ;; `org-store-link' as finished, which is why we make two
                    ;; separate calls to `setq' here instead of only one.
                    (setq org-store-link--return-value (apply orig-func args))
                    (setq org-stored-links--new org-stored-links))

                  ;; Note: `org-stored-links--new' might still be nil after
                  ;; calling `org-store-link', as under some circumstances (and
                  ;; only when the `interactive?' argument to `org-store-link' is
                  ;; non-nil), `org-store-link' may only return a link and not
                  ;; update `org-stored-links'; in this case, we do not have to
                  ;; touch the original value of `org-stored-links' at all.

                  (unless (null org-stored-links--new)
                    (setq org-stored-links (nconc org-stored-links--new
                                                  (cl-remove-if #'(lambda (x)
                                                                    (member x org-stored-links--new))
                                                                org-stored-links--original))))

                  org-store-link--return-value)))

            (define-advice org-link-make-string (:around
                                                 (orig-func link &optional description)
                                                 db/org--remove-statistics-cookie-from-link-descriptions)
              "Remove statistics cookies from link descriptions.

Such cookies get updated with other statistics cookies and
quickly loose their meaning.

Temporary fix: when the description ends on a closing
bracket (ignoring trailing whitespace) after the statistics
cookie has been removed, add a non-breaking space to the end.
This is to fix the current alignment of Org tables, which counts
a zero-width character, which is used to escape the end of an Org
link, as one character but does not adjust the width of the table
accordingly."
              (funcall orig-func
                       link
                       (when description
                         ;; Inspired by `org--get-outline-path-1':
                         (->> description
                              (replace-regexp-in-string "\\[[0-9]*%\\]\\|\\[[0-9]+/[0-9]+\\]\\|\\[/\\]" "")
                              (replace-regexp-in-string "\\]\s-*$" "] ") ; XXX temporary
                              (org-trim)))))))

(use-package ol-bbdb
  :config (add-to-list 'org-bbdb-anniversary-format-alist
                       (cons "day-of-death"
                             #'(lambda (name years suffix)
                                 (format "Day of Death: [[bbdb:%s][%s (%s%s)]]"
                                         name name years suffix)))))

(use-package org-clock
  :commands (org-clock-save)
  :init (progn
          (setq org-clock-history-length 35
                org-clock-in-resume t
                org-clock-into-drawer t
                org-clock-idle-time nil
                org-clock-out-remove-zero-time-clocks t
                org-clock-out-when-done '("DONE" "CANC" "MRGD" "WAIT" "HOLD")
                org-clock-auto-clock-resolution 'when-no-clock-is-running
                org-clock-mode-line-total 'auto
                org-clock-report-include-clocking-task t
                org-clock-in-switch-to-state #'(lambda (_)
                                                 (when (not
                                                        (and (boundp 'org-capture-mode)
                                                             org-capture-mode))
                                                   (cond
                                                     ((member (org-get-todo-state)
                                                              (list "TODO" "READ"))
                                                      "CONT")
                                                     ((member (org-get-todo-state)
                                                              (list "GOTO"))
                                                      "ATTN"))))
                org-clock-persist t
                org-clock-persist-file (expand-file-name "org-clock-save.el" emacs-d-userdata)
                org-clock-persist-query-resume nil
                org-clock-ask-before-exiting nil
                org-time-stamp-rounding-minutes '(1 1))

          ;; On Windows, we don't have dbus to show notifications; default to
          ;; `message' instead
          (when on-windows
            (setq org-show-notification-handler #'message)))

  :config (progn
            (org-clock-persistence-insinuate)

            (add-hook 'org-clock-in-hook #'db/org-mark-current-default-task)
            (add-hook 'org-clock-in-hook #'db/org-update-frame-title-with-current-clock)

            ;; Clock in default task if no other task is given
            (add-hook 'org-clock-out-hook #'db/ensure-running-clock 'append)

            ;; Communicate the currently clocked in task to the outside world
            (add-hook 'org-clock-in-hook #'db/save-current-org-task-to-file)))

;; Agenda

(use-package org-agenda
  :commands (org-agenda
             org-agenda-redo-all)
  :bind (:map org-agenda-mode-map
              ("i" . org-agenda-clock-in)
              ("v" . hydra-org-agenda-view/body)
              ([remap org-clock-goto] . db/org-clock-goto-first-open-checkbox)
              ([remap org-agenda-redo-all] . org-agenda-redo))
  :init (setq org-agenda-include-diary t
              org-agenda-span 1
              org-agenda-insert-diary-strategy 'top-level
              org-agenda-sorting-strategy '((agenda time-up priority-down user-defined-up category-keep)
                                            (todo priority-down user-defined-up category-keep)
                                            (tags priority-down user-defined-up category-keep)
                                            (search priority-down user-defined-up category-keep))
              org-agenda-cmp-user-defined #'db/org-cmp-remaining-effort
              org-agenda-window-setup 'current-window
              org-agenda-restore-windows-after-quit t
              org-agenda-compact-blocks nil
              org-agenda-todo-ignore-with-date nil
              org-agenda-todo-ignore-deadlines nil
              org-agenda-todo-ignore-scheduled nil
              org-agenda-todo-ignore-timestamp nil
              org-agenda-skip-deadline-if-done t
              org-agenda-skip-scheduled-if-done t
              org-agenda-skip-timestamp-if-done t
              org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
              org-agenda-tags-todo-honor-ignore-options t
              org-agenda-start-with-log-mode nil
              org-agenda-log-mode-items '(closed)
              org-agenda-remove-tags nil
              org-agenda-sticky nil
              org-agenda-inhibit-startup nil
              org-agenda-tags-todo-honor-ignore-options t
              org-agenda-dim-blocked-tasks nil
              org-enforce-todo-checkbox-dependencies t
              org-enforce-todo-dependencies          t
              org-agenda-use-time-grid t
              org-agenda-persistent-filter t
              org-agenda-search-headline-for-time nil
              org-agenda-search-view-always-boolean t

              org-agenda-clock-consistency-checks
              '(:max-duration 9999999
                :min-duration 0
                :max-gap 0
                :gap-ok-around nil
                :default-face ((:background "DarkRed") (:foreground "white"))
                :overlap-face nil :gap-face nil :no-end-time-face nil
                :long-face nil :short-face nil)

              org-agenda-clockreport-parameter-plist
              '(:link t :maxlevel 4 :compact t :narrow 60 :fileskip0 t)

              ;; Note: projects scheduled in the future are not considered
              ;; stuck, even though they are projects by itself; the rationale
              ;; here is that projects that have an explicit SCHEDULED entry
              ;; should not be considered before this date is due.
              org-stuck-projects
              '("+TODO=\"\"-DATE-HOLD-NOTE-WAIT-NOP-TOPIC-SOMEWHEN-TEMPLATE-SCHEDULED>=\"<+0d>\""
                ("CONT" "TODO" "READ" "WAIT" "GOTO" "DELG" "ATTN")
                ()
                "")

              org-agenda-prefix-format
              '((agenda . " %11s%?-12t%-4(db/org-remaining-effort-of-current-item) ")
                (todo . " %-8c%-4(db/org-remaining-effort-of-current-item) ")
                (tags . " %-8c%-4(db/org-remaining-effort-of-current-item) ")
                (search . " %-8c%-4(db/org-remaining-effort-of-current-item) "))

              org-agenda-custom-commands
              `(("A" "Main Agenda"
                     ((agenda
                       ""
                       ((org-agenda-entry-types '(:timestamp :sexp :scheduled :deadline))
                        (org-deadline-warning-days 0)))
                      (db/org-agenda-list-deadlines
                       ""
                       ((org-agenda-overriding-header "Deadlines")
                        (org-agenda-sorting-strategy '(deadline-up priority-down))
                        (org-deadline-warning-days 30)))
                      (tags-todo "TODO={CONT\\|ATTN}-HOLD-SOMEWHEN"
                                 ((org-agenda-overriding-header "Things to do next (Task shortlist and WIP, TODO ∈ {CONT,ATTN}, not scheduled, no near deadline)")
                                  (org-agenda-todo-ignore-scheduled 'all)
                                  (org-agenda-todo-ignore-timestamp 0)
                                  (org-agenda-todo-ignore-deadlines 'near)))
                      (tags-todo "TODO<>\"CONT\"-HOLD-SOMEWHEN-DATE-WAIT-TEMPLATE"
                                 ((org-agenda-overriding-header "Task Backlog (not WIP, not scheduled, no near deadline)")
                                  (org-agenda-todo-ignore-scheduled 'all)
                                  (org-agenda-todo-ignore-deadlines 'near)
                                  (org-tags-match-list-sublevels t)))))

                ("B" "Backlog"
                     ((tags-todo "-HOLD-SOMEWHEN-DATE-PERIODIC-TEMPLATE"
                                 ((org-agenda-overriding-header "Backlog: Actionable items (no periodic tasks; includes waiting-fors)")
                                  (org-tags-match-list-sublevels t)))
                      (tags "TODO=\"\"-HOLD-SOMEWHEN-DATE-PERIODIC-NOTE-NOP-TOPIC-TEMPLATE"
                            ((org-agenda-overriding-header "Backlog: Complex tasks (i.e., goals)")
                             (org-tags-match-list-sublevels t)))))

                ("C" "Checks"
                     ((tags "TODO=\"\"-HOLD-SOMEWHEN-DATE-PERIODIC-NOTE-NOP-TOPIC-TEMPLATE-GOAL"
                            ((org-agenda-overriding-header "Goals (i.e., complex tasks) not marked with GOAL")))

                      ;; Note: checking whether any SCHEDULED is behind a
                      ;; DEADLINE is not necessary, as items will appear on the
                      ;; deadline view anyway.

                      ))

                ("U" "Unsupervised (Waiting, Missed Appointments, Hold)"
                     ((tags "WAIT-TODO={DONE\\|CANC\\|MRGD}-HOLD-SOMEWHEN-SCHEDULED<>\"\""
                            ((org-agenda-overriding-header "Waiting For List")))
                      (tags-todo "TAGS={DATE}-TIMESTAMP>=\"<today>\""
                                 ((org-agenda-overriding-header "Missed appointments (DATEs with timestamp in the past)")))
                      (tags "TAGS={HOLD}-TODO={DONE\\|CANC\\|MRGD}-SOMEWHEN-SCHEDULED<>\"\""
                            ((org-agenda-overriding-header "Tasks on Hold")))))

                ("S" "Somewhen (Do if nothing else to do, i.e., personal backlog)"
                     ((tags "TAGS={SOMEWHEN}+TODO=\"\"-NOP-TOPIC-PERIODIC-DATE-SCHEDULED>=\"<today>\""
                            ((org-agenda-overriding-header "Open Tasks to do SOMEWHEN (no TODO keyword, no PERIODIC, no DATE, no now or future SCHEDULED)")))
                      (tags-todo "SOMEWHEN-HOLD"
                                 ((org-agenda-overriding-header "Things To Do SOMEWHEN")
                                  (org-agenda-todo-ignore-with-date t)
                                  (org-tags-match-list-sublevels nil)))))

                ("P" "Current Projects and Topics"
                     ((stuck ""
                             ((org-agenda-overriding-header "Stuck Complex Tasks")))
                      (tags "TAGS={TOPIC}-TODO={DONE\\|CANC\\|MRGD}-SOMEWHEN-SCHEDULED>=\"<today>\"-HOLD-WAIT"
                            ((org-agenda-overriding-header "Topics (items directly tagged with TOPIC)")))
                      (tags "TAGS={NOTE}-TODO={CANC\\|DONE\\|MRGD}-HOLD-NOP-SOMEWHEN-SCHEDULED>=\"<today>\""
                            ((org-agenda-overriding-header "Project Notes (items explicitly tagged with NOTE but not NOP)")))
                      (tags "TAGS={PERIODIC}-TODO={DONE\\|CANC\\|MRGD}-HOLD-SCHEDULED>=\"<today>\"-HOLD-WAIT"
                            ((org-agenda-overriding-header "Periodic Projects (PERIODIC, not scheduled in the future, not done, not on hold)")))))

                ("W" "Weekly Review"
                     ((agenda ""
                              ((org-agenda-span 7)
                               (org-agenda-dim-blocked-tasks nil)
                               (org-agenda-skip-deadline-prewarning-if-scheduled t)))))))

  :config (progn
            ;; Avoid important buffers to end up in `org-agenda-new-buffers’ by
            ;; opening them manually
            (mapc #'find-file-noselect org-agenda-files)

            ;; Check that all expected agenda files are being displayed.
            (advice-add 'org-agenda
                        :before #'db/check-special-org-files-in-agenda)

            (add-hook 'org-agenda-mode-hook #'hl-line-mode 'append)

            ;; Show daily efforts directly in the agenda
            (add-hook 'org-agenda-finalize-hook #'db/org-agenda-insert-efforts)

            ;; Prominently display active filters on top of agenda view; also
            ;; update current agenda view when updating filters to make sure our
            ;; display is always correct.
            (add-hook 'org-agenda-finalize-hook #'db/org-agenda-insert-active-filters)
            (add-hook 'org-agenda-filter-hook #'org-agenda-redo-all)

            (define-advice org-agenda-redo (:around
                                            (actual-agenda-redo &rest r)
                                            inhibit-recentering)
              "Try to avoid recentering the window when redoing the Org agenda buffer."
              (let ((old-recenter (symbol-function 'recenter)))
                (fset 'recenter 'identity)
                (unwind-protect
                     (apply actual-agenda-redo r)
                  (fset 'recenter old-recenter))))))

;; Capturing

(use-package org-capture
  :commands (org-capture)
  :init (setq org-capture-use-agenda-date nil
              org-bookmark-names-plist nil
              org-capture-templates
              `(("t" "Simple Task"
                     entry
                     (file db/org-default-refile-file)
                     ,(concat "* TODO [#B] %^{What}\n"
                              ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                              "\nVia %(with-temp-buffer (db/org-add-link-to-current-clock) (string-trim (buffer-string)))."
                              "%?")
                     :empty-lines-before 1
                     :empty-lines-after 1)
                ("g" "Record new goal"
                     entry
                     (file db/org-default-refile-file)
                     ,(concat "* %^{Description} (%^{Ticket Number}) :GOAL:\n"
                              ":PROPERTIES:\n:CREATED: %U\n:END:"
                              "%?"))
                ("h" "Headline (generic Org item)"
                     entry
                     (file db/org-default-refile-file)
                     ,(concat "* [#B] %^{What}\n"
                              ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                              "%a"
                              "%?")
                     :empty-lines-before 1
                     :empty-lines-after 1)
                ("n" "Note"
                     entry
                     (file db/org-default-refile-file)
                     "* Note: %^{About} :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:%?"
                     :empty-lines-before 1
                     :empty-lines-after 1)
                ("d" "Date"
                     entry
                     (file db/org-default-refile-file)
                     "* GOTO [#B] %^{What} :DATE:\n%^{When}t\n%a%?"
                     :empty-lines-before 1
                     :empty-lines-after 1)
                ("i" "Interruptions"
                     entry
                     (file db/org-default-refile-file)
                     ,(concat "* DONE [#B] %^{What}\nCLOSED: %U\n"
                              ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                              "\nInterrupted %(with-temp-buffer (db/org-add-link-to-current-clock) (string-trim (buffer-string)))."
                              "%?")
                     :clock-in t
                     :clock-resume t
                     :empty-lines-before 1
                     :empty-lines-after 1)
                ("r" "Respond"
                     entry
                     (file db/org-default-refile-file)
                     ,(concat "* TODO [#B] Reply: %:subject (%:from) :EMAIL:\n"
                              ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                              "\n%a%?")
                     :empty-lines-before 1
                     :empty-lines-after 1)))
  :config (progn
            ;; disable usage of helm for `org-capture'
            (with-eval-after-load 'helm-mode
              (defvar helm-completing-read-handlers-alist) ; for the byte compiler
              (add-to-list 'helm-completing-read-handlers-alist
                           '(org-capture . nil)))))

;; Babel

(use-package ob-core
  :init (setq org-export-use-babel t
              org-babel-default-header-args '((:session . "none")
                                              (:results . "output drawer replace")
                                              (:exports . "both")
                                              (:cache . "no")
                                              (:noweb . "no")
                                              (:hlines . "no")
                                              (:tangle . "no")
                                              (:eval . "never-export")))
  :config (progn
            ;; Let's disable evaluation of some text-only modes.
            (dolist (mode '("json" "markdown" "yaml" "textile" "text"))
              (let ((var-name (intern (format "org-babel-default-header-args:%s" mode))))
                (if (boundp var-name)
                    (add-to-list var-name '(:eval . "never"))
                  (defvar-1 var-name '((:eval . "never"))))))))

(use-package ob-sql
  :config (progn

            ;; XXX: maybe merge this into the advice for `org-babel-execute:sql'?
            (define-advice org-babel-sql-dbstring-oracle (:around
                                                          (orig-fun host port user password database)
                                                          ask-for-password)
              "Ask for PASSWORD if not given, and call ORIG-FUN with arguments afterwards."
              (cond
                ((not (or (and user database host port)
                          (and user database)))
                 (user-error "Insufficient login credentials given, aborting"))
                (password
                 (funcall orig-fun host port user password database))
                (t
                 (funcall orig-fun
                          host port user
                          (password-read (format "Password for %s@%s: " user database))
                          database))))))

;; Exporting

(use-package ox-icalendar
  :commands (org-icalendar-combine-agenda-files)
  :init (setq org-icalendar-include-body nil
              org-icalendar-store-UID t
              org-icalendar-use-deadline nil
              org-icalendar-use-scheduled nil
              org-icalendar-include-todo nil
              org-icalendar-exclude-tags '("NO_EXPORT")))

(use-package ox
  :init (setq org-export-with-broken-links 'mark
              org-export-with-sub-superscripts '{}
              org-export-with-author nil
              org-export-with-date nil
              org-export-with-toc nil
              org-export-with-archived-trees nil
              org-export-with-tags t
              org-export-with-priority nil
              org-export-with-creator nil
              org-export-with-entities t
              org-export-with-special-strings t
              org-export-with-todo-keywords nil
              org-export-exclude-tags '("NO_EXPORT"))
  :config (with-demoted-errors "Cannot load package: %s"
            (require 'ox-md)
            (require 'ox-pandoc)))

(use-package ox-html
  :init (setq org-html-postamble nil))

(use-package ox-pandoc
  :init (setq org-pandoc-options-for-docx '((standalone . t))))

;; Extra Org packages

(use-package org-tree-slide
  :commands (org-tree-slide-mode)
  ;; Configuration from https://protesilaos.com/dotemacs/
  :init (setq org-tree-slide-breadcrumbs " → "
              org-tree-slide-header t
              org-tree-slide-slide-in-effect nil
              org-tree-slide-heading-emphasis nil
              org-tree-slide-cursor-init t
              org-tree-slide-modeline-display nil
              org-tree-slide-skip-done nil
              org-tree-slide-skip-comments t
              org-tree-slide-fold-subtrees-skipped t
              org-tree-slide-skip-outline-level 8
              org-tree-slide-never-touch-face t
              org-tree-slide-activate-message (propertize "Presentation mode ON" 'face 'success)
              org-tree-slide-deactivate-message (propertize "Presentation mode OFF" 'face 'error))
  :bind (:map org-tree-slide-mode-map
              ("<C-down>" . org-tree-slide-display-header-toggle)
              ("<C-right>" . org-tree-slide-move-next-tree)
              ("<C-left>" . org-tree-slide-move-previous-tree))
  :hook ((org-tree-slide-play . turn-on-olivetti-mode)
         (org-tree-slide-stop . turn-off-olivetti-mode)))

(use-package org-roam
  :init (progn
          (add-to-list 'display-buffer-alist
                       '("\\*org-roam\\*"
                         (display-buffer-in-side-window)
                         (side . right)
                         (slot . 0)
                         (window-width . 0.33)
                         (window-parameters . ((no-other-window . t)
                                               (no-delete-other-windows . t))))))
  :commands (org-roam-node-insert
             org-roam-node-find
             org-roam-capture
             org-roam-buffer-toggle)
  :custom ((org-roam-directory (expand-file-name "~/Documents/zettelkasten/"))
           (org-roam-db-location (expand-file-name "~/Documents/zettelkasten/org-roam.db"))
           (org-roam-completion-everywhere t)
           (org-roam-mode-sections (list #'org-roam-backlinks-section
                                         #'org-roam-reflinks-section
                                         #'org-roam-unlinked-references-section)))
  :config (progn
            (org-roam-db-autosync-mode)

            (define-advice org-roam-buffer-render-contents
                (:around (orig-func) show-buffer-in-selected-window)
              "Show Org roam buffer in selected window before updating its content.

See https://github.com/org-roam/org-roam/issues/1586, and in particular
https://github.com/org-roam/org-roam/issues/1586#issuecomment-1412250226.
Note that this workaround is incomplete, as explained in this comment."
              (let ((org-roam-buffer-window (display-buffer (current-buffer))))
                ;; When we cannot display the buffer, there is also no need to
                ;; update it, no?
                (when org-roam-buffer-window
                  (with-selected-window org-roam-buffer-window
                    (funcall orig-func)))))))

(with-eval-after-load 'org
  ;; I would like to include this keybinding in the :bind definition of `org'
  ;; above, but doing so seems to attach an autoload to `org-roam-buffer-toggle'
  ;; on org.el; the latter inclusion of `org-roam-buffer-toggle' in the
  ;; :commands list of `org-roam' does not seem to overwrite this.
  (bind-key "C-c n l" #'org-roam-buffer-toggle org-mode-map))


;; * General Programming

;; Configuration that pertains to programming in general, without referring to
;; any programming language in particular.

(use-package ediff
  :init (setq ediff-diff-options "-w"
              ediff-window-setup-function 'ediff-setup-windows-plain
              ediff-split-window-function 'split-window-horizontally
              ediff-show-clashes-only t)
  :config (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package eldoc
  :commands (global-eldoc-mode
             turn-on-eldoc-mode)
  :diminish eldoc-mode)

(use-package eglot
  :ensure nil
  ;; Inspired by https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot.
  :init (setq eglot-autoshutdown t
              eglot-extend-to-xref nil
              eglot-stay-out-of '(yasnippet)))

(use-package flycheck
  :ensure t
  :commands (global-flycheck-mode flycheck-mode)
  :init (setq flycheck-emacs-lisp-load-path 'inherit
              ;; Hack: inherit `byte-compile-warnings' setting in Emacs
              ;; subprocess; this value is only set once upon starting Emacs, so
              ;; make sure to manually update this setting when updating
              ;; `byte-compile-warnings'.
              flycheck-emacs-args `("-Q"
                                    "--batch"
                                    "--eval" ,(message "(setq byte-compile-warnings (quote %s))"
                                                       byte-compile-warnings))))

(use-package git-commit
  :commands (global-git-commit-mode)
  :init (setq git-commit-style-convention-checks '(non-empty-second-line
                                                   overlong-summary-line))
  :config (add-hook 'git-commit-setup-hook
                    #'(lambda ()
                        (setq fill-column 72))))

(use-package highlight-indentation
  :commands highlight-indentation-mode)

(use-package iedit
  :ensure t
  :commands (iedit-mode))

;; See https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/
;; for where some of the configuration for `lsp-mode' is coming from.
(use-package lsp-mode
  :commands (lsp-mode)
  :ensure nil
  :init (setq lsp-keymap-prefix "C-c C-l"
              lsp-session-file (expand-file-name ".lsp-session" emacs-d-userdata)
              lsp-log-io t
              lsp-keep-workspace-alive nil
              lsp-idle-delay 0.5
              lsp-auto-configure t
              lsp-headerline-breadcrumb-enable nil
              lsp-signature-doc-lines 1)
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-completion-mode))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure nil
  :commands (lsp-ui-mode))

(use-package magit
  :ensure t
  :commands (magit-status
             magit-list-repositories)

  :init (progn
          (setq magit-diff-refine-hunk nil
                magit-commit-show-diff nil
                magit-repository-directories '(("~/" . 0)
                                               ("~/.emacs.d" . 0)
                                               ("~/Documents/" . 3)))

          (when on-windows
            ;; Experimental: on Windows, do not refresh magit-status-buffers
            ;; that are not selected, to increase performance.
            (setq magit-refresh-status-buffer nil)))

  :config (progn
            (when (fboundp 'global-magit-file-mode)
              (global-magit-file-mode -1))
            (global-git-commit-mode 1)))

(use-package page-break-lines
  :pin "melpa-stable"
  :commands (page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package transient
  :init (setq transient-levels-file (expand-file-name "transient/levels.el" emacs-d-userdata)
              transient-values-file (expand-file-name "transient/values.el" emacs-d-userdata)
              transient-history-file (expand-file-name "transient/history.el" emacs-d-userdata)))


;; * Mail

(use-package db-mail
  :commands (db/smtpmail-send-it
             db/public-key
             db/encryption-possible-p
             db/message-recipients
             db/signencrypt-message-when-possible
             db/gnus-save-newsrc-with-whitespace-1
             db/gnus-summary-open-Link
             db/gnus-html-mime-part-to-org
             db/gnus-demon-scan-news-on-level-2))

(use-package bbdb
  :ensure t
  :commands (bbdb-search-name
             bbdb-initialize
             bbdb-mua-auto-update-init
             bbdb-save)
  :hook ((message-setup . bbdb-mail-aliases)
         (mail-setup . bbdb-mail-aliases))
  :init (setq bbdb-completion-display-record nil
              bbdb-complete-mail-allow-cycling t
              bbdb-mua-auto-action 'query
              bbdb-default-country "Germany")
  :config (run-with-timer 0 3600 #'bbdb-save))

;; Gnus package configuration

(use-package gnus
  :commands (gnus)
  :config (progn
            (require 'db-mail)

            (with-demoted-errors "Setting up BBDB failed: %s"
              (bbdb-initialize 'gnus 'message)
              (bbdb-mua-auto-update-init 'message))

            ;; Ensure that whenever we compose new mail, it will use the correct
            ;; posting style.  This is ensured by setting ARG of
            ;; `gnus-group-mail’ to 1 to let it query the user for a group.
            (defadvice gnus-group-mail (before inhibit-no-argument activate)
              (unless (ad-get-arg 0)
                (ad-set-arg 0 1)))

            (remove-hook 'gnus-mark-article-hook
                         'gnus-summary-mark-read-and-unread-as-read)
            (add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)

            ;; Quit Gnus gracefully when exiting Emacs
            (add-hook 'kill-emacs-hook #'(lambda ()
                                           (interactive)
                                           (when (get-buffer "*Group*")
                                             (gnus-group-exit))))

            ;; Don’t quit summary buffer when pressing `q’
            (bind-key "q" #'gnus-summary-expand-window gnus-article-mode-map)

            ;; Show topics in group buffer
            (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

            ;; We need to do some magic as otherwise the agent does not delete
            ;; articles from its .overview when we move them around.  Thus we
            ;; mark articles as expireable when they have been moved to another
            ;; group.
            (defadvice gnus-summary-move-article (around
                                                  no-cancel-mark
                                                  (&optional n to-newsgroup
                                                             select-method action)
                                                  activate)
              (let ((articles (gnus-summary-work-articles n))
                    (return   ad-do-it))
                (when (or (null action)
                          (eq action 'move))
                  (dolist (article articles)
                    (gnus-summary-mark-article article gnus-expirable-mark)))
                return))

            ;; Increase score of group after reading it
            (add-hook 'gnus-summary-exit-hook
                      'gnus-summary-bubble-group)

            ;; Use Gnus’ registry; doing this too early conflicts with `gnus'
            ;; calling `gnus-shutdown', which in turn calls
            ;; `gnus-registry-clear', leaving us with an empty registry upon
            ;; startup.  So let's call this initialization right before startup,
            ;; that should be fine.
            (add-hook 'gnus-before-startup-hook
                      #'gnus-registry-initialize)

            ;; Automatic encryption if all necessary keys are present
            (add-hook 'gnus-message-setup-hook
                      #'db/signencrypt-message-when-possible)

            ;; Do some pretty printing before saving the newsrc file
            (add-hook 'gnus-save-quick-newsrc-hook
                      #'db/gnus-save-newsrc-with-whitespace-1)

            ;; Automatically scan for new news
            (gnus-demon-add-handler 'db/gnus-demon-scan-news-on-level-2 5 5)

            ;; Automatically expire groups on idle
            (gnus-demon-add-handler 'gnus-group-expire-all-groups 10 5)

            ;; Visit group under point and immediately close it; this updates
            ;; gnus’ registry as a side-effect
            (bind-key "v u"
                      #'(lambda ()
                          (interactive)
                          (save-mark-and-excursion
                            (when (gnus-topic-select-group)
                              (gnus-summary-exit))))
                      gnus-group-mode-map)

            ;; Toggle visibility of News group
            (bind-key "v c"
                      #'(lambda ()
                          (interactive)
                          (save-mark-and-excursion
                            (gnus-topic-jump-to-topic "News")
                            (gnus-topic-read-group)))
                      gnus-group-mode-map)

            (bind-key "C-<return>" #'db/gnus-summary-open-Link gnus-summary-mode-map)
            (bind-key "C-<return>" #'db/gnus-summary-open-Link gnus-article-mode-map)))

(use-package gnus-topic
  :commands (gnus-topic-select-group
             gnus-topic-jump-to-topic
             gnus-topic-read-group))

(use-package gnus-demon
  :autoload (gnus-demon-add-handler))

(use-package gnus-group
  :commands (gnus-group-exit))

;; General Gnus configuration

(use-package nntp)
(use-package nnml)
(use-package message)
(use-package gnus-msg)
(use-package gnus-async)

(setq gnus-init-file (expand-file-name "gnus.el" emacs-d)
      gnus-home-directory (expand-file-name "~/.config/gnus-news")
      gnus-directory gnus-home-directory
      gnus-kill-files-directory gnus-directory
      gnus-startup-file (expand-file-name "gnus-newsrc" emacs-d-userdata)
      gnus-cache-directory (expand-file-name "cache/" gnus-directory)
      gnus-verbose 6
      gnus-dbus-close-on-sleep t

      message-directory (expand-file-name "mail/" gnus-directory)
      nnmail-message-id-cache-file (expand-file-name ".nnmail-cache" gnus-directory)
      nnml-directory message-directory
      mail-sources '((file))
      mail-source-delete-incoming t
      nntp-nov-is-evil t
      nntp-connection-timeout nil
      gnus-asynchronous t
      gnus-save-killed-list nil
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-check-new-newsgroups nil
      gnus-use-cache 'passive
      gnus-read-active-file 'some
      gnus-subscribe-newsgroup-method 'gnus-subscribe-killed
      gnus-group-list-inactive-groups t
      gnus-suppress-duplicates nil
      gnus-large-newsgroup 500
      nnmail-expiry-wait 7
      nnmail-cache-accepted-message-ids t
      gnus-use-full-window nil
      gnus-always-force-window-configuration t
      gnus-select-method '(nnnil "")
      gnus-refer-article-method 'current

      message-citation-line-function
      #'(lambda ()
          (when message-reply-headers
            (insert "ghItlhpu' "
                    (mail-header-from message-reply-headers)
                    ":")
            (newline))))

;; Gnus Appearence

(use-package gnus-sum
  :commands (gnus-summary-exit
             gnus-summary-expand-window)
  :init (setq gnus-group-line-format "%S%p%P%5y(%2i):%B%(%s:%G%)\n"
              gnus-build-sparse-threads 'some
              gnus-summary-next-group-on-exit nil
              gnus-fetch-old-headers nil
              gnus-auto-select-first nil
              gnus-auto-select-next nil
              gnus-summary-line-format "%U%O%R%6k %(%&user-date;  %-13,13f  %B%s%)\n"
              gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
              gnus-subthread-sort-functions '(gnus-thread-sort-by-date)
              gnus-thread-hide-subtree t
              gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
              gnus-sum-thread-tree-indent "  "
              gnus-sum-thread-tree-root "● "
              gnus-sum-thread-tree-false-root "◎ "
              gnus-sum-thread-tree-single-indent "◯ "
              gnus-sum-thread-tree-single-leaf "╰► "
              gnus-sum-thread-tree-leaf-with-other "├► "
              gnus-sum-thread-tree-vertical "│"
              gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references

              ;; New mark symbols (seen here:
              ;; `https://github.com/cofi/dotfiles/blob/master/gnus.el')
              gnus-ancient-mark ?✓
              ;; gnus-cached-mark ?☍
              gnus-canceled-mark ?↗
              gnus-del-mark ?✗
              ;; gnus-dormant-mark ?⚐
              gnus-expirable-mark ?♻
              gnus-forwarded-mark ?↪
              ;; gnus-killed-mark ?☠
              ;; gnus-process-mark ?⚙
              gnus-read-mark ?✓
              gnus-recent-mark ?✩
              gnus-replied-mark ?↺
              gnus-unread-mark ?✉
              ;; gnus-unseen-mark ?★
              ;; gnus-ticked-mark ?⚑
              ))

(use-package gnus-art
  :init (setq gnus-ignored-mime-types '("text/x-vcard")
              gnus-inhibit-mime-unbuttonizing nil
              gnus-buttonized-mime-types '("multipart/signed" "multipart/encrypted")
              gnus-inhibit-images t
              gnus-blocked-images ".*"
              gnus-visible-headers (regexp-opt '("From:"
                                                 "Newsgroups:"
                                                 "Subject:"
                                                 "Date:"
                                                 "Followup-To:"
                                                 "Reply-To:"
                                                 "Organization:"
                                                 "Summary:"
                                                 "Keywords:"
                                                 "Mail-Copies-To:"
                                                 "To:"
                                                 "Cc:"
                                                 "BCC:"
                                                 "X-Newsreader:"
                                                 "X-Mailer:"
                                                 "X-Sent:"
                                                 "Posted-To:"
                                                 "Mail-Copies-To:"
                                                 "Apparently-To:"
                                                 "Gnus-Warning:"
                                                 "Resent-From:"
                                                 "gpg-key-ID:"
                                                 "fingerprint:"
                                                 "X-Jabber-ID:"
                                                 "User-Agent:"))
              gnus-treat-hide-boring-headers 'head
              gnus-treat-strip-multiple-blank-lines nil
              gnus-treat-display-smileys t
              gnus-treat-emphasize 'head
              gnus-treat-unsplit-urls t))

;; Adaptive Scoring

(use-package gnus-score
  :init (setq gnus-use-scoring t
              gnus-use-adaptive-scoring '(word line)
              gnus-summary-mark-below nil
              gnus-adaptive-word-length-limit 5
              gnus-adaptive-word-no-group-words t
              gnus-default-adaptive-score-alist
              '((gnus-unread-mark)
                (gnus-ticked-mark (from 4))
                (gnus-dormant-mark (from 5))
                (gnus-del-mark (from -4) (subject -1))
                (gnus-read-mark (from 4) (subject 2))
                (gnus-expirable-mark (from -1) (subject -1))
                (gnus-killed-mark (from -1) (subject -3))
                (gnus-kill-file-mark)
                (gnus-ancient-mark)
                (gnus-low-score-mark)
                (gnus-catchup-mark (from -1) (subject -1)))
              gnus-summary-mark-below nil))

;; Gnus Registry

(use-package gnus-registry
  :commands (gnus-registry-split-fancy-with-parent
             gnus-registry-initialize)
  :init (setq gnus-registry-split-strategy 'majority
              gnus-registry-ignored-groups '(("^nntp" t)
                                             ("^nnfolder" t)
                                             ("^nnir" t)
                                             ("^nnmaildir" t)
                                             ("INBOX$" t))
              gnus-registry-max-entries 5000
              gnus-registry-track-extra '(sender subject recipient)
              gnus-registry-cache-file (expand-file-name "gnus.registry.eioioi"
                                                         emacs-d)))

;; MIME decoding

(use-package mm-decode
  :init (setq mm-text-html-renderer 'shr
              mm-discouraged-alternatives '("text/richtext")
              mm-decrypt-option 'known
              mm-verify-option 'known)
  :config (progn
            (setq mm-automatic-display (-difference mm-automatic-display
                                                    '("text/enriched"
                                                      "text/richtext")))

            ;; Automatically show PGP data inline
            (add-to-list 'mm-inlined-types "application/pgp$")
            (add-to-list 'mm-inline-media-tests
                         '("application/pgp$" mm-inline-text identity))
            (add-to-list 'mm-automatic-display "application/pgp$")

            ;; When copying MIME buffers, we are looking for the start of the
            ;; header by searching ^\n; however, if we received mail from
            ;; Outlook, there's an ^\r\n seperating header and body, which is
            ;; not found by `mm-copy-to-buffer', leaving the target buffer empty
            ;; and the mail as well.  Redefining `mm-copy-to-buffer' to also
            ;; search for ^\r\n might help.

            (define-advice mm-copy-to-buffer (:around (orig-fun) also-consider-crlf)
              "Overwrite `mm-copy-to-buffer' to allow for CRLF in addition to LF."
              (ignore orig-fun)
              (let ((obuf (current-buffer))
                    (mb enable-multibyte-characters)
                    beg)
                (goto-char (point-min))
                ;; The following regex has been extended by \r? .
                (search-forward-regexp "^\r?\n" nil 'move) ;; There might be no body.
                (setq beg (point))
                (with-current-buffer
                    (generate-new-buffer " *mm*")
                  ;; Preserve the data's unibyteness (for url-insert-file-contents).
                  (set-buffer-multibyte mb)
                  (insert-buffer-substring obuf beg)
                  (current-buffer))))))

(setq message-forward-as-mime nil)

;; MIME creation; signing, and encryption

(setq mml-attach-file-at-the-end t)

(use-package mm-encode
  :init (setq mm-encrypt-option nil
              mm-sign-option nil))

(setq mml-secure-openpgp-encrypt-to-self t
      mml-secure-smime-encrypt-to-self t
      mml2015-display-key-image nil
      gnus-message-replysign t
      gnus-message-replyencrypt t
      gnus-message-replysignencrypted t
      mml-secure-cache-passphrase nil
      mml-secure-openpgp-sign-with-sender t
      mml-secure-smime-sign-with-sender t)

(use-package mml-smime
  :init (setq mml-smime-use 'epg)
  :config (progn
            ;; Outlook seems to expect \r\n in PKCS#7 encrypted containers, but
            ;; Gnus is only sending \n; so let's artificially replace \n by \r\n
            ;; after, well, signing?  But only if we are encrypting with S/MIME,
            ;; as otherwise the search for \n\n in `message-encode-message-body'
            ;; will break.  However, I think this is not the right way to do it
            ;; … isn't there some way to specify the encoding of the the
            ;; relevant MML buffers to be some nasty cp1252 or something?

            ;; A previous version of this worked, but the following has not been
            ;; tested with Outlook proper.

            (define-advice mml-smime-epg-sign (:after (cont) add-crlf-when-pkcs7)
              "If CONT signifies encryption with smime, replace all \n with \r\n."
              (when (and (eq (car cont) 'part)
                         (string= "smime" (or (cdr (assq 'encrypt cont)) "")))
                (db/convert-lf-to-crlf-in-buffer)))))

;; Archiving

;; We store messages in the current group, so there is
;; no need to use Gnus’ archiving method

(setq gnus-message-archive-method nil
      gnus-update-message-archive-method t
      gnus-message-archive-group nil
      gnus-gcc-mark-as-read t)

;; Searching

(use-package gnus-search
  :init (setq gnus-search-default-engines '((nnimap . gnus-search-imap))
              gnus-search-use-parsed-queries t))

;; Agents

(use-package gnus-agent
  :init (setq gnus-agent-mark-unread-after-downloaded nil
              gnus-agent-consider-all-articles t
              gnus-agent-synchronize-flags t
              gnus-agent-go-online t))

;; Sending mail

(setq message-send-mail-function #'db/smtpmail-send-it
      send-mail-function #'db/smtpmail-send-it
      mail-user-agent 'gnus-user-agent)

(use-package smtpmail
  :init (setq smtpmail-stream-type 'starttls
              smtpmail-smtp-service 587
              smtpmail-debug-info t))


;; * Crypto

(use-package nsm
  :init (setq network-security-level 'high
              nsm-save-host-names t
              nsm-settings-file (expand-file-name "network-security.data" emacs-d-userdata))
  :config (advice-add 'nsm-write-settings
                      :before #'db/sort-nsm-permanent-settings))

(use-package gnutls
  :init (setq gnutls-log-level 0        ; too noisy otherwise
              gnutls-min-prime-bits 1024
              gnutls-verify-error t))

(use-package epg
  :init (setq epg-debug t
              epg-gpg-program "gpg"))

(use-package org-password-manager
  :commands (org-password-manager-get-username
             org-password-manager-get-password))


;; * Appearance

(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil
              font-lock-maximum-decoration '((t . t)))

(setq mode-line-format '((ace-window-display-mode
                          (:eval
                           (window-parameter
                            (selected-window)
                            'ace-window-path)))
                         "%e"
                         mode-line-front-space
                         mode-line-position
                         mode-line-mule-info
                         mode-line-client
                         mode-line-modified
                         mode-line-remote
                         mode-line-buffer-identification
                         mode-line-modes
                         (vc-mode vc-mode)
                         mode-line-misc-info
                         mode-line-end-spaces))

(use-package solarized-theme
  :ensure t
  :init (setq solarized-use-less-bold t
              solarized-emphasize-indicators t
              solarized-use-variable-pitch nil))

(use-package solarized-dark-theme
  :defer t
  :config (progn
            (setq custom--inhibit-theme-enable nil)))

(use-package smart-mode-line
  :ensure t
  :init (setq sml/mode-width 'full
              sml/name-width 30)
  :commands (sml/setup))

(use-package moody
  :ensure t
  :commands (moody-replace-mode-line-buffer-identification
             moody-replace-vc-mode))

(use-package smiley
  :init (setq smiley-style 'emoji))


;; * File Handling

(setq large-file-warning-threshold 10000000
      delete-by-moving-to-trash t)

;; Backups and Autosave
(defvar backup-dir (expand-file-name "ebackup/" emacs-d))
(defvar autosave-dir (expand-file-name "eautosave/" emacs-d))
(setq make-backup-files               t
      backup-directory-alist          (list (cons ".*" backup-dir))
      auto-save-list-file-prefix      autosave-dir
      auto-save-file-name-transforms  `((".*" ,autosave-dir t))
      version-control                 t
      kept-old-versions               2
      kept-new-versions               4
      delete-old-versions             t
      vc-make-backup-files            t)

(use-package dired-aux
  :defines (dired-create-destination-dirs
            dired-vc-rename-file))

(use-package dired-x
  :defines (dired-omit-files)
  :init (setq dired-x-hands-off-my-keys t))

(use-package wdired
  :defines (wdired-create-parent-directories
            wdired-allow-to-change-permissions))

(use-package dired
  :commands (dired-jump
             dired-jump-other-window)
  :bind (:map dired-mode-map
              ("e" . db/dired-ediff-files)
              ("z" . db/dired-get-size)
              ([remap beginning-of-buffer] . db/dired-back-to-top)
              ([remap end-of-buffer] . db/dired-jump-to-bottom)
              ("<f1>" . nil)
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle))
  :init (progn
          (setq dired-dwim-target t
                dired-listing-switches "-ahlvF"
                dired-ls-F-marks-symlinks t
                dired-hide-details-hide-information-lines t
                dired-hide-details-hide-symlink-targets t
                dired-recursive-copies 'top
                dired-recursive-deletes 'top
                dired-create-destination-dirs 'ask
                dired-vc-rename-file t
                dired-kill-when-opening-new-dired-buffer nil
                dired-maybe-use-globstar t
                dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"
                wdired-create-parent-directories t
                wdired-allow-to-change-permissions t
                dired-isearch-filenames 'dwim
                dired-auto-revert-buffer t
                dired-clean-confirm-killing-deleted-buffers t
                dired-clean-up-buffers-too t)

          (setq dired-guess-shell-alist-user
                '(("\\.pdf\\'" "evince")
                  ("\\.ps\\'" "evince")
                  ("\\.\\(?:djvu\\|eps\\)\\'" "evince")
                  ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "eog")
                  ("\\.\\(?:xcf\\)\\'" "gimp")
                  ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|webm\\)\\(?:\\.part\\)?\\'"
                   "vlc")
                  ("\\.\\(?:mp3\\|flac\\|ogg\\)\\'" "mplayer")
                  ("\\.docx?\\'" "loffice"))))
  :config (progn
            (put 'dired-find-alternate-file 'disabled nil)

            (require 'dired-x)
            (require 'dired-aux)
            (require 'wdired)

            (with-demoted-errors "Non-Fatal Errors (dired-open): %s"
              (require 'dired-open))

            (if (eq system-type 'windows-nt)
                (with-demoted-errors "Non-Fatal Error (w32-browser): %s"
                  (bind-key "M-RET" #'dired-w32-browser dired-mode-map)
                  (bind-key "<C-return>" #'dired-w32explore dired-mode-map))
                (bind-key "M-RET" #'dired-open-xdg dired-mode-map))

            (with-demoted-errors "Non-Fatal Errors (dired-recent): %s"
              (dired-recent-mode +1))

            ;; Gnus support in dired
            (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

            ;; Highlight current line more prominently
            (add-hook 'dired-mode-hook 'hl-line-mode)

            ;; omitting files
            (add-hook 'dired-mode-hook 'dired-omit-mode)
            (add-hook 'dired-mode-hook 'dired-hide-details-mode)
            (dolist (extension '(".out" ".synctex.gz" ".thm"))
              (add-to-list 'dired-latex-unclean-extensions extension))))

(use-package dired-open
  :ensure t
  :commands (dired-open-xdg
             dired-open-guess-shell-alist
             dired-open-call-function-by-extension)
  :init (progn
          (unless (eq system-type 'gnu/linux)
            (setq dired-open-use-nohup nil))
          (setq dired-open-extensions-elisp '(("html" . eww-open-file))))
  :config (progn
            (add-to-list 'dired-open-functions
                         #'dired-open-guess-shell-alist)
            (add-to-list 'dired-open-functions
                         #'dired-open-call-function-by-extension)))

(use-package dired-recent
  :ensure t
  :init (setq dired-recent-max-directories nil)
  :commands (dired-recent-mode
             dired-recent-open))

(use-package dired-subtree
  :commands (dired-subtree-toggle))

(use-package ffap
  ;; Inhibit Emacs from pinging hostnames; see
  ;; https://www.n16f.net/blog/investigating-a-ffap-issue-in-emacs/
  :init (setq ffap-machine-p-local 'accept
              ffap-machine-p-known 'accept
              ffap-machine-p-unknown 'reject))

(use-package find-dired
  :commands (find-dired)
  :init (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(use-package mailcap
  :config (progn
            ;; Remove doc-view so pdf will open with default viewer
            (setcdr
             (assoc "application" mailcap-mime-data)
             (remove '("pdf"
                       (viewer . doc-view-mode)
                       (type . "application/pdf")
                       (test eq window-system 'x))
                     (cdr (assoc "application" mailcap-mime-data))))))

(use-package gnus-dired
  :commands (turn-on-gnus-dired-mode))

(use-package tramp
  :init (setq tramp-default-method (if on-windows "pscp" "scp")
              tramp-completion-use-auth-sources nil))

(use-package trashed
  ;; A simple dired-like interface to the system trash bin
  ;; Configuration taken from https://protesilaos.com/dotemacs
  :init (setq trashed-action-confirmer 'y-or-n-p
              trashed-use-header-line t
              trashed-sort-key '("Date deleted" . t)
              trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package w32-browser
  :commands (dired-w32-browser
             dired-w32explore))


;; * Completion

(setq suggest-key-bindings t
      extended-command-suggest-shorter t
      completions-detailed t
      completion-cycle-threshold nil
      completion-styles '(basic orderless)
      completion-category-defaults nil
      ;; Via https://protesilaos.com/emacs/dotemacs, with additional changes
      completion-category-overrides '((file (styles . (basic partial-completion orderless)))
                                      (buffer (styles . (orderless)))
                                      (bookmark (styles . (orderless)))
                                      (imenu (styles . (basic substring orderless)))
                                      (kill-ring (styles . (emacs22 orderless)))))

(use-package helm
  :ensure t
  :diminish helm-mode
  :autoload (helm-execute-persistent-action
             helm-select-action
             helm-make-source)
  :commands (helm-show-kill-ring)
  :defines (helm-source-bookmarks)            ; via helm-bookmarks.el
  :init (setq helm-command-prefix-key "C-c h" ; see `db/run-init' for explicit binding
              helm-input-idle-delay 0.0
              helm-buffers-fuzzy-matching t
              helm-autoresize-min-height 20
              helm-ff-auto-update-initial-value t
              helm-ff-file-name-history-use-recentf t
              helm-ff-search-library-in-sexp t
              helm-ff-skip-boring-files nil
              helm-split-window-inside-p t
              helm-move-to-line-cycle-in-source nil
              helm-scroll-amount nil
              helm-locate-command nil
              helm-candidate-number-limit 100
              helm-follow-mode-persistent t
              helm-buffer-details-flag t
              helm-buffer-skip-remote-checking t
              helm-mode-no-completion-in-region-in-modes '(eshell-mode)
              helm-kill-ring-threshold 0 ; include all yanks in the kill ring
              )
  :config (progn
            (eval-when-compile
              (require 'helm-mode)
              (require 'helm-buffers)
              (require 'helm-ring)
              (require 'helm-source)
              (require 'helm-bookmark))

            (if (require 'helm-global-bindings nil :no-error)
                (progn
                  (bind-key "#" #'helm-emms helm-command-map)
                  (bind-key "P" #'helm-pages helm-command-map))
              (warn (concat
                     "Cannot load `helm-global-bindings', please check your helm installation for completeness. "
                     "(Have you installed it from melpa?)")))

            (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
            (bind-key "C-i" #'helm-execute-persistent-action helm-map)
            (bind-key "C-z" #'helm-select-action helm-map)))

(use-package ivy
  :diminish ivy-mode
  :init (setq ivy-use-virtual-buffers t
              ivy-magic-tilde nil
              ivy-count-format "(%d/%d) "
              ivy-initial-inputs-alist '((counsel-describe-function . "^")
                                         (counsel-describe-variable . "^")
                                         (man . "^")
                                         (woman . "^"))
              ivy-use-selectable-prompt t
              ivy-do-completion-in-region t
              ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :config (progn
            ;; Since we are using `ivy--regex-ignore-order' for completion
            ;; anyway, providing an individual restriction in the ivy buffer is
            ;; not necessary anymore.  Since I often mistype S-SPC for SPC,
            ;; loosing the current candidate and annoying myself, removing this
            ;; shortcut is both helpful and not removing necessary
            ;; functionality.
            (define-key ivy-minibuffer-map (kbd "S-SPC") nil)))

(use-package swiper
  :ensure t
  :commands (swiper
             swiper-from-isearch))

(use-package recentf
  :commands (recentf-mode recentf-save-list)
  :init (setq recentf-max-saved-items 1000
              recentf-save-file (expand-file-name "recentf" emacs-d-userdata))
  :config (run-with-timer 0 3600 #'recentf-save-list))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode)
  :init (setq marginalia-max-relative-age 0))

(use-package vertico
  :ensure t
  :commands (vertico-mode))

(use-package orderless
  :ensure t
  :init (setq orderless-match-faces     ; Some of the default faces are hard to
                                        ; read with when solarized is enabled;
                                        ; only keep those that are readable
              [orderless-match-face-1 orderless-match-face-2]))

(use-package consult
  :commands (consult-completion-in-region
             consult-goto-line
             consult-keep-lines
             consult-focus-lines
             consult-buffer
             consult-imenu
             consult-line
             consult-mark
             consult-yank-pop
             consult-outline)
  :init (progn
          ;; Settings taken from https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
          (setq consult-line-numbers-widen t
                consult-async-min-input 3
                consult-async-input-debounce 0.5
                consult-async-input-throttle 0.8
                consult-narrow-key nil
                consult-preview-key nil))
  :config (require 'consult-imenu))

(use-package corfu
  :commands (global-corfu-mode corfu-mode))


;; * Navigation

(defun db/helm-shortcuts (arg)
  "Open helm completion on common locations.

With universal argument ARG, inhibit display of files in
`db/important-document-path’.  This might be helpful when loading
is too slow (in this case, `db/important-document-path' should
eventuelly be set to nil, however)."
  (interactive "P")
  (helm :sources (list
                  (helm-make-source "Frequently Used" 'helm-source-sync
                    :candidates (mapcar #'(lambda (entry)
                                            (cons (car entry)
                                                  (caddr entry)))
                                        db/frequently-used-features)
                    :action '(("Open" . call-interactively)))

                  ;; Taken from `helm-buffers-list'
                  (helm-make-source "Buffers" 'helm-source-buffers)

                  helm-source-bookmarks

                  ;; If no prefix arg is given, extract files from
                  ;; `db/important-documents-path’ and list them as well
                  (when (and (not arg)
                             (file-directory-p db/important-documents-path))
                    (let ((search-path (expand-file-name db/important-documents-path)))
                      (helm-make-source "Important files" 'helm-source-sync
                        :candidates (mapcar #'(lambda (file)
                                                ;; Display only relative path,
                                                ;; but keep absolute path for
                                                ;; actions
                                                (cons (string-remove-prefix search-path file)
                                                      file))
                                            (directory-files-recursively search-path ""))
                        :action '(("Open externally" . db/system-open)
                                  ("Find file" . find-file))))))))

(use-package ace-window
  :ensure t
  :commands (ace-window ace-window-display-mode)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-background t
              aw-leading-char-style 'char
              aw-scope 'frame))

(use-package avy
  :ensure t
  :commands (avy-goto-char-timer
             avy-goto-word-or-subword-1
             avy-goto-line))

(use-package bm
  ;; Taken from https://protesilaos.com/dotemacs/ and adapted slightly
  :ensure t
  :commands (bm-toggle bm-next bm-previous bm-toggle-buffer-persistence)
  :init (setq bm-restore-repository-on-load t
              bm-annotate-on-create nil
              bm-buffer-persistence t
              bm-cycle-all-buffers t
              bm-goto-position nil
              bm-highlight-style 'bm-highlight-line-and-fringe
              bm-marker 'bm-marker-right
              bm-in-lifo-order nil
              bm-recenter t
              bm-repository-file "~/.emacs.d/bm-bookmarks"
              bm-repository-size 100
              bm-show-annotations t
              bm-wrap-immediately t
              bm-wrap-search t))

(use-package bookmark
  :init (setq bookmark-default-file (expand-file-name "bookmarks" emacs-d-userdata)
              bookmark-menu-confirm-deletion t))

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :init (progn
          (setq dumb-jump-selector 'helm)
          (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))

(use-package helm-pages
  :ensure t
  :commands (helm-pages))

(use-package imenu
  :init (setq imenu-use-markers t
              imenu-auto-rescan t
              imenu-auto-rescan-maxout 600000
              imenu-max-item-length 100
              imenu-use-popup-menu nil
              imenu-eager-completion-buffer t))

(use-package isearch
  :init (setq isearch-allow-scroll t
              isearch-lazy-count t
              isearch-lax-whitespace nil
              isearch-regexp-lax-whitespace nil))

(use-package goto-last-change
  :commands goto-last-change)


;; * Media

(use-package db-music
  :init (setq db/auto-playlist-file-function
              #'(lambda ()
                  (db/playlist-files-from-git-annex-find
                   "--metadata db-playlist=include")))
  :commands (db/play-auto-playlist
             db/playlist-files-from-git-annex-find
             db/play-auto-playlist-from-git-annex-find
             music-control/body
             db/update-playlist-files))

(use-package emms
  :pin "melpa-stable"
  :commands (emms
             emms-pause
             emms-stop
             emms-next
             emms-previous)
  :bind (:map emms-playlist-mode-map
              ("S s" . emms-shuffle))
  :init (setq emms-source-file-default-directory "~/Documents/media/audio/"
              emms-player-list '(emms-player-mplayer emms-player-mplayer-playlist
                                 emms-player-vlc emms-player-vlc-playlist)
              emms-show-format "NP: %s"
              emms-stream-default-action "play"
              emms-track-description-function 'db/emms-track-description
              emms-playlist-default-major-mode 'emms-playlist-mode
              emms-cache-file (expand-file-name "emms/cache" emacs-d-userdata)
              emms-history-file (expand-file-name "emms/history" emacs-d-userdata)
              emms-score-file (expand-file-name "emms/scores" emacs-d-userdata)
              emms-stream-bookmarks-file (expand-file-name "emms/streams" emacs-d-userdata))
  :config (progn

            ;; Initialization copied and adapted from `emms-setup’

            (require 'emms-source-file)
            (require 'emms-source-playlist)
            (require 'emms-player-simple)
            (require 'emms-player-mplayer)
            (require 'emms-player-vlc)
            (require 'emms-playlist-mode)
            (require 'emms-info)
            (require 'emms-info-mp3info)
            (require 'emms-info-ogginfo)
            (require 'emms-info-opusinfo)
            (require 'emms-cache)
            (require 'emms-mode-line)
            (require 'emms-mark)
            (require 'emms-tag-editor)
            (require 'emms-show-all)
            (require 'emms-streams)
            (require 'emms-playing-time)
            (require 'emms-player-mpd)
            (require 'emms-playlist-sort)
            (require 'emms-browser)
            (require 'emms-mode-line-icon)
            (require 'emms-cue)
            (require 'emms-bookmarks)
            (require 'emms-last-played)
            (require 'emms-metaplaylist-mode)
            (require 'emms-stream-info)
            (require 'emms-score)
            (require 'emms-history)
            (require 'emms-i18n)
            (require 'emms-volume)
            (require 'emms-playlist-limit)

            (add-to-list 'emms-track-initialize-functions
                         'emms-info-initialize-track)

            (if (require 'emms-info-mediainfo nil 'no-error)
                (setq emms-info-functions '(emms-info-mediainfo))
              (when (executable-find emms-info-mp3info-program-name)
                (add-to-list 'emms-info-functions 'emms-info-mp3info))
              (when (executable-find emms-info-ogginfo-program-name)
                (add-to-list 'emms-info-functions 'emms-info-ogginfo))
              (when (executable-find emms-info-opusinfo-program-name)
                (add-to-list 'emms-info-functions 'emms-info-opusinfo))
              (add-to-list 'emms-info-functions 'emms-info-cueinfo))

            (when (fboundp 'emms-cache) ; work around compiler warning
              (emms-cache 1))
            (emms-mode-line -1)
            (emms-playing-time 1)
            (emms-score 1)

            (add-hook 'emms-player-started-hook 'emms-last-played-update-current)
            (add-hook 'emms-player-started-hook 'emms-show)

            (advice-add 'emms-tag-editor-submit
                        :after #'(lambda (&rest r)
                                   (ignore r)
                                   (delete-window)))

            (unless (eq system-type 'windows-nt)
              (setq emms-source-file-directory-tree-function
                    #'db/emms-source-file-directory-tree-find))

            ;; `emms-playlist-mode’ sets `emms-playlist-insert-track-function’,
            ;; no matter what previous values or customization may say otherwise
            ;; … so we need to employ a hook to change its value
            (add-hook 'emms-playlist-mode-hook
                      #'(lambda ()
                          (setq emms-playlist-insert-track-function
                                #'db/emms-playlist-mode-insert-track)))

            (run-with-timer 0 3600 #'emms-cache-save)))

(use-package emms-source-file
  :config (progn
            ;; Make sure emms is up and running when we call functions such as
            ;; `emms-play-dired’ etc.
            (require 'emms)))

(use-package db-emms
  :commands (db/emms-source-file-directory-tree-find
             db/emms-track-description
             db/emms-playlist-mode-insert-track))

(use-package helm-emms
  :commands (helm-emms)
  :init (setq helm-emms-use-track-description-function t
              helm-emms-default-sources '(helm-source-emms-streams
                                          helm-source-emms-dired
                                          helm-source-emms-files)))

(use-package image
  :init (setq image-use-external-converter t))


;; * Shells and such

(use-package comint
  :init (setq comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output nil
              comint-scroll-show-maximum-output t
              comint-completion-addsuffix t
              comint-buffer-maximum-size 100000
              comint-input-ring-size 5000)
  :config (progn
            ;; Never let bash know that we are inside Emacs;
            ;; cf. https://coredumped.dev/2020/01/04/native-shell-completion-in-emacs/
            (advice-add 'comint-term-environment
                        :filter-return #'(lambda (env) (cons "INSIDE_EMACS" env)))))

(use-package term
  :commands (term-send-string
             term-send-raw)
  :init (setq explicit-shell-file-name "/bin/bash")
  :config (progn
            (add-hook 'term-exec-hook   ; oremacs.com
                      #'(lambda ()
                          (let* ((buff (current-buffer))
                                 (proc (get-buffer-process buff)))
                            (set-process-sentinel
                             proc
                             `(lambda (process event)
                                (if (string= event "finished\n")
                                    (kill-buffer ,buff)))))))

            ;; Does not work; C-c is shadowed by some minor modes like semantic and winner
            (bind-key "C-c" #'term-send-raw term-raw-map)

            ;; Unbind some keys to allow the global keymap to handle them
            (unbind-key "M-:" term-raw-map)
            (unbind-key "C-h" term-raw-map)
            (unbind-key "M-x" term-raw-map)

            ;; We need to set keys starting with C-x after `ansi-term' has been called, as it resets
            ;; the escape character to C-x.
            (define-advice ansi-term (:after
                                      (&rest _)
                                      db/ansi-term--unbind-annoying-keys)
              (unbind-key "C-x C-j" term-raw-map)
              (unbind-key "C-x g" term-raw-map))

            (add-hook 'term-mode-hook #'(lambda () (yas-minor-mode -1)))))

(use-package ansi-color
  :commands (ansi-color-for-comint-mode-on
             ansi-color-apply-on-region)
  :config (progn
            (add-hook 'compilation-filter-hook 'endless/colorize-compilation)))

(use-package shell
  :commands (shell)
  :config (progn
            (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
            (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
            (add-hook 'shell-mode-hook 'with-editor-export-editor)

            ;; We may want to use readline support in bash, don't inhibit this
            ;; with explicit command line arguments;
            ;; cf. https://coredumped.dev/2020/01/04/native-shell-completion-in-emacs/

            (setq explicit-bash-args
                  (delete "--noediting" explicit-bash-args))))

(use-package db-eshell
  :commands (db/run-or-hide-eshell
             eshell-clear-buffer
             eshell/default-prompt-function
             eshell-insert-history
             pcomplete/git))

(use-package eshell
  :commands (eshell)
  :init (setq eshell-cmpl-cycle-completions nil
              eshell-scroll-to-bottom-on-input t
              eshell-prefer-lisp-functions nil
              eshell-error-if-no-glob nil
              eshell-hist-ignoredups t
              eshell-save-history-on-exit t
              eshell-history-size 5000
              eshell-history-file-name (expand-file-name "eshell/history" emacs-d-userdata)
              eshell-last-dir-ring-file-name (expand-file-name "eshell/lastdir" emacs-d-userdata)
              eshell-destroy-buffer-when-process-dies t
              eshell-prompt-function #'eshell/default-prompt-function
              eshell-prompt-regexp "└─[$#] "
              eshell-highlight-prompt nil
              eshell-cd-on-directory t
              eshell-expand-input-functions '(eshell-expand-history-references))
  :config (progn

            (eval-when-compile
              (require 'em-prompt)
              (require 'em-term)
              (require 'em-cmpl)
              (require 'em-hist)
              (require 'em-glob))

            (setenv "PAGER" "cat")

            (add-to-list 'eshell-command-completions-alist
                         '("gunzip" "gz\\'"))

            (add-to-list 'eshell-command-completions-alist
                         '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

            (add-hook 'eshell-mode-hook
                      'with-editor-export-editor)

            (bind-key "C-a" #'eshell-bol eshell-mode-map)
            (bind-key "M-r" #'eshell-insert-history eshell-hist-mode-map)
            (bind-key "M-P" #'eshell-previous-prompt eshell-mode-map)
            (bind-key "M-N" #'eshell-next-prompt eshell-mode-map)

            ;; Ignoring case when completing file names seems to have a
            ;; bug: when a ~ is encountered, it is implicitly expaned by
            ;; `pcomplete-insert-entry’, overwriting the prompt as a side
            ;; effect.  Keeping the case as it is does not seem to have
            ;; this issue.  This problem occurs by default only on Windows
            ;; systems (in all flavors), because this is the only time
            ;; `pcomplete-ignore-case’ is non-nil by default.
            (when on-windows
              (add-to-list 'eshell-mode-hook
                           #'(lambda ()
                               (setq completion-ignore-case nil))))

            ;; Sometimes, when completing path names and immediately
            ;; hitting RET, `completion-in-region-mode' still seems to be
            ;; active; this often happens when calling cd.  Then,
            ;; `post-command-hook' still contains
            ;; `completion-in-region--postch', which calls some `pcomplete'
            ;; function to determine whether completion mode should exit.
            ;; However, after RET (i.e., `eshell-send-input'), we only have
            ;; an empty prompt, and `pcomplete' just computes all possible
            ;; functions for completion (doesn't show it, though).  This
            ;; introduces a noticeable amount of delay, which is annoying.
            ;; Expliticly disabling `completion-in-region-mode' before
            ;; sending input seems to alleviate the problem.
            (advice-add 'eshell-send-input
                        :before #'(lambda (&rest _)
                                    "Disable completion-in-region-mode before sending input."
                                    (when completion-in-region-mode
                                      (completion-in-region-mode -1))))

            ;; After completing partial inputs, pcomplete wants to add an
            ;; extra character stored in `pcomplete-termination-string',
            ;; which is a space by default.  When completing paths, this
            ;; leads to spaces being inserted after every directory within
            ;; the path, which is annoying.  We thus set the value of this
            ;; variable locally to an empty string, thus silencing this
            ;; bug.  A better way to handle this would be to correctly
            ;; determine whether the completion is not done yet, by passing
            ;; `exact' instead of `finished' to the handlers stored in
            ;; `completion-extra-properties'.
            (add-hook 'eshell-mode-hook
                      #'(lambda ()
                          (setq-local pcomplete-termination-string "")))

            (require 'db-eshell)))

(use-package em-prompt                  ; Why is this extra declaration necessary?
  :commands (eshell-previous-prompt
             eshell-next-prompt))

(use-package esh-mode                   ; Why is this extra declaration necessary?
  :commands (eshell-bol))

(use-package with-editor
  :commands (with-editor-export-editor))

(use-package sh-script
  :init (setq sh-basic-offset 2))


;; * Lisp

(use-package lisp-mode
  :mode (("\\.cl\\'" . lisp-mode)
         ("\\.lisp\\'" . lisp-mode))
  :init (setq lisp-indent-function #'lisp-indent-function))

(use-package lispy
  :ensure t
  :commands (lispy-mode)
  :diminish lispy-mode)

(use-package elisp-mode
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'turn-on-lispy-when-available)
            (add-hook 'emacs-lisp-mode-hook 'turn-on-flycheck-when-file)
            (add-hook 'lisp-mode-hook 'turn-on-lispy-when-available)))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode))
  :config (add-hook 'clojure-mode-hook 'turn-on-lispy-when-available))

(use-package slime
  :commands (slime slime-mode slime-connect)
  :init     (setq inferior-lisp-program "sbcl --noinform --no-linedit"
                  slime-net-coding-system 'utf-8-unix
                  slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
                  slime-lisp-implementations '((sbcl ("sbcl") :coding-system utf-8-unix)
                                               (cmucl ("cmucl") :coding-system utf-8-unix)
                                               (ccl ("ccl") :coding-system utf-8-unix)))
  :config   (progn
              (slime-setup '(slime-repl slime-fancy slime-autodoc))
              (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)))

(use-package hy-mode
  :commands (hy-mode)
  :config (progn
            (add-hook 'hy-mode-hook 'turn-on-lispy-when-available)
            (add-hook 'hy-mode-hook 'inferior-lisp)))


;; * Other Mode Configurations

(use-package cperl-mode
  :ensure t
  :commands (cperl-mode)
  :mode ("\\.plx\\'" . cperl-mode)
  :init (progn
          (if (boundp 'major-mode-remap-alist)
              (add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))
            (mapc
             #'(lambda (pair)
                 (if (eq (cdr pair) 'perl-mode)
                     (setcdr pair 'cperl-mode)))
             (append auto-mode-alist interpreter-mode-alist)))

          (setq cperl-hairy nil
                cperl-invalid-face 'default
                cperl-electric-keywords nil
                cperl-lazy-help-time 2
                cperl-highlight-variables-indiscriminately t
                cperl-indent-parens-as-block t))
  :config (progn
            (add-hook 'cperl-mode-hook 'flycheck-mode)
            (add-hook 'cperl-mode-hook 'prettify-symbols-mode)

            ;; enable display of help messages after a short period of time, as
            ;; controlled by the value of `cperl-lazy-help-time'
            (add-hook 'cperl-mode-hook 'cperl-lazy-install)))

(use-package define-word
  :ensure t
  :commands (define-word-at-point define-word))

(use-package dictcc
  :ensure t
  :commands (dictcc)
  :config (require 'gnutls))

(when (package-installed-p "auctex")
  (when (boundp 'major-mode-remap-alist)
    (add-to-list 'major-mode-remap-alist '(latex-mode . LaTeX-mode)))
  (require 'db-latex))

(use-package edit-indirect              ; to allow code editing in markdown-mode
  :ensure t)

(use-package edit-list
  :ensure t
  :commands edit-list)

(use-package expand-region
  :ensure t
  :commands (er/expand-region))

(use-package eww
  :init (setq eww-bookmarks-directory emacs-d-userdata))

(use-package haskell-mode
  :config (progn
            (add-hook 'haskell-mode-hook 'haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            (add-hook 'haskell-mode-hook 'flycheck-mode)
            (add-hook 'haskle--mode-hook 'subword-mode)

            (with-demoted-errors "Non-Fatal Error: %s"
              (require 'haskell-indentation)
              (add-hook 'haskell-mode-hook
                        'haskell-indentation-mode))

            (add-hook 'haskell-mode-hook
                      'interactive-haskell-mode)))

(use-package markdown-mode
  :ensure t
  :pin "melpa-stable"
  :commands (markdown-mode)
  :init (progn
          (setq markdown-use-pandoc-style-yaml-metadata t
                markdown-command "pandoc --standalone --toc"
                markdown-fontify-code-blocks-natively t)
          (fset 'markdown-output-standalone-p #'(lambda () t))
          (add-hook 'markdown-mode-hook #'turn-off-auto-fill)
          (add-hook 'markdown-mode-hook #'turn-on-visual-line-mode)))

(use-package plantuml-mode
  :load-path "site-lisp"
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :commands (plantuml-mode)
  :init (setq plantuml-output-type "svg"
              plantuml-default-exec-mode 'jar
              plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
              plantuml-indent-level 2))

(use-package project
  :init (setq project-switch-commands 'project-dired))

(use-package python
  :config (progn
            (add-hook 'python-mode-hook #'highlight-indentation-mode)
            (add-hook 'python-mode-hook #'eglot-ensure)
            (add-hook 'python-mode-hook #'subword-mode)))

;; https://ddavis.io/posts/emacs-python-lsp/
(use-package pyvenv
  :commands (pyvenv-workon
             pyvenv-activate
             pyvenv-create)
  :init (unless (getenv "WORKON_HOME")
          (setenv "WORKON_HOME" (expand-file-name "~/.pyenv/versions")))
  :config (progn
            ;; Restart python inferior processes when switching virtual
            ;; environments; this does not work when only calling
            ;; `pyvenv-deactivate', though.
            (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)))

(use-package re-builder
  :commands (re-builder)
  :init (setq reb-re-syntax 'string))

(use-package shr
  :init (setq shr-use-fonts nil
              shr-use-colors nil
              shr-max-image-proportion 0.7
              shr-image-animate nil
              shr-width (current-fill-column)))

(use-package table
  :init (progn
          ;; Pandoc supports colons in grid tables to denote alignments, so let's have table.el
          ;; recognize those too.
          (setq table-cell-horizontal-chars "-=:")))

(use-package textile-mode
  :config (progn
            ;; Do not wrap lines automatically in textile mode, as text produced
            ;; in this mode is usually copied to Redmine Wiki pages later on.
            (add-hook 'textile-mode-hook #'turn-off-auto-fill)
            (add-hook 'textile-mode-hook #'turn-on-visual-line-mode)))

(use-package timeline-tools
  :load-path "site-lisp"
  :commands (timeline-tools-format-timeline
             timeline-tools-format-timeline-of-day
             timeline-tools-copy-clocklines
             timeline-tools-clockline-no-org-agenda-conflicts))

(use-package vlf
  :ensure t
  :commands (vlf))

(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :diminish which-key-mode
  :init (setq which-key-side-window-max-width 0.33
              which-key-side-window-max-height 0.25)
  :config (which-key-setup-side-window-bottom))

(use-package yaml-mode
  :ensure t
  :config (progn
            (add-hook 'yaml-mode-hook #'highlight-indentation-mode)))


;; * Load customizations

(when (file-exists-p custom-file)
  (load-file custom-file))


;; * Actual Mode Activation

(defun db/run-init ()
  "Run main initialization after everything is set up."

  (message "Running main initialization ...")

  ;; Activate modes (builtin)

  (show-paren-mode +1)
  (transient-mark-mode +1)
  (global-font-lock-mode +1)
  (column-number-mode +1)
  (delete-selection-mode -1)

  (dolist (mode '(tool-bar-mode
                  scroll-bar-mode
                  menu-bar-mode
                  blink-cursor-mode
                  tooltip-mode))
    (when (fboundp mode)
      (funcall mode 0)))

  (electric-indent-mode -1)

  (appt-activate +1)
  (savehist-mode +1)

  (size-indication-mode +1)
  (display-battery-mode -1)

  (electric-pair-mode +1)

  (recentf-mode +1)
  (winner-mode +1)
  (tab-bar-history-mode +1)
  (global-auto-revert-mode -1)
  (which-function-mode +1)
  (global-eldoc-mode +1)

  ;; Activate modes (packages)

  (dolist (mode '(global-undo-tree-mode
                  minibuffer-depth-indicate-mode
                  ace-window-display-mode
                  key-chord-mode
                  which-key-mode
                  yas-global-mode
                  global-git-commit-mode
                  marginalia-mode
                  vertico-mode))
    (with-demoted-errors "Cannot activate mode: %s"
      (funcall mode +1)))

  ;; This causes inacceptable lag when drawing buffers, so disable it for now.
  ;; Needs to be investigated further.

  ;; (with-demoted-errors "Cannot activate moody: %s"
  ;;   (moody-replace-mode-line-buffer-identification)
  ;;   (moody-replace-vc-mode))

  (with-demoted-errors "Cannot activate `vlf': %s"
    (require 'vlf-setup))

  ;; Global Hooks

  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'prog-mode-hook 'electric-indent-local-mode)

  (add-hook 'text-mode-hook 'page-break-lines-mode)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'text-mode-hook 'abbrev-mode)
  (add-hook 'text-mode-hook 'hl-line-mode)

  ;; Top-Level Keybindings

  (bind-key "<XF86Back>" #'winner-undo)
  (bind-key "<XF86Forward>" #'winner-redo)
  (bind-key "<Scroll_Lock>" 'scroll-lock-mode)
  (bind-key "<f10>" #'magit-status)
  (bind-key "<f1>" #'db/run-or-hide-eshell)
  (bind-key "<f2>" #'hydra-feature-shortcuts/body)
  (bind-key "<f5>" #'rgrep)
  (bind-key "<f6>" #'hydra-zoom/body)
  (bind-key "<f7>" #'dictcc)
  (bind-key "<f8>" #'bm-toggle)
  (bind-key "<f9>" #'hydra-org-linking/body)
  (bind-key "<C-f8>" #'bm-next)
  (bind-key "<C-S-f8>" #'bm-previous)
  (bind-key "C-," #'mc/skip-to-previous-like-this)
  (bind-key "C-." #'mc/skip-to-next-like-this)
  (bind-key "C-;" #'iedit-mode)
  (bind-key "C-<" #'mc/mark-previous-like-this)
  (bind-key "C->" #'mc/mark-next-like-this)
  (bind-key "C-@" #'er/expand-region)
  (bind-key "C-M-\\" #'crux-cleanup-buffer-or-region)
  (bind-key "C-S-c C-S-c" #'mc/edit-lines)
  (bind-key "C-Z" #'undo-tree-redo)
  (bind-key "C-c C-<" #'mc/mark-all-like-this)
  (bind-key "C-c D" #'define-word)
  (bind-key "C-c J" #'avy-goto-word-or-subword-1)
  (bind-key "C-c a" #'org-agenda)
  (bind-key "C-c c" #'org-capture)
  (bind-key "C-c d" #'define-word-at-point)
  (bind-key "C-c e" #'crux-eval-and-replace)
  (bind-key "C-c i" #'ispell-change-dictionary)
  (bind-key "C-c j" #'avy-goto-char-timer)
  (bind-key "C-c l" #'org-store-link)
  (bind-key "C-c m" #'music-control/body)
  (bind-key "C-c n f" #'org-roam-node-find)
  (bind-key "C-c n i" #'org-roam-node-insert)
  (bind-key "C-c n c" #'org-roam-capture)
  (bind-key "C-c o" #'hydra-org-custom/body)
  (bind-key "C-c r" #'recentf)
  (bind-key "C-c t" #'hydra-toggle/body)
  (bind-key "C-h f" #'helpful-callable)
  (bind-key "C-h o" #'helpful-symbol)
  (bind-key "C-h v" #'helpful-variable)
  (bind-key "C-h k" #'helpful-key)
  (bind-key "C-h x" #'helpful-command)
  (bind-key "C-h C-f" #'find-function)
  (bind-key "C-h C-k" #'find-function-on-key)
  (bind-key "C-h C-v" #'find-variable)
  (bind-key "C-x 4 C-j" #'dired-jump-other-window)
  (bind-key "C-x C-b" #'ibuffer)
  (bind-key "C-x C-d" #'dired)
  (bind-key "C-x C-j" #'dired-jump)
  (bind-key "C-x C-r" #'revert-buffer)
  (bind-key "C-x SPC" #'hydra-rectangle/body)
  (bind-key "C-x g" #'db/helm-shortcuts)
  (bind-key "C-x r E" #'db/bookmark-add-external)
  (bind-key "C-x r M" #'db/bookmark-add-url)
  (bind-key "C-x r v" #'list-registers)
  (bind-key "C-z" #'goto-last-change)
  (bind-key "M-/" #'hippie-expand)
  (bind-key "M-:" #'pp-eval-expression)
  (bind-key "M-=" #'count-words)
  (bind-key "M-SPC" #'cycle-spacing)    ; default since Emacs 29.1
  (bind-key "M-Z" #'zap-to-char)
  (bind-key "M-i" #'swiper-from-isearch isearch-mode-map)
  (bind-key "M-j" #'(lambda () (interactive) (join-line -1)))
  (bind-key "M-z" #'zap-up-to-char)
  (bind-key [remap fill-paragraph] #'endless/fill-or-unfill)
  (bind-key [remap keyboard-quit] #'keyboard-quit-context+)
  (unbind-key "<insert>" global-map)
  (unbind-key "<kp-insert>" global-map)
  (unbind-key "C-x C-c" global-map)
  (unbind-key "M-o" global-map)

  ;; Overwrite certain keybindings only if packages are avilable

  (when (package-installed-p 'helm)
    ;; Explicitly require helm, because autoloading is difficult with helm's
    ;; separate `helm-command-prefix-key' mechanism.
    (require 'helm)
    (bind-key helm-command-prefix-key #'helm-command-prefix))

  (when (package-installed-p 'crux)
    (bind-key [remap kill-whole-line] #'crux-kill-whole-line)
    (bind-key [remap open-line] #'crux-smart-open-line-above))

  (when (package-installed-p 'ace-window)
    (bind-key "C-x o" #'ace-window))

  (when (package-installed-p 'avy)
    (bind-key "M-g M-g" #'avy-goto-line)
    (bind-key "M-g g" #'avy-goto-line))

  (when (package-installed-p 'consult)
    (bind-key "M-g i" #'consult-imenu)
    (bind-key "C-x b" #'consult-buffer)
    (setq completion-in-region-function #'consult-completion-in-region)
    (bind-key "M-r" #'consult-history eshell-hist-mode-map)
    (bind-key "M-r" #'consult-history minibuffer-mode-map)
    (bind-key "C-r" #'consult-history minibuffer-mode-map))

  ;; Environment Variables

  (unless on-windows
    (with-demoted-errors "Cannot import environment variables: %s"
      (exec-path-from-shell-copy-envs '("SSH_AUTH_SOCK"
                                        "SSH_AGENT_PID"
                                        "PATH"
                                        "TEXMFHOME"
                                        "PERL5LIB"
                                        "PERL_LOCAL_LIB_ROOT"
                                        "PERL_MB_OPT"
                                        "PERL_MM_OPT"))))

  ;; Start Server when not running already

  ;; The following condition should actually always be false, since we have
  ;; neither loaded the server package yet nor have explicitly started the
  ;; server process.  Also the --daemon command line switches will start the
  ;; server only later, after initialization (and they do so unconditionally,
  ;; thus restarting the server we have started here).  However, for robustness,
  ;; we keep the condition nevertheless, since when a server process is already
  ;; present, we really don't have to do anything.  Furthermore, calling
  ;; `db/run-init' again in a running Emacs will not restart the server (but
  ;; then, why whould one want to do this?).
  (if (and (boundp 'server-process) server-process)
      (message "Server already running, not restarting.")

    (require 'server)
    (let ((server-file (expand-file-name server-name
                                         (if server-use-tcp server-auth-dir server-socket-dir))))
      (if (file-exists-p server-file)
          (warn "Server file already exists, but no server process is running.  Check %s and restart server manually."
                server-file)

        (server-start)
        (cl-case (server-running-p)
          ((t) t)                       ; server is running
          ((nil) (warn "Server not running, check logs and restart manually."))
          (t (warn "`server-running-p' returned neither nil nor t.  Check and restart server manually if required."))))))

  ;; Warn of Windows pecularitites

  (when on-windows
    ;; Warn if `grep-find-template' or `grep-find-command' are undefined in `grep.el', because then
    ;; they would be set by `grep-compute-defaults', which in turn uses `find-program', which is set
    ;; to "find", which on Windows might be CMD's FIND, which does something else (it's more akin to
    ;; grep).
    (when (and (require 'grep nil t)
               (or (null grep-find-template)
                   (null grep-find-command)))
      (warn (concat "`grep-find-template' or `grep-find-command' are undefined, please customize these to use the right version of `find'"))))

  ;; Load custom code

  (dolist (file db/after-init-load-files)
    (message "Loading %s" file)
    (with-demoted-errors "Error loading file: %s"
      (load-file file)))

  ;; Finish

  (message "Running main initialization ... done")

  t)

(add-hook 'after-init-hook #'db/run-init)

;;; init.el ends here
