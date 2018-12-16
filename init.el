;;; Init.el --- Daniel's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This is the main entry point for Emacs to load this configuration.  The
;; structure is roughly as follows:
;; * first comes some preliminary setup, mostly setting up `package’;
;; * the main activation of the configuration is done in the function
;;   `db/run-init’, which is installed in `after-init-hook’; it is thus run
;;   after init.el has been read
;; * then comes setting up all the packages that can be used by this
;;   configuration; most of these packages are not loaded however, and only
;;   configuration hooks are installed (using `use-package’); this way a user
;;   can choose in `db/run-init’ which configruation to activate without
;;   changing much of the rest of the file.
;; * this file also introduces a new customization group `personal-settings’
;;   containing variables (mostly file paths) that must be set to enable some
;;   of the provied functionality.

;;; Code:


;; * Constants

(defconst emacs-d (file-name-directory
                   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

(defconst on-windows (memq system-type '(windows-nt cygwin))
  "Non-nil if and only if this instance of Emacs runs on Windows.")


;; * Packages

(require 'package)

(setq package-user-dir (expand-file-name "elpa" emacs-d))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(setq package-enable-at-startup nil
      load-prefer-newer t)

(package-initialize)

(eval-when-compile
  (dolist (package '(diminish use-package bind-key))
    (unless (package-installed-p package)
      (package-install package))
    (require package)))

(put 'use-package 'lisp-indent-function 1)
(setq use-package-verbose t
      use-package-minimum-reported-time 0.01)

(add-to-list 'load-path (expand-file-name "site-lisp" emacs-d))


;; * Mode activation

(defun db/run-init ()
  "Run main initialization after everything is set up."

  (message "Running main initialization ...")

  ;; Load customizations

  (when (file-exists-p custom-file)
    (load-file custom-file))

  ;; Activate modes (builtin)

  (show-paren-mode 1)
  (transient-mark-mode 1)
  (global-font-lock-mode 1)
  (column-number-mode 1)
  ;; (display-time)
  (delete-selection-mode 1)

  (dolist (mode '(tool-bar-mode
                  scroll-bar-mode
                  menu-bar-mode
                  blink-cursor-mode
                  tooltip-mode))
    (when (fboundp mode)
      (funcall mode 0)))

  (when (<= 24 emacs-major-version)
    (electric-indent-mode -1))

  (appt-activate +1)
  (savehist-mode 1)

  (quietly-read-abbrev-file)

  (size-indication-mode 1)
  (display-battery-mode -1)

  (electric-pair-mode +1)

  (recentf-mode t)
  (winner-mode 1)
  (global-auto-revert-mode -1)
  (which-function-mode +1)

  ;; Activate modes (packages)

  (dolist (mode '(global-undo-tree-mode
                  ace-window-display-mode
                  key-chord-mode
                  sml/setup
                  ivy-mode
                  which-key-mode
                  eyebrowse-mode))
    (with-demoted-errors "Cannot activate mode: %s"
      (funcall mode +1)))

  (projectile-mode +1)

  (unless on-windows
    (with-demoted-errors "Error: %s"
      (pdf-tools-install)))

  ;; Global Hooks

  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (when (<= 24 emacs-major-version)
    (add-hook 'prog-mode-hook 'electric-indent-local-mode))
  (add-hook 'lisp-mode-hook 'lispy-mode)
  (unless (eq system-type 'windows-nt)
    ;; flyspell doesn’t work on windows right now, need to further investigate
    ;; what is happening here
    (add-hook 'text-mode-hook 'turn-on-flyspell))
  (add-hook 'text-mode-hook 'yas-minor-mode-on)
  (add-hook 'text-mode-hook 'electric-quote-mode)

  ;; Auto-Modes

  (dolist (mode-spec '(("\\.clj\\'" . clojure-mode)
                       ("\\.cl\\'" . lisp-mode)
                       ("\\.lisp\\'" . lisp-mode)
                       ("\\.plx\\’" . cperl-mode)
                       ("\\.hs\\'" . haskell-mode)
                       ("\\.lhs\\'" . haskell-mode)
                       ("\\.md\\'" . markdown-mode)
                       ("\\.html\\'" . nxml-mode)
                       ("\\.xml\\'" . nxml-mode)))
    (add-to-list 'auto-mode-alist mode-spec))

  ;; Top-Level Keybindings

  (bind-key "<XF86Back>" #'winner-undo)
  (bind-key "<XF86Forward>" #'winner-redo)
  (bind-key "<f1>" #'db/run-or-hide-eshell)
  (bind-key "<f2> i" #'counsel-info-lookup-symbol)
  (bind-key "<f2> u" #'counsel-unicode-char)
  (bind-key "<f5>" #'rgrep)
  (bind-key "<f6>" #'hydra-zoom/body)
  (bind-key "<f7>" #'dictcc)
  (bind-key "<f8>" #'counsel-locate)
  (bind-key "<f9>" #'helm-org-agenda-files-headings)
  (bind-key "<f10>" #'magit-status)
  (bind-key "<f11>" #'org-capture)
  (bind-key "<f12>" #'db/helm-shortcuts)
  (bind-key "C-;" #'iedit-mode)
  (bind-key "C-<" #'mc/mark-previous-like-this)
  (bind-key "C->" #'mc/mark-next-like-this)
  (bind-key "C-." #'mc/skip-to-next-like-this)
  (bind-key "C-," #'mc/skip-to-previous-like-this)
  (bind-key "C-@" #'er/expand-region)
  (bind-key "C-M-\\" #'crux-cleanup-buffer-or-region)
  (bind-key "C-S-c C-S-c" #'mc/edit-lines)
  (bind-key "C-Z" #'undo-tree-redo)
  (bind-key "C-c C-<" #'mc/mark-all-like-this)
  (bind-key "C-c C-r" #'ivy-resume)
  (bind-key "C-c D" #'define-word)
  (bind-key "C-c J" #'avy-goto-word-or-subword-1)
  (bind-key "C-c P" #'ivy-pages)
  (bind-key "C-c a" #'org-agenda)
  (bind-key "C-c c" #'org-capture)
  (bind-key "C-c d" #'define-word-at-point)
  (bind-key "C-c e" #'crux-eval-and-replace)
  (bind-key "C-c i" #'hydra-ispell/body)
  (bind-key "C-c j" #'avy-goto-char-timer)
  (bind-key "C-c l" #'org-store-link)
  (bind-key "C-c m" #'emms-control/body)
  (bind-key "C-c o" #'hydra-org-clock/body)
  (bind-key "C-c s" #'synonyms)
  (bind-key "C-h C-f" #'find-function)
  (bind-key "C-h C-k" #'find-function-on-key)
  (bind-key "C-s" #'isearch-forward)
  (bind-key "M-i" #'swiper-from-isearch isearch-mode-map)
  (bind-key "C-x C-d" #'dired)
  (bind-key "C-x C-r" #'revert-buffer)
  (bind-key "C-x g" #'db/helm-shortcuts)
  (bind-key "C-x SPC" #'hydra-rectangle/body)
  (bind-key "C-x r v" #'list-registers)
  (bind-key "C-x t" #'hydra-toggle/body)
  (bind-key "C-z" #'undo)
  (bind-key "M-/" #'hippie-expand)
  (bind-key "M-:" #'pp-eval-expression)
  (bind-key "M-=" #'count-words)
  (bind-key "M-SPC" #'cycle-spacing)
  (bind-key "M-Z" #'zap-to-char)
  (bind-key "M-g M-g" #'avy-goto-line)
  (bind-key "M-g g" #'avy-goto-line)
  (bind-key "M-g j b" #'dumb-jump-back)
  (bind-key "M-g j g" #'dumb-jump-go)
  (bind-key "M-j" #'(lambda () (interactive) (join-line -1)))
  (bind-key "M-o" nil)
  (bind-key "M-z" #'zap-up-to-char)
  (bind-key [insert] nil)
  (bind-key [kp-insert] nil)
  (bind-key [remap fill-paragraph] #'endless/fill-or-unfill)

  ;; Overwrite certain keybindings only if packages are avilable

  (when (package-installed-p 'counsel)
    (bind-key "M-x" #'counsel-M-x)      ; gets nicer sorting with smex installed
    (bind-key "C-c r" #'counsel-recentf)
    (bind-key "C-x C-f" #'counsel-find-file)
    (bind-key "C-h f" #'counsel-describe-function)
    (bind-key "C-h v" #'counsel-describe-variable)
    (bind-key "C-S-s" #'counsel-grep-or-swiper))

  (when (package-installed-p 'helm)
    (bind-key "M-y" #'helm-show-kill-ring))

  (when (package-installed-p 'crux)
    (bind-key [remap kill-whole-line] #'crux-kill-whole-line)
    (bind-key [remap open-line] #'crux-smart-open-line-above))

  (when (package-installed-p 'ace-window)
    (bind-key "C-x o" #'ace-window))

  (when (executable-find "ag")
    (bind-key "<f5>" #'counsel-ag))

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

  ;; Fixes

  (with-eval-after-load 'enriched
    (defun enriched-decode-display-prop (start end &optional params)
      (ignore params)
      (list start end)))

  ;; Start Server when not running already

  (unless (server-running-p)
    (server-start))

  ;; Finish
  
  (message "Running main initialization ... done")

  t)

(add-hook 'after-init-hook #'db/run-init)


;; * Personal customization

(defgroup personal-settings nil
  "A bunch of functions and variables for personalizing emacs."
  :prefix "db/"
  :group 'convenience
  :group 'help
  :tag "Personal settings")

(defcustom db/additional-mail-addresses nil
  "List of additional email addresses (apart from `user-mail-address’)."
  :group 'personal-settings
  :type '(repeat string))

(defcustom db/jabber-id ""
  "Personal XMPP ID."
  :group 'personal-settings
  :type 'string)


;; * Builtin Variables

(setq custom-file
      (expand-file-name "private/custom.el" emacs-d))

(use-package cl-lib)
(use-package subr-x)

(use-package warnings
  :config (cl-pushnew '(undo discard-info) warning-suppress-types
                      :test #'equal))

(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      ring-bell-function #'ignore
      garbage-collection-messages nil
      load-prefer-newer nil)            ; t breaks `org-reload'

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

(setq frame-title-format "emacs")

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline nil
      sentence-end-double-space t
      recenter-positions '(top middle bottom)
      scroll-conservatively 10
      message-log-max t
      inhibit-eol-conversion nil
      tab-always-indent 'complete
      enable-recursive-minibuffers t
      set-mark-command-repeat-pop t
      large-file-warning-threshold 10000000
      echo-keystrokes 0.1
      delete-by-moving-to-trash t
      delete-trailing-lines nil
      x-underline-at-descent-line t
      search-whitespace-regexp "[ \t\r\n]+"
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(when on-windows
  ;; treat memory for display time ...  but hey, this is Windows, memory doesn’t
  ;; matter!
  (setq inhibit-compacting-font-caches t))

(setq-default cursor-type 'bar)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties '(read-only t
                                     face minibuffer-prompt
                                     cursor-intangible t))

;; Make M-v undo C-v
(setq scroll-preserve-screen-position 'always)

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

(setq-default async-shell-command-buffer 'new-buffer)
(add-to-list 'display-buffer-alist
             '("^\*Async Shell Command*" . (display-buffer-no-window)))

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package calendar
  :init
  (setq calendar-date-style 'iso
        calendar-bahai-all-holidays-flag nil
        calendar-chinese-all-holidays-flag nil
        calendar-christian-all-holidays-flag t
        calendar-islamic-all-holidays-flag nil
        calendar-hebrew-all-holidays-flag nil
        holiday-general-holidays '((holiday-fixed 1 1 "New Year's Day")
                                   (holiday-fixed 2 14 "Valentine's Day")
                                   (holiday-fixed 4 1 "April Fools' Day")
                                   (holiday-fixed 5 1 "Labour Day")
                                   (holiday-fixed 10 3 "German Unity Day")
                                   (holiday-fixed 10 31 "Reformation Day")
                                   (holiday-float 11 3 -1 "Day of Repentance and Prayer" 22)
                                   (holiday-float 11 4 4 "Thanksgiving"))
        diary-show-holidays-flag t
        calendar-view-holidays-initially-flag t))

(setq-default font-lock-maximum-decoration '((t . t)))
(setq-default savehist-file (expand-file-name "savehist" emacs-d))

(use-package tramp
  :init (setq tramp-save-ad-hoc-proxies t))

(use-package re-builder
  :commands (re-builder)
  :init (setq reb-re-syntax 'string))

(setq lisp-indent-function #'lisp-indent-function)

(setq custom-theme-directory (expand-file-name "themes/" emacs-d))

;; https://florian.adamsky.it/2016/03/31/emacs-calc-for-programmers-and-cs.html
(setq math-additional-units
      '((bit nil "Bit")
        (byte "8 * bit" "Byte")
        (bps "bit / s" "Bit per second"))
      math-units-table nil)

(setq default-input-method "TeX")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")


;; * Basic Builtin Packages

(use-package misc
  :commands (zap-up-to-char zap-to-char))

(use-package grep
  :commands (rgrep zrgrep)
  :bind (:map grep-mode-map
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(use-package winner
  :commands (winner-mode winner-undo winner-redo))

(use-package abbrev
  :commands (quietly-read-abbrev-file)
  :init (progn
          (setq-default abbrev-mode t)
          (setq save-abbrevs 'silently))
  :diminish abbrev-mode)

(use-package appt
  :commands (appt-activate)
  :init (setq appt-display-mode-line nil))

(use-package ediff
  :defer t
  :init (setq ediff-diff-options "-w"
              ediff-window-setup-function 'ediff-setup-windows-plain
              ediff-split-window-function 'split-window-horizontally)
  :config (progn
            (add-hook 'ediff-keymap-setup-hook
                      '(lambda ()
                        (bind-key "j" #'ediff-next-difference ediff-mode-map)
                        (bind-key "k" #'ediff-previous-difference ediff-mode-map)))

            (add-hook 'ediff-after-quit-hook-internal 'winner-undo)))

(use-package ispell
  :commands (ispell-change-directory)
  :init (setq ispell-dictionary "en_US"
              ispell-really-hunspell t))

(use-package mailcap
  :defer t
  :config (progn
            ;; Remove doc-view so pdf will open with default viewer
            (setcdr
             (assoc "application" mailcap-mime-data)
             (remove '("pdf"
                       (viewer . doc-view-mode)
                       (type . "application/pdf")
                       (test eq window-system 'x))
                     (cdr (assoc "application" mailcap-mime-data))))))

(use-package quail
  :defer t
  :config (add-hook 'input-method-activate-hook
                    #'db/add-symbols-to-TeX-input-method))

(use-package server
  :commands (server-running-p server-start))


;; * Some essential packages

(use-package db-utils
  :commands (endless/fill-or-unfill
             db/delete-trailing-whitespace-maybe
             db/go-dark
             db/go-light
             db/show-current-org-task
             db/run-or-hide-shell
             db/run-or-hide-eshell
             db/run-or-hide-ansi-term
             db/helm-shortcuts
             db/hex-to-ascii
             db/ascii-to-hex
             conditionally-enable-lispy
             db/sort-nsm-permanent-settings
             db/update-cert-file-directory
             endless/colorize-compilation
             db/add-use-package-to-imenu
             db/turn-off-local-electric-pair-mode
             db/export-diary
             db/add-symbols-to-TeX-input-method
             hydra-ispell/body
             hydra-toggle/body
             hydra-zoom/body
             hydra-rectangle/body
             db/two-monitors-xrandr
             db/one-monitor-xrandr
             db/pretty-print-xml))

(use-package hydra
  :commands (defhydra))

(use-package magit
  :commands (magit-status)
  :init (setq magit-diff-refine-hunk t
              magit-commit-show-diff nil
              magit-popup-use-prefix-argument 'default
              magit-completing-read-function 'ivy-completing-read)
  :config (progn
            (global-magit-file-mode -1)

            (with-demoted-errors "Non-Fatal Error: %s"
              (eval-when-compile
                (require 'projectile))
              (setq magit-repository-directories
                    (mapcar
                     (lambda (dir)
                       (cons (substring dir 0 -1) 0))
                     (cl-remove-if-not
                      (lambda (project)
                        (unless (file-remote-p project)
                          (file-exists-p (concat project "/.git"))))
                      projectile-known-projects))))))

(use-package projectile
  :commands (projectile-mode)
  :defines (projectile-known-projects)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init (setq projectile-switch-project-action 'projectile-dired
              projectile-completion-system 'ivy
              projectile-ignored-project-function #'file-remote-p
              projectile-create-missing-test-files t)
  :config (projectile-cleanup-known-projects)
  :diminish projectile-mode)

(use-package counsel-projectile
  :commands counsel-projectile)

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-copy-envs))


;; * Org

(use-package db-org
  :commands (db/verify-refile-target
             org-reset-checkbox-state-maybe
             db/find-parent-task
             db/ensure-running-clock
             db/save-current-org-task-to-file
             db/org-clock-out
             db/org-clock-in-break-task
             db/org-clock-in-home-task
             db/org-clock-in-work-task
             endless/org-ispell
             db/update-org-agenda-files
             db/org-agenda-list-deadlines
             db/org-agenda-skip-tag
             db/cmp-date-property
             hydra-org-agenda-view/body
             org-babel-execute:hy
             db/org-timestamp-difference
             db/read-clockline
             db/org-capture-code-snippet
             hydra-org-clock/body
             db/make-org-capture-frame))

(use-package org
  :commands (org-store-link)
  :bind (:map org-mode-map
              ([remap org-return] . org-return-indent))
  :init (setq org-deadline-warning-days 14
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
              org-ellipsis "⤵"
              org-fontify-done-headline nil

              org-todo-keywords
              '((sequence "TODO(t)" "CONT(n!)" "|" "DONE(d@)")
                (sequence "GOTO(g)" "ATTN(a)" "|" "DONE(d@)")
                (sequence "READ(r)" "CONT(n!)" "|" "DONE(d@)")
                (sequence "DELG(e@/!)" "WAIT(w@/!)" "HOLD(h@/!)"
                          "|" "CANC(c@/!)" "PHONE" "MEETING"))

              org-todo-state-tags-triggers
              '(("WAIT" ("WAIT" . t))
                ("HOLD" ("HOLD" . t))
                (done ("HOLD") ("WAIT") ("DATE") ("NO_EXPORT" . t))
                ("TODO" ("HOLD") ("WAIT") ("NO_EXPORT"))
                ("READ" ("READ" . t) ("HOLD") ("WAIT"))
                ("GOTO" ("DATE" . t) ("HOLD") ("WAIT"))
                ("CONT" ("HOLD") ("WAIT"))
                ("ATTN" ("HOLD") ("WAIT")))

              org-tag-alist
              '((:startgroup . nil)
                ("WORK" . ?w)
                ("HOME" . ?h)
                ("FUN" . ?f)
                ("UNTAGGED" . ?u)
                (:endgroup . nil)
                ("NOTE" . ?n))

              org-treat-S-cursor-todo-selection-as-state-change nil
              org-fast-tag-selection-single-key 'expert

              org-global-properties
              '(("Effort_ALL" . "0:00 0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00"))

              org-columns-default-format
              "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"

              ;; Faces

              org-todo-keyword-faces
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
                ("REPEAT" :foreground "indian red" :weight normal))

              org-priority-faces
              '((?A . (:foreground "Red" :weight bold))
                (?B . (:foreground "firebrick"))
                (?C . (:foreground "tomato")))

              ;; Refiling

              org-refile-targets '((org-agenda-files . (:maxlevel . 9))
                                   (nil . (:maxlevel . 9)))
              org-refile-use-outline-path 'file
              org-refile-allow-creating-parent-nodes 'confirm
              org-indirect-buffer-display 'current-window
              org-outline-path-complete-in-steps nil
              org-refile-target-verify-function 'db/verify-refile-target

              ;; Bable

              org-babel-load-languages '((shell . t)
                                         (emacs-lisp . t)))
  :config (progn
            ;; Reset checkboxes if the RESET_CHECK_BOXES property is set
            (add-hook 'org-after-todo-state-change-hook 'org-reset-checkbox-state-maybe)

            ;; Color links to file according to whether they exist or not
            (org-link-set-parameters
             "file"
             :face (lambda (path) (if (file-exists-p path) 'org-link 'org-warning)))

            ;; File Apps

            (add-to-list 'org-file-apps '(directory . emacs))

            (add-to-list 'org-file-apps '("\\.docx?\\'" . default))
            (add-to-list 'org-file-apps '("\\.pptx?\\'" . default))
            (add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))

            (when (eq system-type 'cygwin)
              (add-to-list 'org-file-apps '(t . "cygstart %s") t))

            ;; Skip some org mode regions to be skipped by ispell
            (add-hook 'org-mode-hook #'endless/org-ispell)

            ;; Some timers

            (unless (memq #'org-clock-save
                          (mapcar #'timer--function timer-list))
              (run-with-timer 0 3600 #'org-clock-save))
            (unless (memq #'db/export-diary
                          (mapcar #'timer--function timer-idle-list))
              (run-with-idle-timer 20 t #'db/export-diary))

            ;; Drag-and-Drop images into org-mode buffer
            (use-package org-download
              :init (setq org-download-method 'attach))

            ;; Hack: The default implementation is too slow, because it is
            ;; parsing all properties of an entry by default.  Let’s simplify
            ;; this to only parse what we are looking for.  This makes tag
            ;; search *much* faster!
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
                  (org-entry-get pom property))))))

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

(defcustom db/org-clock-current-task-file "~/.org-current-task"
  "File to save the currently clocked in task to."
  :group 'personal-settings
  :type 'string)

(use-package org-clock
  :defer t
  :commands (org-clock-save)
  :init (setq org-clock-history-length 23
              org-clock-in-resume t
              org-clock-into-drawer t
              org-clock-idle-time nil
              org-clock-out-remove-zero-time-clocks t
              org-clock-out-when-done '("DONE" "CANC" "WAIT" "HOLD")
              org-clock-auto-clock-resolution 'when-no-clock-is-running
              org-clock-mode-line-total 'auto
              org-clock-report-include-clocking-task t
              org-clock-in-switch-to-state (lambda (_)
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
              org-clock-persist-query-resume nil
              org-duration-format 'h:mm
              org-time-stamp-rounding-minutes '(1 1))
  :config (progn
            (org-clock-persistence-insinuate)

            (add-hook 'org-clock-in-hook ; mark current default task
                      (lambda ()
                        (let ((current-id (org-id-get org-clock-marker)))
                          (when (member current-id (list org-working-task-id
                                                         org-home-task-id))
                            (org-clock-mark-default-task)))))

            ;; Clock in default task if no other task is given
            (add-hook 'org-clock-out-hook #'db/ensure-running-clock 'append)

            ;; Communicate the currently clocked in task to the outside world
            (add-hook 'org-clock-in-hook #'db/save-current-org-task-to-file)))

;; Agenda

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

(use-package org-agenda
  :commands (org-agenda)
  :bind (:map org-agenda-mode-map
              ("v" . hydra-org-agenda-view/body))
  :init (setq org-agenda-include-diary t
              org-agenda-span 1
              org-agenda-diary-file db/org-default-refile-file
              org-agenda-insert-diary-strategy 'top-level
              org-catch-invisible-edits 'show
              org-agenda-sorting-strategy '((agenda time-up habit-up priority-down)
                                            (todo category-keep)
                                            (tags category-keep)
                                            (search category-keep))
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
              org-agenda-search-headline-for-time nil

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

              org-stuck-projects
              '("-DATE-HOLD-REGULAR-HOLD-NOTE+TODO=\"\""
                ("CONT" "TODO" "READ" "WAIT" "GOTO" "DELG")
                ("NOP")
                "")

              org-agenda-prefix-format
              '((agenda . "%11s%?-12t")
                (todo . "%-8c ")
                (tags . "%-8c ")
                (search . "%-8c "))

              org-agenda-custom-commands
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
  :config (progn
            ;; avoid important buffers to end up in `org-agenda-new-buffers’ by
            ;; opening them manually
            (mapc #'find-file-noselect org-agenda-files)

            (add-hook 'org-agenda-mode-hook #'hl-line-mode 'append)))

;; Capturing

(use-package org-capture
  :commands (org-capture)
  :init (setq org-capture-use-agenda-date nil
              org-capture-templates
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
                 :immediate-finish t)
                ("s" "Code Snippet"
                 entry
                 (file db/org-default-refile-file)
                 "* %?\n%(db/org-capture-code-snippet \"%F\")")
                ;; http://www.howardism.org/Technical/Emacs/capturing-content.html
                ("c" "Note for currently clocked in task"
                 item
                 (clock)
                 "%i%?"
                 :empty-lines 1)
                ("K" "Kill-ring to currently clocked in task"
                 plain
                 (clock)
                 "%c"
                 :immediate-finish t :empty-lines 1)))
  :config (progn
            ;; disable usage of helm for `org-capture'
            (with-eval-after-load 'helm-mode
              (defvar helm-completing-read-handlers-alist) ; for the byte compiler
              (add-to-list 'helm-completing-read-handlers-alist
                           '(org-capture . nil)))))

;; Babel

(use-package ob-core
  :defer t
  :init (setq org-export-use-babel nil)
  :config (setf (alist-get :results org-babel-default-header-args)
                "output code replace"))

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
  :defer t
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

(use-package ox-latex
  :defer t
  :init (setq org-latex-default-class "scrartcl"
              org-latex-listings t
              org-latex-compiler "lualatex")
  :config (progn
            (add-to-list 'org-latex-classes
                         `("scrartcl"
                           ,(concat "\\documentclass[parskip=half,colorlinks]{scrartcl}\n"
                                    "[DEFAULT-PACKAGES]"
                                    "[PACKAGES]"
                                    "
\\lstset{
  basewidth=0.5em,
  keywordstyle=\\textcolor{blue!80!white},
  basicstyle=\\ttfamily,
  commentstyle={\\itshape},
  frame=tb,
  showspaces=false,
  showtabs=false,
  showstringspaces=false,
}
"
                                    "[EXTRA]\n")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
            (add-to-list 'org-latex-classes
                         `("beamer"
                           ,(concat "\\documentclass[presentation]{beamer}\n"
                                    "[DEFAULT-PACKAGES]"
                                    "[PACKAGES]"
                                    "
\\lstset{
  basewidth=0.5em,
  keywordstyle=\\textcolor{blue!80!white},
  basicstyle=\\ttfamily,
  commentstyle={\\itshape},
  frame=tb,
  showspaces=false,
  showtabs=false,
  showstringspaces=false,
}
"
                                    "[EXTRA]\n")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
            (add-to-list 'org-latex-packages-alist
                         '("" "listings"))
            (add-to-list 'org-latex-packages-alist
                         '("" "xcolor"))))

(use-package ox-html
  :defer t
  :init (setq org-html-postamble nil))

(use-package ox-reveal
  :defer t
  :config (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/"
                org-reveal-mathjax t
                org-reveal-transition "none"))


;; * Mail

(use-package bbdb
  :commands (bbdb-search-name bbab-initialize bbdb-mua-auto-update-init bbdb-save)
  :init (setq bbdb-completion-display-record nil
              bbdb-complete-mail-allow-cycling t
              bbdb-mua-auto-update-p 'query
              bbdb-default-country "Germany"
              bbdb-user-mail-address-re (regexp-opt
                                         (cons user-mail-address
                                               db/additional-mail-addresses)))
  :config (progn
            (add-hook 'message-setup-hook 'bbdb-mail-aliases)
            (add-hook 'mail-setup-hook 'bbdb-mail-aliases)
            (run-with-timer 0 3600 #'bbdb-save)))

(use-package gnus
  :defines (gnus-init-file)
  :commands (gnus)
  :init (setq gnus-init-file (expand-file-name "gnus.el" emacs-d)
              gnus-home-directory (expand-file-name "~/Mail/news/")
              gnus-directory (expand-file-name "~/Mail/news/")
              gnus-kill-files-directory gnus-directory
              gnus-startup-file (expand-file-name "~/Mail/gnus-newsrc")
              gnus-cache-directory (expand-file-name "cache/" gnus-directory))
  :config (progn
            (eval-when-compile
              (require 'gnus-start))
            (bbdb-initialize 'gnus 'message)
            (bbdb-mua-auto-update-init 'message)))


;; * Crypto

(use-package nsm
  :defer t
  :init (setq network-security-level 'high
              nsm-save-host-names t
              nsm-settings-file (expand-file-name
                                 "~/.emacs.d/private/network-security.data"))
  :config (advice-add 'nsm-write-settings
                      :before #'db/sort-nsm-permanent-settings))

(use-package gnutls
  :defer t
  :init (setq gnutls-log-level 0        ; too noisy otherwise
              gnutls-min-prime-bits 1024
              gnutls-verify-error t))

(defcustom db/cert-file-directory "~/.local/etc/certs/"
  "Local directory with additional certificates."
  :group 'personal-settings
  :type 'string
  :set #'db/update-cert-file-directory)

(use-package epg
  :defer t
  :init (setq epg-debug t
              epg-gpg-program "gpg"))


;; * Appearance

(use-package solarized-theme
  :defer t
  :init (setq solarized-use-less-bold t
              solarized-emphasize-indicators t
              solarized-use-variable-pitch nil))

(use-package smart-mode-line
  :init (setq sml/mode-width 'full
              sml/name-width 30
              sml/theme 'respectful)
  :commands (sml/setup))


;; * Dired

(use-package dired
  :defer t
  :bind (:map dired-mode-map
              ("e" . ora-ediff-files)
              ("z" . dired-get-size)
              ([remap beginning-of-buffer] . dired-back-to-top)
              ([remap end-of-buffer] . dired-jump-to-bottom)
              ("<f1>" . nil))
  :init (progn
          (setq dired-dwim-target t
                dired-listing-switches "-alh"
                dired-hide-details-hide-information-lines t
                dired-hide-details-hide-symlink-targets t
                dired-recursive-copies 'top
                dired-recursive-deletes 'top

                ;; Don’t use obsolete diredx local variables
                dired-enable-local-variables nil
                dired-local-variables-file nil

                dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"
                diredp-hide-details-initially-flag t

                wdired-create-parent-directories t
                wdired-allow-to-change-permissions t)

          (if on-windows
              (setq dired-guess-shell-alist-user '((".*" "cmd /c")))
            (setq dired-guess-shell-alist-user
                  '(("\\.pdf\\'" "evince")
                    ("\\.ps\\'" "evince")
                    ("\\.\\(?:djvu\\|eps\\)\\'" "evince")
                    ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "eog")
                    ("\\.\\(?:xcf\\)\\'" "gimp")
                    ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|webm\\)\\(?:\\.part\\)?\\'"
                     "vlc")
                    ("\\.\\(?:mp3\\|flac\\|ogg\\)\\'" "mplayer")
                    ("\\.html?\\'" "firefox")
                    ("\\.docx?\\'" "loffice"))))

          (when on-windows
            (setq directory-free-space-program nil)))
  :config (progn
            (put 'dired-find-alternate-file 'disabled nil)

            (require 'dired-x)
            (with-demoted-errors "Non-Fatal Error: %s"
              (require 'dired+)
              (when (and (eq system-type 'windows-nt)
                         (not (package-installed-p 'w32-browser)))
                (warn "`w32-browser’ not installed, dired will have reduced functionality."))
              (when (and (require 'dired-open)
                         (eq system-type 'gnu/linux))
                (bind-key "M-RET" #'dired-open-xdg dired-mode-map)))

            ;; Gnus support in dired
            (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

            ;; omitting files
            (add-hook 'dired-mode-hook 'dired-omit-mode)
            (dolist (extension '(".out" ".synctex.gz" ".thm"))
              (add-to-list 'dired-latex-unclean-extensions extension))

            ;; disable exaggerated fontification of dired+
            (require 'font-lock)
            (add-to-list 'font-lock-maximum-decoration '(wdired-mode . 1))
            (add-to-list 'font-lock-maximum-decoration '(dired-mode . 1))

            (defun ora-ediff-files ()
              "Compare marked files in dired with ediff."
              ;; from: https://oremacs.com/2017/03/18/dired-ediff/
              (interactive)
              (lexical-let ((files (dired-get-marked-files))
                            (wnd (current-window-configuration)))
                (if (<= (length files) 2)
                    (lexical-let ((file1 (car files))
                                  (file2 (if (cdr files)
                                             (cadr files)
                                           (read-file-name
                                            "file: "
                                            (dired-dwim-target-directory)))))
                      (if (file-newer-than-file-p file1 file2)
                          (ediff-files file2 file1)
                        (ediff-files file1 file2))
                      (add-hook 'ediff-after-quit-hook-internal
                                (lambda ()
                                  (setq ediff-after-quit-hook-internal nil)
                                  (set-window-configuration wnd))))
                  (error "No more than 2 files should be marked"))))

            (defun dired-back-to-top ()
              "Jump to first non-trivial line in dired."
              (interactive)
              (goto-char (point-min))
              (dired-next-line 1))

            (defun dired-jump-to-bottom ()
              "Jump to last non-trivial line in dired."
              (interactive)
              (goto-char (point-max))
              (dired-next-line -1))

            (defun dired-get-size ()    ; from emacswiki, via oremacs
              "Print size of all files marked in the current dired buffer."
              (interactive)
              (let ((files (dired-get-marked-files)))
                (with-temp-buffer
                  (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
                  (message
                   "size of all marked files: %s"
                   (progn
                     (re-search-backward "\\(^[0-9.,]+[a-za-z]+\\).*total$")
                     (match-string 1))))))))

(use-package find-dired
  :commands (find-dired)
  :init (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(use-package dired-open
  :config (add-to-list 'dired-open-functions #'dired-open-guess-shell-alist))

(use-package gnus-dired
  :commands (turn-on-gnus-dired-mode))


;; * Completion

(use-package hippie-exp
  :commands (hippie-expand))

(use-package helm-config
  :init (setq helm-command-prefix-key "C-c h"))

(use-package helm
  :commands (helm-show-kill-ring)
  :diminish helm-mode
  :bind (:map helm-command-map
              ("#" . helm-emms)
              ("P" . helm-pages))
  :init (setq helm-input-idle-delay 0.0
              helm-buffers-fuzzy-matching t
              helm-mode-fuzzy-match t
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
              helm-mini-default-sources '(helm-source-buffers-list
                                          helm-source-recentf
                                          db/helm-frequently-used-features
                                          db/helm-frequently-visited-locations
                                          helm-source-buffer-not-found
                                          helm-source-bookmarks
                                          helm-source-bookmark-set))
  :config (progn
            (eval-when-compile
              (require 'helm-mode)
              (require 'helm-buffers)
              (require 'helm-ring))

            (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
            (bind-key "C-i" #'helm-execute-persistent-action helm-map)
            (bind-key "C-z" #'helm-select-action helm-map)))

(use-package helm-org
  :commands (helm-org-agenda-files-headings)
  :bind (:map helm-org-headings-map
              ("C-c c" . helm-org-clock-in-at-heading))
  :config (progn
            ;; Add action to clock in at current heading to
            ;; `helm-org-agenda-files-headings’

            (defun helm-org--clock-in-at-heading (marker)
              "Clock in to current heading at MARKER."
              (org-with-point-at marker
                (org-clock-in)))

            (defun helm-org-clock-in-at-heading ()
              (interactive)
              (with-helm-alive-p
                (helm-exit-and-execute-action 'helm-org--clock-in-at-heading)))

            (add-to-list 'helm-org-headings-actions
                         '("Clock in to this heading"
                           . helm-org--clock-in-at-heading)
                         t)))

(use-package ivy
  :commands (ivy-mode
             ivy-resume)
  :diminish ivy-mode
  :init (setq ivy-use-virtual-buffers t
              enable-recursive-minibuffers t
              ivy-magic-tilde nil
              ivy-count-format "(%d/%d) "
              ivy-format-function #'(lambda (cands)
                                      (ivy--format-function-generic
                                       (lambda (str)
                                         (concat "→ " str))
                                       (lambda (str)
                                         (concat "  " str))
                                       cands
                                       "\n"))
              ivy-initial-inputs-alist '((counsel-describe-function . "^")
                                         (counsel-describe-variable . "^")
                                         (man . "^")
                                         (woman . "^"))
              ivy-use-selectable-prompt t)
  :config (add-to-list 'ivy-completing-read-handlers-alist
                       '(org-capture . completing-read-default)))

(use-package counsel
  :commands (counsel-org-goto-all
             counsel-ag
             counsel-M-x
             counsel-find-file
             counsel-info-lookup-symbol
             counsel-unicode-char
             counsel-describe-variable
             counsel-describe-function
             counsel-recentf))

(use-package swiper
  :commands (swiper
             swiper-from-isearch))

(use-package recentf
  :commands (recentf-mode recentf-save-list)
  :init (setq recentf-max-saved-items 1000)
  :config (run-with-timer 0 3600 #'recentf-save-list))

(use-package company
  :commands (company-mode global-company-mode)
  :init (setq company-show-numbers t))


;; * Navigation

(use-package ace-window
  :commands (ace-window ace-window-display-mode)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-background nil
              aw-leading-char-style 'char
              aw-scope 'frame))

(use-package avy
  :commands (avy-goto-char-timer
             avy-goto-word-or-subword-1
             avy-goto-line))

(use-package dumb-jump
  :commands (dumb-jump-go-other-window
             dumb-jump-go
             dumb-jump-back
             dumb-jump-quick-look
             dumb-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window)
  :init (setq dumb-jump-selector 'helm))

(use-package helm-pages
  :commands (helm-pages))

(use-package eyebrowse
  :commands (eyebrowse-mode)
  :init (setq eyebrowse-keymap-prefix (kbd "C-c w")))


;; * Media

(use-package emms
  :commands (emms
             emms-pause
             emms-stop
             emms-next
             emms-previous)
  :bind (:map emms-playlist-mode-map
              ("S s" . emms-shuffle))
  :init (setq emms-source-file-default-directory "~/Documents/media/audio/"
              emms-player-list '(emms-player-mplayer emms-player-mplayer-playlist)
              emms-show-format "NP: %s"
              emms-stream-default-action "play"
              emms-track-description-function 'db/emms-track-description
              emms-playlist-default-major-mode 'emms-playlist-mode)
  :config (progn

            ;; Initialization copied and adapted from `emms-setup’

            (require 'emms-source-file)
            (require 'emms-source-playlist)
            (require 'emms-player-simple)
            (require 'emms-player-mplayer)
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
                        :after (lambda (&rest r)
                                 (ignore r)
                                 (delete-window)))

            (unless (eq system-type 'windows-nt)
              (setq emms-source-file-directory-tree-function
                    #'db/emms-source-file-directory-tree-find))

            ;; `emms-playlist-mode’ sets `emms-playlist-insert-track-function’,
            ;; no matter what previous values or customization may say otherwise
            ;; … so we need to employ a hook to change its value
            (add-hook 'emms-playlist-mode-hook
                      (lambda ()
                        (setq emms-playlist-insert-track-function
                              #'db/emms-playlist-mode-insert-track)))

            (run-with-timer 0 3600 #'emms-cache-save)))

;; Make sure emms is up and running when we call functions such as
;; `emms-play-dired’ etc.
(use-package emms-source-file
  :defer t
  :config (require 'emms))

(use-package db-emms
  :commands (db/play-playlist
             db/emms-source-file-directory-tree-find
             db/emms-track-description
             db/emms-playlist-mode-insert-track
             emms-control/body))

(use-package helm-emms
  :commands (helm-emms)
  :init (setq helm-emms-use-track-description-function t
              helm-emms-default-sources '(helm-source-emms-streams
                                          helm-source-emms-dired
                                          helm-source-emms-files))
  :config (progn
            (require 'emms)
            (require 'helm-adaptive)))


;; * Shells and such

(use-package comint
  :defer t
  :init (setq comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output nil
              comint-scroll-show-maximum-output t
              comint-completion-addsuffix t
              comint-buffer-maximum-size 100000
              comint-input-ring-size 5000))

(use-package term
  :commands (term-send-string)
  :init (setq explicit-shell-file-name shell-file-name)
  :config (progn
            (add-hook 'term-exec-hook   ; oremacs.com
                      (lambda ()
                        (let* ((buff (current-buffer))
                               (proc (get-buffer-process buff)))
                          (set-process-sentinel
                           proc
                           `(lambda (process event)
                              (if (string= event "finished\n")
                                  (kill-buffer ,buff)))))))

            ;; does not work; C-c is shadowed by some minor modes like semantic,
            ;; projectile, and winner
            (bind-key "C-c" #'term-send-raw term-raw-map)

            ;; unbind some keys to allow the global keymap to handle them
            (unbind-key "M-:" term-raw-map)
            (unbind-key "C-h" term-raw-map)
            (unbind-key "M-x" term-raw-map)
            (unbind-key "M-o" term-raw-map)

            ;; we need to set keys starting with C-x after `ansi-term' has been
            ;; called, as it resets the escape character to C-x.
            (defadvice ansi-term (after ansi-term-set-keys activate)
              (unbind-key "C-x C-j" term-raw-map)
              (unbind-key "C-x g" term-raw-map))

            (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))))

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
            (add-hook 'shell-mode-hook 'with-editor-export-editor)))

(use-package db-eshell
  :commands (eshell-clear-buffer
             eshell/default-prompt-function
             eshell/gst
             eshell-insert-history
             pcomplete/git))

(use-package eshell
  :commands (eshell)
  :init (setq eshell-cmpl-cycle-completions nil
              eshell-scroll-to-bottom-on-input t
              eshell-prefer-lisp-functions nil
              eshell-error-if-no-glob t
              eshell-hist-ignoredups t
              eshell-save-history-on-exit t
              eshell-destroy-buffer-when-process-dies t
              eshell-prompt-function #'eshell/default-prompt-function
              eshell-prompt-regexp "└─[$#] "
              eshell-highlight-prompt nil)
  :config (progn (require 'em-prompt)
                 (require 'em-term)
                 (require 'em-cmpl)

                 (setenv "PAGER" "cat")

                 (add-to-list 'eshell-command-completions-alist
                              '("gunzip" "gz\\'"))

                 (add-to-list 'eshell-command-completions-alist
                              '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

                 (add-hook 'eshell-mode-hook
                           'with-editor-export-editor)

                 (add-hook 'eshell-mode-hook
                           (lambda ()
                             (bind-key "C-a" #'eshell-bol eshell-mode-map)
                             (bind-key "C-l" #'eshell-clear-buffer eshell-mode-map)
                             (bind-key "M-r" #'eshell-insert-history eshell-mode-map)
                             (bind-key "M-P" #'eshell-previous-prompt eshell-mode-map)
                             (bind-key "M-N" #'eshell-next-prompt eshell-mode-map)))

                 ;; Ignoring case when completing file names seems to have a
                 ;; bug: when a ~ is encountered, it is implicitly expaned by
                 ;; `pcomplete-insert-entry’, overwriting the prompt as a side
                 ;; effect.  Keeping the case as it is does not seem to have
                 ;; this issue.  This problem occurs by default only on Windows
                 ;; systems (in all flavors), because this is the only time
                 ;; `pcomplete-ignore-case’ is non-nil by default.
                 (when on-windows
                   (add-to-list 'eshell-mode-hook
                                (lambda ()
                                  (setq pcomplete-ignore-case nil))))


                 (require 'db-eshell)))

(use-package with-editor
  :commands (with-editor-export-editor))


;; * Lisp

;; General Stuff first

(use-package lispy
  :commands (lispy-mode)
  :diminish lispy-mode)

(use-package eldoc
  :commands (eldoc-mode)
  :diminish eldoc-mode)

;; Lisp Dialects

(use-package elisp-mode
  :defer t
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
            (add-hook 'emacs-lisp-mode-hook 'db/add-use-package-to-imenu)
            (add-hook 'ielm-mode-hook 'eldoc-mode)
            (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)))

(use-package geiser
  :commands (geiser-mode))

(use-package cider
  :commands (cider-jack-in)
  :init (setq nrepl-hide-special-buffers t
              cider-auto-select-error-buffer t
              cider-stacktrace-default-filters '(tooling dup)
              cider-stacktrace-fill-column 80
              cider-save-file-on-load nil
              cider-repl-result-prefix ";; => "
              cider-repl-use-clojure-font-lock t
              cider-repl-wrap-history t
              cider-repl-history-size 1000
              ;;cider-lein-parameters "trampoline repl :headless"
              cider-lein-parameters "repl :headless"
              cider-repl-history-file (expand-file-name ".cider-history" emacs-d)
              cider-repl-display-help-banner nil
              cider-cljs-lein-repl "(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))")
  :config (progn
            (add-hook 'cider-repl-mode-hook 'subword-mode)
            (add-hook 'cider-repl-mode-hook 'lispy-mode)
            (add-hook 'cider-repl-mode-hook 'cider-repl-toggle-pretty-printing)
            (add-hook 'cider-repl-mode-hook 'company-mode)
            (add-hook 'cider-mode-hook 'eldoc-mode)))

(use-package clojure-mode
  :defer t
  :config (progn
            (define-clojure-indent
              (forall 'defun)
              (exists 'defun)
              (dopar 'defun))
            (add-hook 'clojure-mode-hook 'lispy-mode)
            (add-hook 'clojure-mode-hook 'clj-refactor-mode)
            (add-hook 'clojure-mode-hook 'yas-minor-mode)
            (add-hook 'clojure-mode-hook 'company-mode)))

(use-package clj-refactor
  :commands (clj-refactor-mode)
  :config (progn
            (cljr-add-keybindings-with-prefix "C-c C-m")
            (setq cljr-eagerly-build-asts-on-startup nil
                  cljr-warn-on-eval nil)))

(use-package slime
  :commands (slime slime-mode slime-connect)
  :init     (progn
              (setq inferior-lisp-program "sbcl --noinform --no-linedit"
                    slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/")
                    slime-net-coding-system 'utf-8-unix
                    slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
                    slime-lisp-implementations '((sbcl ("sbcl") :coding-system utf-8-unix)
                                                 (cmucl ("cmucl") :coding-system utf-8-unix)
                                                 (ccl ("ccl") :coding-system utf-8-unix))
                    slime-repl-history-remove-duplicates t
                    slime-repl-history-trim-whitespaces t)
              (add-hook 'lisp-mode-hook '(lambda () (slime-mode +1)) t))
  :config   (progn
              (make-directory "/tmp/slime-fasls/" t)
              (slime-setup '(slime-repl slime-fancy slime-autodoc))
              (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)))

(use-package hy-mode
  :commands (hy-mode)
  :config (progn
            (add-hook 'hy-mode-hook 'lispy-mode)
            (add-hook 'hy-mode-hook 'inferior-lisp)))


;; * TeX

(use-package reftex
  :commands (turn-on-reftex)
  :init (setq reftex-plug-into-AUCTeX t)
  :config (with-eval-after-load 'helm-mode
            (add-to-list 'helm-completing-read-handlers-alist
                         '(reftex-citation . nil))))

(use-package tex
  :defer t
  :init (setq TeX-auto-save t
              TeX-save-query nil
              TeX-parse-self t
              TeX-master t
              TeX-electric-sub-and-superscript t
              TeX-electric-math '("$" . "$")
              TeX-electric-escape nil
              LaTeX-electric-left-right-brace t
              LaTeX-fill-break-at-separators nil
              TeX-fold-math-spec-list '(("≤" ("le"))
                                        ("≥" ("ge"))
                                        ("∉" ("notin")))

              TeX-source-correlate-start-server nil

              LaTeX-eqnarray-label "eqn:"
              LaTeX-equation-label "eqn:"
              LaTeX-figure-label "fig:"
              LaTeX-table-label "tab:"
              TeX-newline-function 'reindent-then-newline-and-indent
              LaTeX-section-hook '(LaTeX-section-heading
                                   LaTeX-section-title
                                   LaTeX-section-section
                                   LaTeX-section-label))
  :config (progn

            (require 'latex)
            (require 'tex-buf)
            (require 'reftex)

            (TeX-engine-set 'default)

            (put 'TeX-narrow-to-group 'disabled nil)
            (put 'LaTeX-narrow-to-environment 'disabled nil)

            (add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
            (add-hook 'LaTeX-mode-hook #'turn-on-visual-line-mode)
            (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
            (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
            (add-hook 'LaTeX-mode-hook #'page-break-lines-mode)
            (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
            (add-hook 'latex-mode-hook #'turn-on-reftex)

            (add-hook 'LaTeX-mode-hook '(lambda ()
                                          (TeX-PDF-mode 1)
                                          (TeX-source-correlate-mode 1)
                                          (TeX-fold-mode 1)))


            (add-to-list 'TeX-view-program-selection
                         '(output-pdf "Evince"))

            ;; use pdf-tools when loaded
            (with-eval-after-load 'pdf-tools
              (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
              (add-to-list 'TeX-after-compilation-finished-functions
                           #'TeX-revert-document-buffer))

            ;; style used for my personal definitions; not clear whether this
            ;; works as intended
            (TeX-add-style-hook
             "mydefs"
             (lambda ()
               (TeX-run-style-hooks "etex"
                                    "etoolbox"
                                    "ifthen"
                                    "amsmath"
                                    "amssymb"
                                    "latexsym"
                                    "mathabx"
                                    "stmaryrd"
                                    "verbatim"
                                    "graphicx"
                                    "enumerate"
                                    "array"
                                    "booktabs"
                                    "ulem"
                                    "nicefrac"
                                    "listings"
                                    "microtype"
                                    "tabularx"
                                    "tikz"
                                    "csquotes"
                                    "ntheorem"
                                    "xspace")
               (LaTeX-add-environments
                '("Exercise" LaTeX-env-label)
                '("Theorem" LaTeX-env-label)
                '("Proposition" LaTeX-env-label)
                '("Lemma" LaTeX-env-label)
                '("Corollary" LaTeX-env-label)
                '("Remark" LaTeX-env-label)
                '("Example" LaTeX-env-label)
                '("Definition" LaTeX-env-label)
                '("Proof" LaTeX-env-label))

               ;; https://tex.stackexchange.com/questions/217799/auctex-11-88-bug-on-latex-env-label-cannot-automatically-insert-label
               (setf (cadr reftex-insert-label-flags)
                     (concat (cadr reftex-insert-label-flags) "TLPDRCE"))

               (dolist (label-spec
                        '(("Theorem" ?T "thm:" "~\\ref{%s}" t ("Theorem" "Thm.") nil)
                          ("Lemma" ?L "lem:" "~\\ref{%s}" t ("Lemma" "Lem.") nil)
                          ("Proposition" ?P "prop:" "~\\ref{%s}" t ("Proposition" "Prop.") nil)
                          ("Satz" ?T "thm:" "~\\ref{%s}" t ("Satz") nil)
                          ("Definition" ?D "def:" "~\\ref{%s}" t ("Definition" "Def.") nil)
                          ("Remark" ?R "rem:" "~\\ref{%s}" t ("Remark" "Rem.") nil)
                          ("Corollary" ?C "cor:" "~\\ref{%s}" t ("Corollary" "Cor.") nil)
                          ("Example" ?E "expl:" "~\\ref{%s}" t ("Example") nil)))
                 (add-to-list 'reftex-label-alist label-spec)
                 (add-to-list 'LaTeX-label-alist (cons (nth 0 label-spec)
                                                       (nth 2 label-spec))))))

            ;; Add completion for cleverref’s reference macros; not clear
            ;; whether this works as intended
            (TeX-add-style-hook
             "cleveref"
             (lambda ()
               (add-to-list 'reftex-ref-style-alist
                            '("Cleveref" "cleveref"
                              (("\\cref" ?c) ("\\Cref" ?C)
                               ("\\cpageref" ?d) ("\\Cpageref" ?D))))
               (reftex-ref-style-activate "Cleveref")
               (TeX-add-symbols
                '("cref" TeX-arg-ref)
                '("Cref" TeX-arg-ref)
                '("cpageref" TeX-arg-ref)
                '("Cpageref" TeX-arg-ref))))

            ;; Language definitions
            (add-hook 'TeX-language-de-hook
                      (lambda () (ispell-change-dictionary "de_DE")))
            (add-hook 'TeX-language-en-hook
                      (lambda () (ispell-change-dictionary "en_US")))
            (add-hook 'TeX-mode-hook
                      (lambda () (setq ispell-parser 'tex)))))

(use-package ebib
  :commands (ebib))

(use-package helm-bibtex
  :commands (helm-bibtex))


;; * Various Mode Configurations

;; These are packages that are not essential, but still nice to have.  They
;; provide optional functionality and may redefine builtin commands.

(use-package cperl-mode
  :commands (cperl-mode)
  :init (progn
          ;; replace perl-mode with cperl-mode
          (mapc
           (lambda (pair)
             (if (eq (cdr pair) 'perl-mode)
                 (setcdr pair 'cperl-mode)))
           (append auto-mode-alist interpreter-mode-alist))
          (setq cperl-hairy nil))
  :config (progn
            (add-hook 'cperl-mode-hook 'flycheck-mode)
            (add-hook 'cperl-mode-hook 'prettify-symbols-mode)))

(use-package crux
  :commands (crux-eval-and-replace
             crux-smart-open-line-above
             crux-kill-whole-line
             crux-cleanup-buffer-or-region
             crux-delete-buffer-and-file))

(use-package db-projects
  :commands (projects-add-project projects-archive-project))

(use-package define-word
  :commands (define-word-at-point define-word))

(use-package dictcc
  :commands (dictcc)
  :config (require 'gnutls))

(use-package edit-list
  :commands edit-list)

(use-package electric
  :commands (electric-quote-mode))

(use-package elec-pair
  :commands (electric-pair-mode)
  :config   (progn
              (add-to-list 'electric-pair-pairs '(?“ . ?”))
              (add-to-list 'electric-pair-text-pairs '(?“ . ?”))
              (add-to-list 'electric-pair-pairs '(?„ . ?“))
              (add-to-list 'electric-pair-text-pairs '(?„ . ?“))))

(use-package expand-region
  :commands (er/expand-region))

(use-package flycheck
  :commands (global-flycheck-mode flycheck-mode))

(use-package flyspell
  :commands (flyspell-mode turn-on-flyspell)
  :config (progn
            (unbind-key "C-M-i" flyspell-mode-map)
            (unbind-key "C-c $" flyspell-mode-map)))

(use-package haskell-mode
  :defer t
  :defines (haskell-program-name)
  :init (setq haskell-program-name "ghci")
  :config (progn
            (add-hook 'haskell-mode-hook 'haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            (add-hook 'haskell-mode-hook
                      (lambda ()
                        (company-mode +1)
                        (set (make-local-variable 'company-backends)
                             (append '((company-capf company-dabbrev-code))
                                     company-backends))))
            (add-hook 'haskell-mode-hook 'flycheck-mode)

            (with-demoted-errors "Non-Fatal Error: %s"
              (require 'haskell-indentation)
              (add-hook 'haskell-mode-hook
                        'haskell-indentation-mode))

            (add-hook 'haskell-mode-hook
                      'interactive-haskell-mode)))

(use-package ivy-pages
  :commands (ivy-pages))

(use-package highlight-indentation
  :commands highlight-indentation-mode)

(use-package iedit
  :commands (iedit-mode))

(use-package key-chord
  :commands (key-chord-mode)
  :config (progn
            (key-chord-define-global "``" "“")
            (key-chord-define-global "''" "”")
            (key-chord-define-global ",," "„")))

(use-package mastodon
  :commands (mastodon))

(use-package multiple-cursors
  :commands (mc/edit-lines
             mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this))

(use-package org-ref
  :defer t
  :config (progn
            (require 'org-ref-pdf)
            (require 'org-ref-url-utils)))

(use-package page-break-lines
  :commands (page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package pdf-occur
  :commands (pdf-occur-global-minor-mode))

(use-package pdf-tools
  :commands (pdf-tools-install))

(use-package python
  :defer t
  :init (setq python-indent-offset 2)
  :config (progn
            (add-hook 'python-mode-hook 'highlight-indentation-mode)
            (when (package-installed-p 'elpy)
              (elpy-enable))))

(use-package semantic
  :commands (semantic-mode)
  :config (progn
            (require 'semantic/ia)
            (require 'semantic/bovine/el)

            ;; recognize `use-package' as include statement; the function seems
            ;; to have to be a byte-compiled function, for otherwise it just
            ;; won’t work … ?
            (eval `(semantic-elisp-setup-form-parser
                       ,(lambda (form start end)
                          (ignore start end)
                          (semantic-tag-new-include (symbol-name (nth 1 form))
                                                    nil))
                     use-package))))

(use-package sh-scripts
  :defer t
  :init (setq sh-basic-offset 2
              sh-indentation 2))

(use-package synonyms
  :commands (synonyms))

(use-package timeline-tools
  :load-path "site-lisp"
  :commands (timeline-tools-format-timeline
             timeline-tools-format-timeline-of-day
             timeline-tools-copy-clocklines))

(use-package typing
  :commands (typing-of-emacs)
  :init (setq toe-highscore-file nil))

(use-package undo-tree
  :commands (global-undo-tree-mode
             undo
             undo-tree-redo)
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t)
  :diminish undo-tree-mode)

(use-package vlf-setup)

(use-package wgrep
  :commands (wgrep-finish-edit
             wgrep-change-to-wgrep-mode))

(use-package which-key
  :commands (which-key-mode)
  :diminish which-key-mode
  :init (setq which-key-side-window-max-width 0.33
              which-key-side-window-max-height 0.25)
  :config (which-key-setup-side-window-bottom))

(use-package yasnippet
  :commands (yas-minor-mode-on yas-minor-mode)
  :diminish yas-minor-mode
  :config (yas-reload-all))

;;; init.el ends here
