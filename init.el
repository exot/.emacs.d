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

(defconst emacs-d (file-name-directory
                   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

(defconst on-windows (memq system-type '(windows-nt cygwin))
  "Non-nil if and only if this instance of Emacs runs on Windows.")

(setq custom-file (expand-file-name "private/custom.el" emacs-d)
      custom-theme-directory (expand-file-name "themes/" emacs-d))

(add-to-list 'load-path (expand-file-name "site-lisp" emacs-d))

;; Ensure that ~/.emacs.d/private exists, because we want to store data there
(let ((private-data-dir (expand-file-name "private/" emacs-d)))
  (unless (file-directory-p private-data-dir)
    (make-directory private-data-dir)))


;; * Packages

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(when (< emacs-major-version 27)
  ;; Before Emacs 27.1, we had to do package initialization ourselves.  In Emacs
  ;; 27.1 and later, it's done directly after loading early-init.el.  See
  ;; https://www.gnu.org/software/emacs/news/NEWS.27.1
  (load-file (expand-file-name "early-init.el" emacs-d))
  (package-initialize))

(eval-when-compile
  (setq use-package-enable-imenu-support t
        use-package-always-defer t
        use-package-verbose t
        use-package-minimum-reported-time 0.01)

  (dolist (package '(bind-key use-package))
    (unless (package-installed-p package)
      (package-install package))
    (require package)))

(add-to-list 'package-pinned-packages '(use-package . "melpa-stable"))
(add-to-list 'package-pinned-packages '(bind-key . "melpa-stable"))

(put 'use-package 'lisp-indent-function 1)


;; * Mode activation

(defun db/run-init ()
  "Run main initialization after everything is set up."

  (message "Running main initialization ...")

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

  (size-indication-mode 1)
  (display-battery-mode -1)

  (electric-pair-mode +1)

  (recentf-mode t)
  (winner-mode 1)
  (global-auto-revert-mode -1)
  (which-function-mode +1)
  (global-eldoc-mode +1)

  ;; Activate modes (packages)

  (dolist (mode '(global-undo-tree-mode
                  ace-window-display-mode
                  key-chord-mode
                  ivy-mode
                  minions-mode
                  which-key-mode
                  projectile-mode
                  yas-global-mode
                  global-git-commit-mode))
    (with-demoted-errors "Cannot activate mode: %s"
      (funcall mode +1)))

  ;; This causes inacceptable lack when drawing buffers, so disable it for now.
  ;; Needs to be investigated further.

  ;; (with-demoted-errors "Cannot activate moody: %s"
  ;;   (moody-replace-mode-line-buffer-identification)
  ;;   (moody-replace-vc-mode))

  (with-demoted-errors "Cannot activate `vlf': %s"
    (require 'vlf-setup))

  ;; Explicitly require helm, because autoloading is difficult with helm's
  ;; separate `helm-command-prefix-key' mechanism.
  (require 'helm)

  (when (package-installed-p 'org-roam)
    (org-roam-db-autosync-mode))

  ;; Global Hooks

  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (add-hook 'prog-mode-hook 'subword-mode)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'lisp-mode-hook 'turn-on-lispy-when-available)

  (when (<= 24 emacs-major-version)
    (add-hook 'prog-mode-hook 'electric-indent-local-mode))

  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'text-mode-hook 'abbrev-mode)
  (add-hook 'text-mode-hook 'hl-line-mode)

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
  (bind-key "C-c C-r" #'ivy-resume)
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
  (bind-key "C-c o" #'hydra-org-clock/body)
  (bind-key "C-c s" #'synonyms)
  (bind-key "C-c t" #'hydra-toggle/body)
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
  (bind-key "M-SPC" #'cycle-spacing)
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

  (when (package-installed-p 'counsel)
    (bind-key "M-x" #'counsel-M-x)      ; gets nicer sorting with smex installed
    (bind-key "C-c r" #'counsel-recentf)
    (bind-key "C-x C-f" #'counsel-find-file)
    (bind-key "C-h f" #'counsel-describe-function)
    (bind-key "C-h v" #'counsel-describe-variable)
    (bind-key "C-h b" #'counsel-descbinds)
    (bind-key "C-S-s" #'counsel-grep-or-swiper))

  (when (package-installed-p 'helm)
    (bind-key "M-y" #'helm-show-kill-ring))

  (when (package-installed-p 'crux)
    (bind-key [remap kill-whole-line] #'crux-kill-whole-line)
    (bind-key [remap open-line] #'crux-smart-open-line-above))

  (when (package-installed-p 'ace-window)
    (bind-key "C-x o" #'ace-window))

  (when (package-installed-p 'avy)
    (bind-key "M-g M-g" #'avy-goto-line)
    (bind-key "M-g g" #'avy-goto-line))

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
        (cl-ecase (server-running-p)
          ((t) t)                       ; server is running
          (nil (warn "Server not running, check logs and restart manually."))
          (t (warn "`server-running-p' returned neither nil nor t.  Check and restart server manually if required."))))))

  ;; Load custom code

  (dolist (file db/after-init-load-files)
    (message "Loading %s" file)
    (with-demoted-errors "Error loading file: %s"
      (load-file file)))

  ;; Finish
  
  (message "Running main initialization ... done")

  t)

(add-hook 'after-init-hook #'db/run-init)


;; * Personal customization

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


;; * General configuration

(use-package cl-lib
  :demand t)

(use-package subr-x
  :demand t)

(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq inhibit-startup-message t
      inhibit-default-init t
      frame-inhibit-implied-resize t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      ring-bell-function #'ignore
      garbage-collection-messages nil
      load-prefer-newer nil             ; t breaks `org-reload'
      auth-sources '("~/.authinfo.gpg")
      auth-source-save-behavior nil)

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
      completion-cycle-threshold 10
      enable-recursive-minibuffers t
      set-mark-command-repeat-pop t
      large-file-warning-threshold 10000000
      echo-keystrokes 0.1
      delete-by-moving-to-trash t
      delete-trailing-lines nil
      x-underline-at-descent-line t
      search-whitespace-regexp "[ \t\r\n]+"
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      history-delete-duplicates t
      track-eol t
      gc-cons-threshold (* 100 1024 1024) ; 100mb
      read-process-output-max (* 1024 1024) ; 1mb
      next-error-message-highlight t)

(when (memq system-type '(gnu gnu/linux gnu/kfreebsd))
  (setq x-wait-for-event-timeout nil))

(when on-windows
  ;; treat memory for display time ...  but hey, this is Windows, memory doesn’t
  ;; matter!
  (setq inhibit-compacting-font-caches t))

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

(setq undo-limit 80000000)

(setq-default async-shell-command-buffer 'new-buffer)
(add-to-list 'display-buffer-alist
             '("^\\*Async Shell Command*" . (display-buffer-no-window)))

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq-default savehist-file (expand-file-name "savehist" emacs-d))

(setq default-input-method "TeX")


;; * Fixes

(with-eval-after-load 'enriched
  (defun enriched-decode-display-prop (start end &optional params)
    (ignore params)
    (list start end)))

;; Disable gconf settings, as it might interfere with ours.  Cf.
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25228 and
;; https://emacs.stackexchange.com/questions/32641/something-changes-the-default-face-in-my-emacs.
(define-key special-event-map [config-changed-event] 'ignore)


;; * Builtin Packages

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
  :init (setq save-abbrevs 'silently
              abbrev-file-name (expand-file-name "private/abbrev_defs"))
  :diminish abbrev-mode)

(use-package appt
  :commands (appt-activate)
  :init (setq appt-display-mode-line nil))

(use-package ispell
  :commands (ispell-change-directory))

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

(use-package quail
  :config (add-hook 'input-method-activate-hook
                    #'db/add-symbols-to-TeX-input-method))

(use-package isearch
  :init (setq isearch-allow-scroll t))

(use-package server
  :commands (server-running-p server-start)
  :init (setq server-log t))

(use-package bookmark
  :init (setq bookmark-default-file (expand-file-name "private/bookmarks"
                                                      emacs-d)))

(use-package warnings
  :config (cl-pushnew '(undo discard-info) warning-suppress-types
                      :test #'equal))

(use-package calender
  :init (setq calendar-date-style 'iso
              calendar-time-zone-style 'numeric
              calendar-week-start-day 1 ; Monday
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
              holiday-other-holidays '((holiday-fixed 2 13 "Jahrestag Zerstörung Dresden 1945")
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
                                       (holiday-fixed 11 11 "End WWI 1918"))
              diary-show-holidays-flag t
              calendar-view-holidays-initially-flag nil))

(use-package re-builder
  :commands (re-builder)
  :init (setq reb-re-syntax 'string))

(use-package browser-url
  :init (setq browse-url-browser-function 'browse-url-generic
              browse-url-generic-program "firefox"))

(use-package tramp
  :init (setq tramp-default-method (if on-windows "pscp" "scp")
              tramp-completion-use-auth-sources nil))

(use-package calc
  ;; https://florian.adamsky.it/2016/03/31/emacs-calc-for-programmers-and-cs.html
  :defines (math-additional-units
            math-units-table)
  :init (setq math-additional-units
              '((bit nil "Bit")
                (byte "8 * bit" "Byte")
                (bps "bit / s" "Bit per second"))
              math-units-table nil))

(use-package tab-bar
  :init (setq tab-bar-show t)
  :config (progn
            (tab-bar-history-mode +1)))


;; * Essential external packages

(use-package dash
  :defer nil
  :config (progn
            (global-dash-fontify-mode)
            (dash-register-info-lookup)))

(use-package hydra
  :pin "melpa-stable")

;; `lv' is a dependency of `hydra'
(add-to-list 'package-pinned-packages '(lv . "melpa-stable"))

(use-package db-utils
  :commands (endless/fill-or-unfill
             db/delete-trailing-whitespace-maybe
             db/run-or-hide-shell
             db/gnus
             db/org-agenda
             db/scratch
             db/find-user-init-file
             db/run-or-hide-ansi-term
             db/hex-to-ascii
             db/text-to-hex
             conditionally-enable-lispy
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
             db/sync-magit-repos-from-projectile
             db/replace-variables-in-string))

(use-package db-hydras
  :commands (hydra-toggle/body
             hydra-zoom/body
             hydra-rectangle/body
             db/define-feature-shortcuts-hydra
             hydra-feature-shortcuts/body))

(use-package exec-path-from-shell
  :pin "melpa-stable"
  :commands (exec-path-from-shell-copy-envs))

(use-package crux
  :ensure t
  :commands (crux-eval-and-replace
             crux-smart-open-line-above
             crux-kill-whole-line
             crux-cleanup-buffer-or-region
             crux-delete-buffer-and-file))


;; * Text editing

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

(use-package synonyms
  :commands (synonyms))

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
             org-reset-checkbox-state-maybe
             db/find-parent-task
             db/ensure-running-clock
             db/save-current-org-task-to-file
             db/org-update-frame-title-with-current-clock
             db/org-clock-out
             db/org-clock-in-break-task
             db/org-clock-in-home-task
             db/org-clock-in-work-task
             db/show-current-org-task
             endless/org-ispell
             db/org-agenda-list-deadlines
             db/org-agenda-skip-tag
             hydra-org-agenda-view/body
             db/org-agenda-insert-efforts
             org-babel-execute:hy
             db/org-timestamp-difference
             db/org-capture-code-snippet
             hydra-org-clock/body
             db/make-org-capture-frame
             db/org-onenote-open
             db/org-outlook-open
             db/org-rfc-open
             db/org-clear-stored-links
             db/org-cleanup-continuous-clocks
             db/find-csv-in-org
             db/org-mark-current-default-task
             db/export-diary
             db/org-copy-template-for-periodic-task
             db/org-copy-template
             db/org-copy-body-from-item-to-point
             db/org-find-links-to-current-item
             db/org-add-link-to-other-item
             db/org-add-link-to-current-clock
             hydra-org-linking/body
             org-dblock-write:db/org-backlinks))

(use-package org
  :pin "gnu"
  :bind (:map org-mode-map
              ([remap org-return] . org-return-indent))
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
                org-log-note-clock-out nil
                org-log-done 'note
                org-clone-delete-id t
                org-catch-invisible-edits 'error
                org-M-RET-may-split-line '((default . nil))
                org-highlight-latex-and-related '(latex)
                org-use-sub-superscripts '{}
                org-src-fontify-natively t
                org-src-preserve-indentation t
                org-src-tab-acts-natively nil
                org-ellipsis "⤵"
                org-fontify-done-headline nil
                org-cycle-separator-lines 0
                org-special-ctrl-a/e t
                org-highlight-latex-and-related nil
                org-attach-store-link-p 'attached
                org-attach-auto-tag nil
                org-bookmark-names-plist nil

                org-blank-before-new-entry '((heading . t)
                                             (plain-list-item . t))

                org-duration-format '(("y") ("w") ("d") (special . h:mm))

                org-treat-S-cursor-todo-selection-as-state-change nil

                org-global-properties
                '(("Effort_ALL" . "0:00 0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 6:00 8:00"))

                org-columns-default-format
                "%80ITEM(Task) %10Effort(Effort) %10CLOCKSUM")

          ;; Keywords and Tags

          (setq org-todo-keywords
                '((sequence "TODO(t)" "CONT(n!)" "|" "DONE(d@)")
                  (sequence "GOTO(g)" "ATTN(a)" "|" "DONE(d@)")
                  (sequence "READ(r)" "CONT(n!)" "|" "DONE(d@)")
                  (sequence "DELG(e@/!)" "WAIT(w@/!)" "HOLD(h@/!)"
                            "|" "CANC(c@/!)" "PHONE" "MEETING"))

                org-todo-state-tags-triggers
                '(("WAIT" ("HOLD") ("WAIT" . t))
                  ("DELG" ("HOLD") ("WAIT" . t))
                  ("HOLD" ("HOLD" . t) ("WAIT"))
                  (done ("HOLD") ("WAIT"))
                  ("TODO" ("HOLD") ("WAIT") ("DATE") ("READ"))
                  ("READ" ("READ" . t) ("DATE") ("HOLD") ("WAIT"))
                  ("GOTO" ("DATE" . t) ("READ") ("HOLD") ("WAIT"))
                  ("CONT" ("DATE") ("HOLD") ("WAIT"))
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

                org-fast-tag-selection-single-key 'expert)

          ;; Faces

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
                  ("REPEAT" :foreground "indian red" :weight normal))

                org-priority-faces
                '((?A . (:foreground "Red" :weight bold))
                  (?B . (:foreground "firebrick"))
                  (?C . (:foreground "tomato"))))

          ;; Refiling

          (setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))
                                     (nil . (:maxlevel . 9))
                                     (db/org-default-notes-file . (:maxlevel . 9)))
                org-refile-use-outline-path 'file
                org-refile-use-cache t
                org-refile-allow-creating-parent-nodes 'confirm
                org-indirect-buffer-display 'current-window
                org-outline-path-complete-in-steps nil
                org-refile-target-verify-function 'db/verify-refile-target)

          ;; Bable

          (setq org-babel-load-languages '((shell . t)
                                           (emacs-lisp . t))))
  :config (progn

            ;; Reset checkboxes if the RESET_CHECK_BOXES property is set
            (add-hook 'org-after-todo-state-change-hook 'org-reset-checkbox-state-maybe)

            ;; Statically color links sponding to whether the file exists, but
            ;; this turns out to be slow on Windows; we can use `org-lint' for
            ;; this when necessary)
            (org-link-set-parameters "file" :face 'org-link)

            ;; File Apps

            (add-to-list 'org-file-apps '(directory . emacs))

            (add-to-list 'org-file-apps '("\\.docx?\\'" . default))
            (add-to-list 'org-file-apps '("\\.pptx?\\'" . default))
            (add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))

            (when (eq system-type 'cygwin)
              (add-to-list 'org-file-apps '(t . "cygstart %s") t))

            ;; Custom link types for Windows
            (when (eq system-type 'windows-nt)
              (org-link-set-parameters "onenote" :follow #'db/org-onenote-open)
              (org-link-set-parameters "outlook" :follow #'db/org-outlook-open))

            ;; Mark some org mode regions to be skipped by ispell
            (add-hook 'org-mode-hook #'endless/org-ispell)

            ;; Link type for RFCs
            (org-link-set-parameters "rfc" :follow #'db/org-rfc-open)

            ;; Some timers

            (unless (memq #'org-clock-save
                          (mapcar #'timer--function timer-list))
              (run-with-timer 0 3600 #'org-clock-save))

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

;; Drag-and-Drop images into org-mode buffer
(use-package org-download
  :commands (org-download-yank
             org-download-screenshot
             org-download-clipboard)
  :init (setq org-download-method 'attach))

;; Extended query language and dynamic blocks
(use-package org-ql-search
  :ensure org-ql
  :commands (org-ql-view
             org-ql-search
             org-dblock-write:org-ql)

  :config (progn

            ;; Redefine the regular expression for link searches to allow
            ;; brackets in the description.  This function comes straight from
            ;; org-ql.el
            (cl-defun org-ql--link-regexp (&key description-or-target description target)
              "Return a regexp matching Org links according to arguments.
Each argument is treated as a regexp (so non-regexp strings
should be quoted before being passed to this function).  If
DESCRIPTION-OR-TARGET, match it in either description or target.
If DESCRIPTION, match it in the description.  If TARGET, match it
in the target.  If both DESCRIPTION and TARGET, match both,
respectively."
              (cl-labels
                  ((no-desc
                    (match) (rx-to-string `(seq (or bol (1+ blank))
                                                "[[" (0+ (not (any "]"))) (regexp ,match) (0+ (not (any "]")))
                                                "]]")))
                   (match-both
                    (description target)
                    (rx-to-string `(seq (or bol (1+ blank))
                                        "[[" (0+ (not (any "]"))) (regexp ,target) (0+ (not (any "]")))
                                        ;; Added .* wildcards
                                        "][" (regexp ".*") (regexp ,description) (regexp ".*")
                                        "]]")))
                   ;; Note that these actually allow empty descriptions
                   ;; or targets, depending on what they are matching.
                   (match-desc
                    (match) (rx-to-string `(seq (or bol (1+ blank))
                                                "[[" (0+ (not (any "]")))
                                                ;; Added .* wildcards
                                                "][" (regexp ".*") (regexp ,match) (regexp ".*")
                                                "]]")))
                   (match-target
                    (match) (rx-to-string `(seq (or bol (1+ blank))
                                                "[[" (0+ (not (any "]"))) (regexp ,match) (0+ (not (any "]")))
                                                ;; Removed pattern for description
                                                "][" ))))
                (cond (description-or-target
                       (rx-to-string `(or (regexp ,(no-desc description-or-target))
                                          (regexp ,(match-desc description-or-target))
                                          (regexp ,(match-target description-or-target)))))
                      ((and description target)
                       (match-both description target))
                      (description (match-desc description))
                      (target (rx-to-string `(or (regexp ,(no-desc target))
                                                 (regexp ,(match-target target))))))))))

(use-package ol
  :init (setq org-link-keep-stored-after-insertion t)
  :commands (org-store-link))

(use-package org-id
  :init (setq org-id-link-to-org-use-id t))

(use-package org-clock
  :commands (org-clock-save)
  :init (progn
          (setq org-clock-history-length 35
                org-clock-in-resume t
                org-clock-into-drawer t
                org-clock-idle-time nil
                org-clock-out-remove-zero-time-clocks t
                org-clock-out-when-done '("DONE" "CANC" "WAIT" "HOLD")
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
  :commands (org-agenda)
  :bind (:map org-agenda-mode-map
              ("i" . org-agenda-clock-in)
              ("v" . hydra-org-agenda-view/body))
  :init (setq org-agenda-include-diary t
              org-agenda-span 1
              org-agenda-insert-diary-strategy 'top-level
              org-catch-invisible-edits 'show
              org-agenda-sorting-strategy '((agenda time-up priority-down effort-up category-keep)
                                            (todo priority-down effort-up category-keep)
                                            (tags priority-down effort-up category-keep)
                                            (search priority-down effort-up category-keep))
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
              org-agenda-remove-tags t
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

              ;; Show daily efforts directly in the agenda
              org-agenda-finalize-hook '(db/org-agenda-insert-efforts)

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
              '("+TODO=\"\"-DATE-HOLD-NOTE-TAGS={NOP\\|TOPIC\\|SOMEWHEN\\|TEMPLATE}-SCHEDULED>=\"<+0d>\""
                ("CONT" "TODO" "READ" "WAIT" "GOTO" "DELG" "ATTN")
                ()
                "")

              org-agenda-prefix-format
              '((agenda . "%11s%?-12t%-4e ")
                (todo . "%-8c%-4e ")
                (tags . "%-8c%-4e ")
                (search . "%-8c%-4e "))

              org-agenda-custom-commands
              `(("A" "Main Agenda"
                 ((agenda
                   ""
                   ((org-agenda-entry-types '(:timestamp :sexp :scheduled))
                    (org-deadline-warning-days 0)))
                  (db/org-agenda-list-deadlines
                   ""
                   ((org-agenda-overriding-header "Deadlines")
                    (org-agenda-sorting-strategy '(deadline-up priority-down))
                    (org-deadline-warning-days 30)))
                  (tags-todo "TODO={CONT\\|ATTN}-HOLD-TIMESTAMP>\"<now>\""
                             ((org-agenda-overriding-header "WIP List (TODO ∈ {CONT,ATTN}, not scheduled now or in the future)")
                              (org-agenda-todo-ignore-scheduled 0)))
                  (tags-todo "TODO<>\"CONT\"-HOLD-SOMEWHEN-DATE-WAIT-TEMPLATE/-DONE"
                             ((org-agenda-overriding-header "Next Actions List (not WIP, not scheduled now or in the future)")
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled 0)))))

                ("B" "Backlog"
                 ((tags-todo "-HOLD-SOMEWHEN-DATE-PERIODIC-TEMPLATE/-DONE"
                             ((org-agenda-overriding-header "Backlog: Actionable items (no periodic tasks; includes waiting-fors)")
                              (org-tags-match-list-sublevels t)))
                  (tags "TODO=\"\"-HOLD-SOMEWHEN-DATE-PERIODIC-NOTE-NOP-TOPIC-TEMPLATE"
                        ((org-agenda-overriding-header "Backlog: Complex tasks (i.e., goals)")
                         (org-tags-match-list-sublevels t)))))

                ("C" "Checks"
                 ((tags "TODO=\"\"-HOLD-SOMEWHEN-DATE-PERIODIC-NOTE-NOP-TOPIC-TEMPLATE-GOAL"
                        ((org-agenda-overriding-header "Goals (i.e., complex tasks) not marked with GOAL")))
                  (org-ql-block '(and (not (tags "TOPIC"))
                                     (descendants (tags "TOPIC")))
                                ((org-ql-block-header "Non-TOPIC items containing TOPICs")
                                 (org-agenda-sorting-strategy nil)))))

                ("U" "Unsupervised (Waiting, Missed Appointments, Hold)"
                 ((tags-todo "WAIT-HOLD-SOMEWHEN"
                             ((org-agenda-overriding-header "Waiting For List")
                              (org-agenda-todo-ignore-scheduled 0)))
                  (tags-todo "DATE"
                             ((org-agenda-overriding-header "Missed appointments (DATEs with timestamp in the past)")
                              (org-agenda-todo-ignore-timestamp 0)))
                  (tags "REFILE"
                        ((org-agenda-files (list db/org-default-refile-file))
                         (org-agenda-overriding-header "Things to refile (make it empty!)")))
                  (tags-todo "HOLD"
                             ((org-agenda-overriding-header "Tasks on Hold")))))


                ("S" "Somewhen (Do if nothing else to do, i.e., personal backlog)"
                 ((tags "TAGS={SOMEWHEN}+TODO=\"\"-TAGS={NOP\\|TOPIC}-PERIODIC-DATE-SCHEDULED>=\"<+0d>\""
                        ((org-agenda-overriding-header "Open Tasks to do SOMEWHEN (no TODO keyword, no PERIODIC, no DATE, no now or future SCHEDULED)")))
                  (tags-todo "SOMEWHEN/-CANC-DONE"
                             ((org-agenda-overriding-header "Things To Do SOMEWHEN")
                              (org-agenda-todo-ignore-with-date t)
                              (org-tags-match-list-sublevels nil)))))


                ("P" "Current Projects and Topics"
                 ((stuck ""
                         ((org-agenda-overriding-header "Stuck Complex Tasks")))
                  (tags "TAGS={NOTE}-TODO={CANC\\|DONE}-HOLD-NOP-SCHEDULED>=\"<+0d>\""
                        ((org-agenda-overriding-header "Project Notes (items explicitly tagged with NOTE but not NOP, not scheduled now or in the future)")
                         (org-agenda-prefix-format '((tags . "%-8c ")))))
                  (tags "TAGS={TOPIC}-TODO={DONE\\|CANC}-SCHEDULED>=\"<+0d>\"-HOLD-WAIT"
                        ((org-agenda-overriding-header "Topics")
                         (org-agenda-prefix-format '((tags . "%-8c%l ")))
                         (org-agenda-sorting-strategy nil)))
                  (tags "TAGS={PERIODIC}-TODO={DONE\\|CANC}-HOLD-SCHEDULED>=\"<+0d>\"-HOLD-WAIT"
                        ((org-agenda-overriding-header "Periodic Projects (PERIODIC, not scheduled in the future, not done, not on hold)")
                         (org-agenda-prefix-format '((tags . "%-8c ")))))))

                ("W" "Weekly Review"
                 ((agenda ""
                          ((org-agenda-span 7)
                           (org-agenda-dim-blocked-tasks nil)
                           (org-agenda-skip-deadline-prewarning-if-scheduled t)))))))

  :config (progn
            ;; avoid important buffers to end up in `org-agenda-new-buffers’ by
            ;; opening them manually
            (mapc #'find-file-noselect org-agenda-files)

            (add-hook 'org-agenda-mode-hook #'hl-line-mode 'append)

            (advice-add 'org-agenda
                        :before #'db/check-special-org-files-in-agenda)))

;; Capturing

(use-package org-capture
  :commands (org-capture)
  :init (setq org-capture-use-agenda-date nil
              org-capture-bookmark nil
              org-capture-templates
              `(("t" "Simple Task"
                 entry
                 (file db/org-default-refile-file)
                 ,(concat "* TODO [#B] %^{What}\n"
                          ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                          "%a\n"
                          "%?")
                 :empty-lines-after 1)
                ("g" "Record new goal with first item"
                 entry
                 (file db/org-default-refile-file)
                 ,(concat "* %^{Ticket Description} (%^{Ticket Number}) :GOAL:\n"
                          ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                          "\n** TODO [#B] %^{First Task}\n"
                          ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                          "\n%?"))
                ("n" "Note"
                 entry
                 (file db/org-default-refile-file)
                 "* Note: %^{About} :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
                 :empty-lines-after 1)
                ("d" "Date"
                 entry
                 (file db/org-default-refile-file)
                 "* GOTO [#B] %^{What} :DATE:\n%^{When}t\n%a%?"
                 :empty-lines-after 1)
                ("i" "Interruptions"
                 entry
                 (file db/org-default-refile-file)
                 ,(concat "* DONE [#B] %^{What}\nCLOSED: %U\n"
                          ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                          "\nInterrupted %K\n\n%?")
                 :clock-in t :clock-resume t :empty-lines-after 1)
                ("r" "respond"
                 entry
                 (file db/org-default-refile-file)
                 ,(concat "* TODO [#B] Reply: %:subject (%:from) :EMAIL:\n"
                          ":PROPERTIES:\n:CREATED: %U\n:END:\n"
                          "\n%a\n%?")
                 :empty-lines-after 1)))
  :config (progn
            ;; disable usage of helm for `org-capture'
            (with-eval-after-load 'helm-mode
              (defvar helm-completing-read-handlers-alist) ; for the byte compiler
              (add-to-list 'helm-completing-read-handlers-alist
                           '(org-capture . nil)))))

;; Babel

(use-package ob-core
  :init (setq org-export-use-babel nil)
  :config (setf (alist-get :results org-babel-default-header-args)
                "output code replace"))

(use-package ob-sql
  :config (progn

            (defun db/ob-sql-oracle-ask-for-password (orig-fun
                                                      host port user password database)
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
                         database))))

            (advice-add #'org-babel-sql-dbstring-oracle
                        :around #'db/ob-sql-oracle-ask-for-password)))

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
              ("<C-left>" . org-tree-slide-move-previous-tree)))

(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :commands (org-roam-node-insert
             org-roam-node-find
             org-roam-capture)
  :custom ((org-roam-directory "~/Documents/zettelkasten/")
           (org-roam-db-location "~/Documents/zettelkasten/org-roam.db")
           (org-roam-completion-everywhere t))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam-buffer-toggle)
               ("C-c n g" . org-roam-graph))))


;; * General Programming

(use-package ediff
  :init (setq ediff-diff-options "-w"
              ediff-window-setup-function 'ediff-setup-windows-plain
              ediff-split-window-function 'split-window-horizontally
              ediff-show-clashes-only t)
  :config (progn
            (add-hook 'ediff-keymap-setup-hook
                      #'(lambda ()
                          (bind-key "j" #'ediff-next-difference ediff-mode-map)
                          (bind-key "k" #'ediff-previous-difference ediff-mode-map)))

            (add-hook 'ediff-after-quit-hook-internal 'winner-undo)))

(use-package git-commit
  :commands (global-git-commit-mode)
  :ensure t
  :init (setq git-commit-style-convention-checks '(non-empty-second-line
                                                   overlong-summary-line)
              git-commit-known-pseudo-headers '("Signed-off-by"
                                                "Acked-by"
                                                "Modified-by"
                                                "Cc"
                                                "Suggested-by"
                                                "Reported-by"
                                                "Tested-by"
                                                "Reviewed-by")))

(use-package magit
  :ensure t
  :commands (magit-status)
  :init (setq magit-diff-refine-hunk nil
              magit-commit-show-diff nil)
  :config (progn
            (when (fboundp 'global-magit-file-mode)
              (global-magit-file-mode -1))
            (global-git-commit-mode +1)

            (db/sync-magit-repos-from-projectile)))

(use-package magit-repos
  :commands (magit-list-repositories))

(use-package projectile
  :ensure t
  :commands (projectile-mode)
  :defines (projectile-known-projects)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init (setq projectile-switch-project-action 'projectile-dired
              projectile-completion-system 'helm
              projectile-ignored-project-function #'file-remote-p
              projectile-create-missing-test-files t
              projectile-known-projects-file (expand-file-name "private/projectile-bookmarks.eld"
                                                               emacs-d))
  :diminish projectile-mode)

(use-package counsel-projectile
  :commands counsel-projectile)

(use-package highlight-indentation
  :commands highlight-indentation-mode)

(use-package iedit
  :ensure t
  :commands (iedit-mode))

(use-package page-break-lines
  :pin "melpa-stable"
  :commands (page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package flycheck
  :ensure t
  :commands (global-flycheck-mode flycheck-mode)
  :init (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c C-l")
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode))


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
  :init (setq bbdb-completion-display-record nil
              bbdb-complete-mail-allow-cycling t
              bbdb-mua-auto-update-p 'query
              bbdb-default-country "Germany")
  :config (progn
            (add-hook 'message-setup-hook 'bbdb-mail-aliases)
            (add-hook 'mail-setup-hook 'bbdb-mail-aliases)
            (run-with-timer 0 3600 #'bbdb-save)

            (with-eval-after-load 'ol-bbdb
              (add-to-list 'org-bbdb-anniversary-format-alist
                           (cons "day-of-death"
                                 #'(lambda (name years suffix)
                                     (format "Day of Death: [[bbdb:%s][%s (%s%s)]]"
                                             name name years suffix)))))))

;; General Gnus configuration

(setq gnus-init-file (expand-file-name "gnus.el" emacs-d)
      gnus-home-directory (expand-file-name "~/.config/gnus-news")
      gnus-directory gnus-home-directory
      gnus-kill-files-directory gnus-directory
      gnus-startup-file (expand-file-name "private/gnus-newsrc" emacs-d)
      gnus-cache-directory (expand-file-name "cache/" gnus-directory)
      gnus-verbose 10

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
      gnus-build-sparse-threads 'some
      gnus-subscribe-newsgroup-method 'gnus-subscribe-killed
      gnus-group-list-inactive-groups t
      gnus-suppress-duplicates nil
      gnus-large-newsgroup 200
      nnmail-expiry-wait 7
      nnmail-cache-accepted-message-ids t
      gnus-summary-next-group-on-exit nil
      gnus-use-full-window nil
      gnus-always-force-window-configuration t
      gnus-fetch-old-headers nil
      gnus-select-method '(nnnil "")
      gnus-refer-article-method 'current

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

      message-citation-line-function
      #'(lambda ()
          (when message-reply-headers
            (insert "ghItlhpu' "
                    (mail-header-from message-reply-headers)
                    ":")
            (newline))))

;; Gnus Appearence

(setq gnus-group-line-format "%S%p%P%5y(%2i):%B%(%s:%G%)\n"
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

      gnus-treat-hide-boring-headers 'head
      gnus-treat-strip-multiple-blank-lines nil
      gnus-treat-display-smileys t
      gnus-treat-emphasize 'head
      gnus-treat-unsplit-urls t)

(use-package gnus-art
  :init (setq gnus-ignored-mime-types '("text/x-vcard")
              gnus-inhibit-mime-unbuttonizing nil
              gnus-buttonized-mime-types '("multipart/signed" "multipart/encrypted")
              gnus-inhibit-images t
              gnus-blocked-images ".*"))

;; Adaptive Scoring

(setq gnus-use-scoring t
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
      gnus-summary-mark-below nil)

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

            (defun mm-copy-to-buffer ()
              "Copy the contents of the current buffer to a fresh buffer."
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

(use-package mml
  :config (progn
            ;; Move to end of message buffer before attaching a file
            ;; http://mbork.pl/2015-11-28_Fixing_mml-attach-file_using_advice

            (defun db/mml-attach-file--go-to-eob (orig-fun &rest args)
              "Go to the end of buffer before attaching files."
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-max))
                  (apply orig-fun args))))

            (advice-add 'mml-attach-file
                        :around #'db/mml-attach-file--go-to-eob)))

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

            (defun db/smime-add-crlf-when-pkcs7 (cont)
              "If CONT signifies encryption with smime, replace all \n with \r\n."
              (when (and (eq (car cont) 'part)
                         (string= "smime" (or (cdr (assq 'encrypt cont)) "")))
                (db/convert-lf-to-crlf-in-buffer)))

            (advice-add 'mml-smime-epg-sign
                        :after #'db/smime-add-crlf-when-pkcs7)))

;; Archiving

;; We store messages in the current group, so there is
;; no need to use Gnus’ archiving method

(setq gnus-message-archive-method nil
      gnus-update-message-archive-method t
      gnus-message-archive-group nil
      gnus-gcc-mark-as-read t)

;; Searching

(setq nnir-method-default-engines '((nnimap . imap)
                                    (nntp . gmane)))

;; Agents

(use-package gnus-agent
  :init (setq gnus-agent-mark-unread-after-downloaded nil
              gnus-agent-synchronize-flags t
              gnus-agent-go-online t))

;; Package configuration

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
              nsm-settings-file (expand-file-name
                                 "~/.emacs.d/private/network-security.data"))
  :config (advice-add 'nsm-write-settings
                      :before #'db/sort-nsm-permanent-settings))

(use-package gnutls
  :init (setq gnutls-log-level 0        ; too noisy otherwise
              gnutls-min-prime-bits 1024
              gnutls-verify-error t))

(use-package epg
  :init (setq epg-debug t
              epg-gpg-program "gpg"))


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
            (setq custom--inhibit-theme-enable nil)

            (custom-theme-set-faces
             'solarized-dark
             `(tab-bar ((t (:height 1.2
                                    :foregreound ,(cdr (assoc 'base03 solarized-dark-color-palette-alist))
                                    :background ,(cdr (assoc 'base02 solarized-dark-color-palette-alist))))))
             `(tab-bar-tab ((t (:background ,(cdr (assoc 'base02 solarized-dark-color-palette-alist))
                                            :foreground ,(cdr (assoc 'magenta solarized-dark-color-palette-alist))
                                            :inverse-video nil
                                            :box (:line-width 1 :style released-button)))))
             `(tab-bar-tab-inactive ((t (:inherit tab-bar-tab
                                                  :background ,(cdr (assoc 'base0 solarized-dark-color-palette-alist))
                                                  :foreground ,(cdr (assoc 'base03 solarized-dark-color-palette-alist))
                                                  :inverse-video t
                                                  :box nil)))))))

(use-package smart-mode-line
  :ensure t
  :init (setq sml/mode-width 'full
              sml/name-width 30)
  :commands (sml/setup))

(use-package minions
  :ensure t
  :commands (minions-mode)
  :init (setq minions-mode-line-lighter "…"))

(use-package moody
  :ensure t
  :commands (moody-replace-mode-line-buffer-identification
             moody-replace-vc-mode))


;; * Dired

(use-package dired
  :bind (:map dired-mode-map
              ("e" . ora-ediff-files)
              ("z" . dired-get-size)
              ([remap beginning-of-buffer] . dired-back-to-top)
              ([remap end-of-buffer] . dired-jump-to-bottom)
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
                dired-kill-when-opening-new-dired-buffer t

                ;; Don’t use obsolete diredx local variables
                dired-enable-local-variables nil
                dired-local-variables-file nil

                dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"
                diredp-hide-details-initially-flag t

                wdired-create-parent-directories t
                wdired-allow-to-change-permissions t

                dired-isearch-filenames 'dwim
                dired-auto-revert-buffer t)

          (setq dired-guess-shell-alist-user
                '(("\\.pdf\\'" "evince")
                  ("\\.ps\\'" "evince")
                  ("\\.\\(?:djvu\\|eps\\)\\'" "evince")
                  ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "eog")
                  ("\\.\\(?:xcf\\)\\'" "gimp")
                  ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|webm\\)\\(?:\\.part\\)?\\'"
                   "vlc")
                  ("\\.\\(?:mp3\\|flac\\|ogg\\)\\'" "mplayer")
                  ("\\.docx?\\'" "loffice")))

          (when on-windows
            (setq directory-free-space-program nil)))
  :config (progn
            (put 'dired-find-alternate-file 'disabled nil)

            (require 'dired-x)

            (with-demoted-errors "Non-Fatal Errors (dired-open): %s"
              (require 'dired-open))

            (if (eq system-type 'windows-nt)
                (with-demoted-errors "Non-Fatal Error (w32-browser): %s"
                  (require 'w32-browser)
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
              (add-to-list 'dired-latex-unclean-extensions extension))

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
                                #'(lambda ()
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

(use-package dired-x
  :commands (dired-jump dired-jump-other-window) ; In Emacs 28.1, this has been
                                                 ; moved to dired.el, but we'll
                                                 ; keep it here to support Emacs
                                                 ; 27.2 as well.
  :init (setq dired-clean-confirm-killing-deleted-buffers t
              dired-x-hands-off-my-keys t
              dired-bind-man nil
              dired-bind-info nil
              dired-clean-up-buffers-too t))

(use-package dired-subtree
  :commands (dired-subtree-toggle))

(use-package find-dired
  :commands (find-dired)
  :init (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(use-package dired-open
  :ensure t
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

(use-package gnus-dired
  :commands (turn-on-gnus-dired-mode))

(use-package trashed
  ;; A simple dired-like interface to the system trash bin
  ;; Configuration taken from https://protesilaos.com/dotemacs
  :init (setq trashed-action-confirmer 'y-or-n-p
              trashed-use-header-line t
              trashed-sort-key '("Date deleted" . t)
              trashed-date-format "%Y-%m-%d %H:%M:%S"))


;; * Completion

(use-package hippie-exp
  :commands (hippie-expand))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init (setq helm-command-prefix-key "C-c h"
              helm-input-idle-delay 0.0
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
              helm-mode-no-completion-in-region-in-modes '(eshell-mode)
              helm-kill-ring-threshold 0 ; include all yanks in the kill ring
              )
  :config (progn
            (require 'helm-config)
            (require 'helm-mode)
            (require 'helm-buffers)
            (require 'helm-ring)

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

(use-package helm-ring
  :commands (helm-show-kill-ring))

(use-package ivy
  :ensure t
  :commands (ivy-mode
             ivy-resume)
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
            ;; anyway, providing the an individual restriction in the ivy buffer
            ;; is not necessary anymore.  Since I often mistype S-SPC for SPC,
            ;; loosing the current candidate and annoying myself, removing this
            ;; shortcut is both helpful and not removing necessary
            ;; functionality.
            (define-key ivy-minibuffer-map (kbd "S-SPC") nil)))

(use-package ivy-hydra)

(use-package counsel
  :ensure t
  :commands (counsel-org-goto-all
             counsel-M-x
             counsel-find-file
             counsel-info-lookup-symbol
             counsel-unicode-char
             counsel-descbinds
             counsel-describe-variable
             counsel-describe-function
             counsel-recentf
             counsel-shell-history))

(use-package swiper
  :ensure t
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

(defun db/helm-shortcuts (arg)
  "Open helm completion on common locations.
With given ARG, display files in `db/important-document-path’."
  (interactive "p")
  (require 'helm-bookmark)
  (require 'helm-for-files)             ; for helm-source-recentf
  (helm :sources (list
                  (helm-make-source "Frequently Used" 'helm-source-sync
                    :candidates (mapcar #'(lambda (entry)
                                            (cons (car entry)
                                                  (caddr entry)))
                                        db/frequently-used-features)
                    :action '(("Open" . call-interactively))
                    :filtered-candidate-transformer #'helm-adaptive-sort)

                  ;; taken from `helm-buffers-list'
                  (helm-make-source "Buffers" 'helm-source-buffers)

                  helm-source-recentf

                  ;; if prefix arg is given, extract files from
                  ;; `db/important-documents-path’ and list them as well
                  (when (and (= arg 4)
                             (file-directory-p db/important-documents-path))
                    (let ((search-path (expand-file-name db/important-documents-path)))
                      (helm-make-source "Important files" 'helm-source-sync
                        :candidates (mapcar #'(lambda (file)
                                                ;; display only relative path,
                                                ;; but keep absolute path for
                                                ;; actions
                                                (cons (string-remove-prefix search-path file)
                                                      file))
                                            (directory-files-recursively search-path ""))
                        :action '(("Open externally" . db/system-open)
                                  ("Find file" . find-file)))))

                  helm-source-bookmarks

                  helm-source-buffer-not-found
                  helm-source-bookmark-set)))

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

(use-package goto-last-change
  :commands goto-last-change)


;; * Media

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
              emms-player-list '(emms-player-mplayer emms-player-mplayer-playlist)
              emms-show-format "NP: %s"
              emms-stream-default-action "play"
              emms-track-description-function 'db/emms-track-description
              emms-playlist-default-major-mode 'emms-playlist-mode
              emms-cache-file (expand-file-name "private/emms/cache" emacs-d)
              emms-history-file (expand-file-name "private/emms/history" emacs-d)
              emms-score-file (expand-file-name "private/emms/scores" emacs-d)
              emms-stream-bookmarks-file (expand-file-name "private/emms/streams" emacs-d))
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

;; Make sure emms is up and running when we call functions such as
;; `emms-play-dired’ etc.
(use-package emms-source-file
  :config (require 'emms))

(use-package db-emms
  :commands (db/emms-source-file-directory-tree-find
             db/emms-track-description
             db/emms-playlist-mode-insert-track))

(use-package helm-emms
  :commands (helm-emms)
  :init (setq helm-emms-use-track-description-function t
              helm-emms-default-sources '(helm-source-emms-streams
                                          helm-source-emms-dired
                                          helm-source-emms-files))
  :config (progn
            (require 'emms)
            (require 'helm-adaptive)))

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
  :commands (term-send-string)
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

            (add-hook 'term-mode-hook #'(lambda () (yas-minor-mode -1)))))

(use-package ansi-color
  :commands (ansi-color-for-comint-mode-on
             ansi-color-apply-on-region)
  :config (progn
            (add-hook 'compilation-filter-hook 'endless/colorize-compilation)))

(use-package shell
  :commands (shell)
  :bind (:map shell-mode-map
              ("C-r" . counsel-shell-history))
  :config (progn
            (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
            (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
            (add-hook 'shell-mode-hook 'with-editor-export-editor)

            ;; We may want to use readline support in bash, don't inhibit this
            ;; with explicit command line arguments;
            ;; cf. https://coredumped.dev/2020/01/04/native-shell-completion-in-emacs/

            (setq explicit-bash-args
                  (delete "--noediting" explicit-bash-args))

            ;; When doing completion, ivy seems to add an extra space, much like
            ;; in the case for eshell.  However, here the space seems to come
            ;; out of nowhere.  Since the builtin completion using
            ;; `completion--in-region' is good enough for the shell mode, let's
            ;; stick to that.

            (add-hook 'shell-mode-hook
                      #'(lambda ()
                          (setq-local completion-in-region-function
                                      #'completion--in-region)))))

(use-package db-eshell
  :commands (db/run-or-hide-eshell
             eshell-clear-buffer
             eshell/default-prompt-function
             eshell/gst
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
              eshell-destroy-buffer-when-process-dies t
              eshell-prompt-function #'eshell/default-prompt-function
              eshell-prompt-regexp "└─[$#] "
              eshell-highlight-prompt nil
              eshell-cd-on-directory t
              eshell-expand-input-functions '(eshell-expand-history-references))
  :config (progn (require 'em-prompt)
                 (require 'em-term)
                 (require 'em-cmpl)
                 (require 'em-hist)

                 (setenv "PAGER" "cat")

                 (add-to-list 'eshell-command-completions-alist
                              '("gunzip" "gz\\'"))

                 (add-to-list 'eshell-command-completions-alist
                              '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

                 (add-hook 'eshell-mode-hook
                           'with-editor-export-editor)

                 (if (<= emacs-major-version 27)
                     (add-hook 'eshell-mode-hook
                               #'(lambda ()
                                   (bind-key "C-a" #'eshell-bol eshell-mode-map)
                                   (bind-key "M-r" #'eshell-insert-history eshell-mode-map)
                                   (bind-key "M-P" #'eshell-previous-prompt eshell-mode-map)
                                   (bind-key "M-N" #'eshell-next-prompt eshell-mode-map)))
                   ;; In Emacs 28.1, eshell's mode maps have been refactored to
                   ;; follow standard extensibility.  There's thus no need
                   ;; anymore to use the special hook construction.
                   (progn
                     (bind-key "C-a" #'eshell-bol eshell-mode-map)
                     (bind-key "M-r" #'eshell-insert-history eshell-hist-mode-map)
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
                                #'(lambda ()
                                    (setq pcomplete-ignore-case nil))))

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

                 (defun db/set-empty-pcomplete-termination-string ()
                   "Locally set `pcomplete-termination-string' to the empty string."
                   (setq-local pcomplete-termination-string ""))

                 (add-hook 'eshell-mode-hook
                           #'db/set-empty-pcomplete-termination-string)

                 (require 'db-eshell)))

(use-package with-editor
  :commands (with-editor-export-editor))


;; * Lisp

;; General Stuff first

(use-package lisp-mode
  :init (setq lisp-indent-function #'lisp-indent-function))

(use-package lispy
  :ensure t
  :commands (lispy-mode)
  :diminish lispy-mode)

(use-package eldoc
  :commands (global-eldoc-mode
             turn-on-eldoc-mode)
  :diminish eldoc-mode)

;; Lisp Dialects

(use-package elisp-mode
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'turn-on-lispy-when-available)
            (add-hook 'emacs-lisp-mode-hook 'turn-on-flycheck-when-file)))

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
              cider-repl-display-help-banner nil)
  :config (progn
            (add-hook 'cider-repl-mode-hook 'subword-mode)
            (add-hook 'cider-repl-mode-hook 'turn-on-lispy-when-available)
            (add-hook 'cider-repl-mode-hook 'cider-repl-toggle-pretty-printing)
            (add-hook 'cider-repl-mode-hook 'company-mode)))

(use-package clojure-mode
  :config (progn
            (define-clojure-indent
              (forall 'defun)
              (exists 'defun)
              (dopar 'defun))
            (add-hook 'clojure-mode-hook 'turn-on-lispy-when-available)
            (add-hook 'clojure-mode-hook 'clj-refactor-mode)
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
              (add-hook 'lisp-mode-hook #'(lambda () (slime-mode +1)) t))
  :config   (progn
              (make-directory "/tmp/slime-fasls/" t)
              (slime-setup '(slime-repl slime-fancy slime-autodoc))
              (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)))

(use-package hy-mode
  :commands (hy-mode)
  :config (progn
            (add-hook 'hy-mode-hook 'turn-on-lispy-when-available)
            (add-hook 'hy-mode-hook 'inferior-lisp)))


;; * TeX

(use-package reftex
  :commands (turn-on-reftex)
  :init (setq reftex-plug-into-AUCTeX t)
  :config (with-eval-after-load 'helm-mode
            (add-to-list 'helm-completing-read-handlers-alist
                         '(reftex-citation . nil))))

(use-package tex
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

            (add-hook 'LaTeX-mode-hook #'(lambda ()
                                           (TeX-PDF-mode 1)
                                           (TeX-source-correlate-mode 1)
                                           (TeX-fold-mode 1)))


            (add-to-list 'TeX-view-program-selection
                         '(output-pdf "Evince"))

            ;; style used for my personal definitions; not clear whether this
            ;; works as intended
            (TeX-add-style-hook
             "mydefs"
             #'(lambda ()
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
             #'(lambda ()
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
                      #'(lambda () (ispell-change-dictionary "de_DE")))
            (add-hook 'TeX-language-en-hook
                      #'(lambda () (ispell-change-dictionary "en_US")))
            (add-hook 'TeX-mode-hook
                      #'(lambda () (setq ispell-parser 'tex)))))


;; * Various Mode Configurations

;; These are packages that are not essential, but still nice to have.  They
;; provide optional functionality and may redefine builtin commands.

(use-package cperl-mode
  :ensure t
  :commands (cperl-mode)
  :init (progn
          ;; replace perl-mode with cperl-mode
          (mapc
           #'(lambda (pair)
               (if (eq (cdr pair) 'perl-mode)
                   (setcdr pair 'cperl-mode)))
           (append auto-mode-alist interpreter-mode-alist))

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

(use-package edit-list
  :ensure t
  :commands edit-list)

(use-package expand-region
  :ensure t
  :commands (er/expand-region))

(use-package eproject
  ;; This configuration is only present to inhibit eproject overriding
  ;; keybindings in `message-mode'
  :config (progn
            (message "Loaded eproject … done")
            (with-eval-after-load 'message
              (add-hook 'message-setup-hook
                        #'(lambda ()
                            (eproject-mode -1))))))

(use-package eww
  :init (setq eww-bookmarks-directory
              (expand-file-name "private/" emacs-d)))

(use-package haskell-mode
  :config (progn
            (add-hook 'haskell-mode-hook 'haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            (add-hook 'haskell-mode-hook
                      #'(lambda ()
                          (company-mode 1)
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

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :init (progn
          (setq markdown-use-pandoc-style-yaml-metadata t
                markdown-command "pandoc --standalone")
          (fset 'markdown-output-standalone-p #'(lambda () t))))

(use-package plantuml-mode
  :load-path "site-lisp"
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :commands (plantuml-mode)
  :init (setq plantuml-output-type "svg"
              plantuml-default-exec-mode 'jar
              plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
              plantuml-indent-level 2)
  :config (progn
            (add-hook 'plantuml-mode-hook
                      #'(lambda ()
                          (subword-mode -1)))))

(use-package python
  :config (progn
            (unless (require 'lsp-pyright nil :no-error)
              (message "`lsp-pyright' not available, using defaults from `lsp'"))

            (add-hook 'python-mode-hook #'highlight-indentation-mode)
            (add-hook 'python-mode-hook #'company-mode)
            (add-hook 'python-mode-hook #'lsp)))

;; https://ddavis.io/posts/emacs-python-lsp/
(use-package pyvenv
  :ensure t
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

(use-package shr
  :init (setq shr-use-fonts nil
              shr-use-colors nil
              shr-max-image-proportion 0.7
              shr-image-animate nil
              shr-width (current-fill-column)))

;; Interactive interface to sdcv, the StarDict concole version.  To use sdcv,
;; put the dictionary data under ~/.stardict/dic.
(use-package sdcv
  :commands (sdcv-search-pointer
             sdcv-search-input))

(use-package sh-script
  :init (setq sh-basic-offset 2))

(use-package timeline-tools
  :load-path "site-lisp"
  :commands (timeline-tools-format-timeline
             timeline-tools-format-timeline-of-day
             timeline-tools-copy-clocklines
             timeline-tools-clockline-no-org-agenda-conflicts))

(use-package typing
  :commands (typing-of-emacs)
  :init (setq toe-highscore-file nil))

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


;; * Load customizations

(when (file-exists-p custom-file)
  (load-file custom-file))

;;; init.el ends here
