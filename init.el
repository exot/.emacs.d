;;; Init.el --- Daniel's Emacs Configuration

;;; Commentary:

;;; Code:


;; * Packages

(defconst emacs-d (file-name-directory
                   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

(require 'package)

(setq package-user-dir (expand-file-name "elpa" emacs-d))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(setq package-enable-at-startup nil
      load-prefer-newer t)

(package-initialize)

(dolist (package '(diminish use-package bind-key))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(put 'use-package 'common-lisp-indent-function 1)
(setq use-package-verbose t
      use-package-minimum-reported-time 0.01)

(use-package auto-compile
  :config (progn
            (auto-compile-on-load-mode)
            (auto-compile-on-save-mode)))

(add-to-list 'load-path (expand-file-name "site-lisp" emacs-d))


;; * Mode activation

(defun db/run-init ()
  "Run main initialization after everything is set up."

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
                  blink-cursor-mode))
    (when (fboundp mode)
      (funcall mode 0)))

  (when (<= 24 emacs-major-version)
    (electric-indent-mode -1))

  (appt-activate +1)
  (savehist-mode 1)
  (midnight-mode +1)

  (quietly-read-abbrev-file)

  (size-indication-mode 1)
  (display-battery-mode -1)

  (electric-pair-mode +1)
  (electric-quote-mode +1)

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
                  projectile-mode))
    (ignore-errors                      ; don’t barf if mode cannot be loaded
      (funcall mode +1)))

  ;; Color Theme
  ;; setting this in `custom-file’ does not work, so we set it here

  (custom-set-variables
   '(custom-enabled-themes (quote (exot-main
                                   solarized-dark
                                   smart-mode-line-dark))))

  ;; Global Hooks

  (when (package-installed-p 'lispy)
    (add-hook 'minibuffer-setup-hook #'conditionally-enable-lispy))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'text-mode-hook #'turn-on-auto-fill)
  (add-hook 'before-save-hook #'db/delete-trailing-whitespace-maybe)
  (add-hook 'prog-mode-hook #'page-break-lines-mode)
  (when (<= 24 emacs-major-version)
    (add-hook 'prog-mode-hook #'electric-indent-local-mode))
  (add-hook 'lisp-mode-hook #'lispy-mode)
  (unless (eq system-type 'windows-nt)
    (add-hook 'text-mode-hook #'turn-on-flyspell))

  ;; Hydras

  (defhydra hydra-toggle (:color blue)
    "toggle"
    ("c" column-number-mode "column")
    ("d" toggle-debug-on-error "debug-on-error")
    ("e" toggle-debug-on-error "debug-on-error")
    ("f" auto-fill-mode "auto-fill")
    ("l" toggle-truncate-lines "truncate lines")
    ("q" toggle-debug-on-quit "debug-on-quit")
    ("r" read-only-mode "read-only"))

  ;; zooming with single keystrokes (from oremacs)
  (defhydra hydra-zoom (:color red)
    "zoom"
    ("g" text-scale-increase "increase")
    ("l" text-scale-decrease "decrease"))

  ;; rectangle mode
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                             :color pink
                             :post (deactivate-mark))
    "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
    ("h" backward-char nil)
    ("l" forward-char nil)
    ("k" previous-line nil)
    ("j" next-line nil)
    ("e" ora-ex-point-mark nil)
    ("n" copy-rectangle-as-kill nil)
    ("d" delete-rectangle nil)
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1))
         nil)
    ("y" yank-rectangle nil)
    ("u" undo nil)
    ("s" string-rectangle nil)
    ("p" kill-rectangle nil)
    ("o" nil nil))

  (defhydra hydra-ispell (:color blue)
    "ispell"
    ("g" (lambda ()
           (interactive)
           (setq ispell-dictionary "de_DE")
           (ispell-change-dictionary "de_DE"))
         "german")
    ("e" (lambda ()
           (interactive)
           (setq ispell-dictionary "en_US")
           (ispell-change-dictionary "en_US"))
         "english"))

  ;; Top-Level Keybindings

  (bind-key "<XF86Back>" #'winner-undo)
  (bind-key "<XF86Forward>" #'winner-redo)
  (bind-key "<f1>" #'db/run-or-hide-eshell)
  (bind-key "<f2> i" #'counsel-info-lookup-symbol)
  (bind-key "<f2> u" #'counsel-unicode-char)
  (bind-key "<f5>" #'counsel-ag)
  (bind-key "<f6>" #'hydra-zoom/body)
  (bind-key "<f7>" #'dictcc)
  (bind-key "<f8>" #'counsel-locate)
  (bind-key "<f9>" #'counsel-org-goto-all)
  (bind-key "<f10>" #'magit-status)
  (bind-key "<f11>" #'org-capture)
  (bind-key "<f12>" #'db/helm-shortcuts)
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
    (bind-key "M-y" #'helm-show-kill-ring)
    (bind-key "C-c h" #'helm-command-prefix)
    (bind-key "C-c h #" #'helm-emms))

  (when (package-installed-p 'crux)
    (bind-key [remap kill-whole-line] #'crux-kill-whole-line)
    (bind-key [remap open-line] #'crux-smart-open-line-above))

  (when (package-installed-p 'ace-window)
    (bind-key "C-x o" #'ace-window))

  ;; Environment Variables

  (ignore-errors
    (exec-path-from-shell-copy-envs '("SSH_AUTH_SOCK"
                                      "SSH_AGENT_PID"
                                      "PATH"
                                      "TEXMFHOME"
                                      "PERL5LIB"
                                      "PERL_LOCAL_LIB_ROOT"
                                      "PERL_MB_OPT"
                                      "PERL_MM_OPT")))

  ;; Fixes

  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
      (list start end)))

  ;; Start Server when on Windows

  (when (eq system-type 'windows-nt)
    (server-start))

  t)

(add-hook 'after-init-hook #'db/run-init)


;; * Builtin Variables

(use-package db-private
  :load-path "private"
  :defines (db/personal-mail-address
            db/work-mail-address
            db/jabber-id
            db/smtp-accounts
            db/personal-gnus-filter-rules)
  :config  (setq user-full-name "Daniel Borchmann"
                 user-mail-address db/personal-mail-address))

(setq custom-file (expand-file-name "custom.el" emacs-d))
(load-file custom-file)

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
      large-file-warning-threshold 100000000
      echo-keystrokes 0.1
      delete-by-moving-to-trash t
      delete-trailing-lines nil
      x-underline-at-descent-line t
      search-whitespace-regexp "[ \t\r\n]+")

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

(require 'time)
(require 'calendar)

(setq display-time-24hr-format t
      calendar-date-style 'iso
      calendar-view-diary-initially-flag t
      diary-show-holidays-flag nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil
      holiday-solar-holidays nil
      holiday-general-holidays nil
      holiday-other-holidays '((holiday-fixed 5 1 "Labour Day")
                               (holiday-fixed 10 3 "German Unity Day")
                               (holiday-fixed 10 31 "Reformation Day")
                               (holiday-float 11 3 -1 "Day of Repentance and Prayer" 22)))

(setq-default font-lock-maximum-decoration '((t . t)))
(setq-default savehist-file (expand-file-name "savehist" emacs-d))

(require 'tramp)
(setq tramp-save-ad-hoc-proxies t)

(require 'bookmark)
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))

(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "start-tor-browser")

(require 're-builder)
(setq reb-re-syntax 'string)

(setq lisp-indent-function 'common-lisp-indent-function)

(setq shell-file-name (if (eq system-type 'windows-nt)
                          "C:/Programme/cygwin64/bin/bash.exe"
                          "/usr/bin/zsh"))

(setq custom-theme-directory (expand-file-name "themes/" emacs-d))


;; * Basic Builtin Packages

(use-package misc
  :commands (zap-up-to-char zap-to-char))

(use-package grep
  :commands (rgrep zrgrep)
  :config (progn
            (bind-key "C-x C-q" #'wgrep-change-to-wgrep-mode grep-mode-map)
            (bind-key "C-c C-c" #'wgrep-finish-edit          grep-mode-map)))

(use-package winner
  :commands (winner-mode winner-undo winner-redo))

(use-package abbrev
  :commands (quitely-read-abbrev-file)
  :config   (progn
              (setq-default abbrev-mode t)
              (setq save-abbrevs 'silently))
  :diminish abbrev-mode)

(use-package appt
  :commands (appt-activate)
  :config (setq appt-display-mode-line nil))

(use-package ediff
  :defer t
  :config (progn
            (setq ediff-diff-options "-w"
                  ediff-window-setup-function 'ediff-setup-windows-plain
                  ediff-split-window-function 'split-window-horizontally)

            (add-hook 'ediff-keymap-setup-hook
                      '(lambda ()
                        (bind-key "j" 'ediff-next-difference ediff-mode-map)
                        (bind-key "k" 'ediff-previous-difference ediff-mode-map)))

            (add-hook 'ediff-after-quit-hook-internal 'winner-undo)))

(use-package ispell
  :commands (ispell-change-directory)
  :init (progn
          (setq ispell-dictionary "en_US"
                ispell-really-hunspell t)))

(use-package mailcap
  :defer t
  :config (progn
            ;;Remove doc-view so pdf will open with ocular
            (setcdr
             (assoc "application" mailcap-mime-data)
             (remove '("pdf"
                       (viewer . doc-view-mode)
                       (type . "application/pdf")
                       (test eq window-system 'x))
                     (cdr (assoc "application" mailcap-mime-data))))))

(use-package pp
  :commands (db/eval-last-sexp-or-region
             pp-eval-expression)
  :config   (progn
              (defun db/eval-last-sexp-or-region (prefix)
                ;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline140
                "Eval region from BEG to END if active, otherwise the last sexp."
                (interactive "P")
                (if (and (mark) (use-region-p))
                    (eval-region (min (point) (mark)) (max (point) (mark)))
                  (pp-eval-last-sexp prefix)))))

(use-package quail
  :defer t
  :config (progn
            (defun db/add-symbols-to-TeX-input-method ()
              (when (string= current-input-method "TeX")
                (let ((quail-current-package (assoc "TeX" quail-package-alist)))
                  (quail-define-rules
                   ((append . t))
                   ("\\land" ?∧)
                   ("\\lor" ?∨)
                   ("\\lnot" ?¬)
                   ("\\implies" ?⇒)
                   ("\\powerset" ?𝔓)
                   ("\\mathbbK" ?𝕂)
                   ("\\mathbbR" ?ℝ)
                   ("\\mathbbN" ?ℕ)
                   ("\\mathbbZ" ?ℤ)
                   ("\\mathbbP" ?ℙ)
                   ("\\mathcalA" ?𝒜)
                   ("\\mathcalB" ?ℬ)
                   ("\\mathcalC" ?𝒞)
                   ("\\mathcalD" ?𝒟)
                   ("\\mathcalE" ?ℰ)
                   ("\\mathcalH" ?ℋ)
                   ("\\mathcalI" ?ℐ)
                   ("\\mathcalJ" ?𝒥)
                   ("\\mathcalK" ?𝒦)
                   ("\\mathcalL" ?ℒ)
                   ("\\mathcalM" ?ℳ)
                   ("\\mathcalR" ?ℛ)
                   ("\\mathcalQ" ?𝒬)
                   ("\\mathcalS" ?𝒮)
                   ("\\mathfrakP" ?𝔓)))))
            (add-hook 'input-method-activate-hook
                      #'db/add-symbols-to-TeX-input-method)))


;; * Org Mode

(use-package org
  :commands (org-agenda
             org-capture
             org-store-link
             org-clock-save
             db/export-diary
             hydra-org-clock/body)
  :config (progn
            (setq org-home-task-id "ade37344-ccd9-4c93-aa15-406886a86627"
                  org-working-task-id "a686ba96-116e-457b-94c1-5dbc42f9c051")

            (setq db/org-default-work-file nil
                  db/org-default-home-file "~/Documents/home/admin/home.org"
                  db/org-default-notes-file "~/Documents/home/misc/notes.org"
                  db/org-default-refile-file "~/Desktop/refile.org"
                  db/org-default-pensieve-file "~/Documents/home/misc/pensieve.org.gpg")

            (require 'db-org)

            ;; avoid important buffers to end up in `org-agenda-new-buffers’ by
            ;; opening them manually
            (mapc #'find-file-noselect org-agenda-files)

            (run-with-timer 0 3600 #'org-clock-save)
            (unless (eq system-type 'windows-nt)
              (run-with-idle-timer 1200 t #'db/export-diary))))

(use-package org-ref
  :defer t
  :config (progn
            (require 'org-ref-pdf)
            (require 'org-ref-url-utils)

            (setq org-ref-default-bibliography '("~/Documents/uni/research/references.bib")
                  org-ref-pdf-directory "~/Documents/library/.bibtex-pdfs/"
                  org-ref-bibliography-notes "~/Documents/uni/research/references.org")))


;; * Some essential packages

(use-package crux
  :commands (crux-eval-and-replace
             crux-smart-open-line-above
             crux-kill-whole-line
             crux-cleanup-buffer-or-region))

(use-package db-utils
  :commands (endless/fill-or-unfill
             db/delete-trailing-whitespace-maybe
             db/go-dark
             db/go-light
             db/show-current-org-task
             db/run-or-hide-shell
             db/run-or-hide-eshell
             db/run-or-hide-ansi-term
             db/helm-shortcuts))

(use-package db-emacsclient)

(use-package hydra
  :commands (defhydra))

(use-package magit
  :commands (magit-status)
  :config (progn
            (setq magit-diff-refine-hunk t
                  magit-commit-show-diff nil
                  magit-popup-use-prefix-argument 'default)

            (add-to-list 'magit-no-confirm 'stage-all-changes)

            (require 'projectile)
            (setq magit-repository-directories
                  (mapcar
                   (lambda (dir)
                     (substring dir 0 -1))
                   (cl-remove-if-not
                    (lambda (project)
                      (unless (file-remote-p project)
                        (file-exists-p (concat project "/.git"))))
                    projectile-known-projects)))))

(use-package projectile
  :commands (projectile-mode)
  :defines (projectile-known-projects)
  :config (progn
            (setq projectile-switch-project-action 'projectile-dired
                  projectile-completion-system 'ivy
                  projectile-ignored-project-function #'file-remote-p)
            (projectile-cleanup-known-projects))
  :diminish projectile-mode)

(use-package counsel-projectile
  :commands counsel-projectile)

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-copy-envs))


;; * Mail

(use-package bbdb
  :commands (bbdb-search-name bbab-initialize bbdb-mua-auto-update-init bbdb-save)
  :config (progn
            (setq bbdb-completion-display-record nil
                  bbdb-complete-mail-allow-cycling t
                  bbdb-mua-auto-update-p 'query
                  bbdb-default-country "Germany"
                  bbdb-user-mail-address-re (regexp-opt
                                             (list db/personal-mail-address
                                                   db/work-mail-address))
                  bbdb-file (expand-file-name "private/bbdb" emacs-d))
            (add-hook 'message-setup-hook 'bbdb-mail-aliases)
            (add-hook 'mail-setup-hook 'bbdb-mail-aliases)
            (run-with-timer 0 3600 #'bbdb-save)))

(use-package gnus
  :defines (gnus-init-file)
  :commands (gnus)
  :init   (setq gnus-group-update-tool-bar nil)
  :config (progn
            (bbdb-initialize 'gnus 'message)
            (bbdb-mua-auto-update-init 'message)
            (setq gnus-init-file (expand-file-name "gnus" emacs-d)
                  gnus-home-directory (expand-file-name "~/Mail/news/")
                  gnus-directory (expand-file-name "~/Mail/news/")
                  gnus-kill-files-directory gnus-directory
                  gnus-startup-file (expand-file-name "~/Mail/gnus-newsrc")
                  gnus-cache-directory (expand-file-name
                                        "cache/" gnus-directory))))


;; * Crypto

(use-package nsm
  :defer t
  :config (progn (setq network-security-level 'medium
                       nsm-save-host-names t
                       nsm-settings-file (expand-file-name
                                          "private/network-security.data"
                                          emacs-d))))

(use-package gnutls
  :defer t
  :config (progn
            (setq gnutls-log-level 0
                  gnutls-min-prime-bits 1024
                  gnutls-verify-error t)

            ;; add own trustfiles
            (dolist (cert-file
                      (directory-files "~/.local/etc/certs" t "\.crt$"))
              (add-to-list 'gnutls-trustfiles cert-file))))

(use-package epg
  :defer t
  :config (progn
            (setq epg-debug t)))


;; * Appearance

(use-package solarized-theme
  :defer t
  :init (setq solarized-use-less-bold t
              solarized-emphasize-indicators t
              solarized-use-variable-pitch nil))

(use-package smart-mode-line
  :commands (sml/setup)
  :init (setq sml/use-projectile-p t))


;; * Dired

(use-package dired
  :defer t
  :config (progn
            (setq dired-dwim-target t)
            (put 'dired-find-alternate-file 'disabled nil)
            (setq dired-listing-switches "-alLh")
            (setq dired-hide-details-hide-information-lines nil)
            (setq dired-recursive-copies 'top)
            (setq dired-recursive-deletes 'top)

            (require 'dired-x)

            ;; Gnus support in dired
            (require 'gnus-dired)
            (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

            ;; omitting files
            (add-hook 'dired-mode-hook #'dired-omit-mode)
            (setq dired-omit-files
                  (concat dired-omit-files "\\|^\\...+$"))
            (dolist (extension '(".out" ".synctex.gz" ".thm"))
              (add-to-list 'dired-latex-unclean-extensions extension))

            (setq dired-guess-shell-alist-user
                  '(("\\.pdf\\'" "evince")
                    ("\\.ps\\'" "evince")
                    ("\\.\\(?:djvu\\|eps\\)\\'" "evince")
                    ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "eog")
                    ("\\.\\(?:xcf\\)\\'" "gimp")
                    ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
                     "vlc")
                    ("\\.\\(?:mp3\\|flac\\|ogg\\)\\'" "mplayer")
                    ("\\.html?\\'" "firefox")
                    ("\\.docx?\\'" "loffice")))

            (unbind-key "C-M-o" dired-mode-map)

            (require 'dired+)
            (custom-set-variables       ; needs to be set with custom
             '(diredp-hide-details-initially-flag nil))

            ;; disable exaggerated fontification of dired+
            (require 'font-lock)
            (add-to-list 'font-lock-maximum-decoration '(wdired-mode . 1))
            (add-to-list 'font-lock-maximum-decoration '(dired-mode . 1))

            ;; https://oremacs.com/2017/03/18/dired-ediff/
            (defun ora-ediff-files ()
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

            (require 'dired-quick-sort)
            (dired-quick-sort-setup)

            (bind-key [remap beginning-of-buffer]
                      #'dired-back-to-top dired-mode-map)
            (bind-key [remap end-of-buffer]
                      #'dired-jump-to-bottom dired-mode-map)
            (bind-key "z" 'dired-get-size dired-mode-map)
            (unbind-key "s" dired-mode-map)
            (unbind-key "<f1>" dired-mode-map)
            (bind-key "e" #'ora-ediff-files dired-mode-map)))

(use-package find-dired
  :commands (find-dired)
  :config (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))


;; * Completion

(use-package hippie-exp
  :commands (hippie-expand))

(use-package helm
  :commands (helm-show-kill-ring)
  :diminish helm-mode
  :defines (helm-command-prefix-key
            helm-command-prefix
            helm-command-map
            helm-completing-read-handlers-alist)
  :init   (require 'helm-config)
  :config (progn
            (setq helm-input-idle-delay 0.0
                  helm-buffers-fuzzy-matching t
                  helm-recentf-fuzzy-match t
                  helm-mode-fuzzy-match t
                  helm-autoresize-min-height 20
                  helm-ff-auto-update-initial-value t
                  helm-ff-file-name-history-use-recentf t
                  helm-ff-search-library-in-sexp t
                  helm-ff-skip-boring-files nil
                  helm-split-window-in-side-p t
                  helm-move-to-line-cycle-in-source nil
                  helm-scroll-amount nil
                  helm-locate-command nil
                  helm-candidate-number-limit 100
                  helm-follow-mode-persistent t
                  helm-buffer-details-flag t
                  helm-buffer-skip-remote-checking t)

            (unbind-key helm-command-prefix-key)

            (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
            (bind-key "C-i" 'helm-execute-persistent-action helm-map)
            (bind-key "C-z" 'helm-select-action helm-map)

            (require 'db-utils)

            (setq helm-mini-default-sources '(helm-source-buffers-list
                                              helm-source-recentf
                                              db/helm-frequently-used-features
                                              db/helm-frequently-visited-locations
                                              helm-source-buffer-not-found
                                              helm-source-bookmarks
                                              helm-source-bookmark-set))))

(use-package ivy
  :commands (ivy-mode
             ivy-resume)
  :diminish ivy-mode
  :config (progn
            (setq ivy-use-virtual-buffers t
                  enable-recursive-minibuffers t)
            (setq ivy-initial-inputs-alist '((counsel-describe-function . "^")
                                             (counsel-describe-variable . "^")
                                             (man . "^")
                                             (woman . "^")))))

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
  :commands (swiper))

(use-package recentf
  :commands (recentf-mode recentf-save-list)
  :init (setq recentf-max-saved-items 1000)
  :config (run-with-timer 0 3600 #'recentf-save-list))

(use-package company
  :commands (company-mode global-company-mode))


;; * Navigation

(use-package ace-window
  :commands (ace-window ace-window-display-mode)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
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
  :config (setq dumb-jump-selector 'helm))


;; * Media

(use-package db-emms
  :commands (emms
             emms-stream-init
             db/play-playlist
             emms-cache-save
             emms-play-directory-tree
             emms-control/body)
  :config (run-with-timer 0 3600 #'emms-cache-save))

(use-package helm-emms
  :commands (helm-emms)
  :config (progn
            (require 'db-emms)
            (require 'helm-adaptive)
            (setq helm-emms-default-sources
                  '(helm-source-emms-streams
                    helm-source-emms-dired
                    helm-source-emms-files))
            (setq helm-emms-use-track-description-function t)))


;; * Shells and such

(use-package comint
  :defer t
  :config (progn
            (setq comint-scroll-to-bottom-on-input t)
            (setq comint-scroll-to-bottom-on-output nil)
            (setq comint-scroll-show-maximum-output t)
            (setq comint-completion-addsuffix t)
            (setq comint-buffer-maximum-size 100000)
            (setq comint-input-ring-size 5000)))

(use-package term
  :commands (term-send-string)
  :config (progn
            (setq explicit-shell-file-name shell-file-name)

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
            (bind-key "C-c" 'term-send-raw term-raw-map)

            ;; unbind some keys to allow the global keymap to handle them
            (unbind-key "M-:" term-raw-map)
            (unbind-key "C-h" term-raw-map)
            (unbind-key "M-x" term-raw-map)
            (unbind-key "M-o" term-raw-map)

            ;; we need to set keys starting with C-x after `ansi-term' has been
            ;; called, as it resets the escape character to C-x.
            (defadvice ansi-term (after ansi-term-set-keys activate)
              (unbind-key "C-x C-j" term-raw-map)
              (unbind-key "C-x g" term-raw-map))))

(use-package ansi-color
  :commands (ansi-color-for-comint-mode-on)
  :config (progn
            ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
            (defun endless/colorize-compilation ()
              "Colorize from `compilation-filter-start' to `point'."
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region compilation-filter-start (point))))

            (add-hook 'compilation-filter-hook #'endless/colorize-compilation)))

(use-package shell
  :defer t
  :config (progn
            (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
            (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

(use-package eshell
  :commands (eshell)
  :config (use-package db-eshell))


;; * Lisp

;; General Stuff first

(use-package lispy
  :commands (lispy-mode conditionally-enable-lispy)
  :diminish lispy-mode
  :config (progn
            (defun conditionally-enable-lispy ()
              (when (or (eq this-command 'eval-expression)
                        (eq this-command 'pp-eval-expression))
                (lispy-mode 1)))))

(use-package eldoc
  :commands (eldoc-mode)
  :diminish eldoc-mode)

;; Lisp Dialects

(use-package elisp-mode
  :defer t
  :config (progn (add-hook 'emacs-lisp-mode-hook #'lispy-mode)

                 (defun db/add-use-package-to-imenu ()
                   (add-to-list 'imenu-generic-expression
                                '("Used Packages"
                                  "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)"
                                  2)))
                 (add-hook 'emacs-lisp-mode-hook #'db/add-use-package-to-imenu)

                 (add-hook 'ielm-mode-hook #'eldoc-mode)
                 (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)))

(use-package geiser
  :commands (geiser-mode))

(use-package cider
  :commands (cider-jack-in)
  :config (progn
            (setq nrepl-hide-special-buffers t
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

            (add-hook 'cider-repl-mode-hook #'subword-mode)
            (add-hook 'cider-repl-mode-hook #'lispy-mode)
            (add-hook 'cider-repl-mode-hook #'cider-repl-toggle-pretty-printing)
            (add-hook 'cider-repl-mode-hook #'company-mode)
            (add-hook 'cider-mode-hook #'eldoc-mode)

            (setq cider-cljs-lein-repl "(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))")))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode))
  :config (progn (define-clojure-indent
                     (forall 'defun)
                     (exists 'defun)
                   (dopar 'defun))
                 (add-hook 'clojure-mode-hook #'lispy-mode)
                 (add-hook 'clojure-mode-hook #'clj-refactor-mode)
                 (add-hook 'clojure-mode-hook #'yas-minor-mode)
                 (add-hook 'clojure-mode-hook #'company-mode)))

(use-package clj-refactor
  :commands (clj-refactor-mode)
  :config (progn
            (cljr-add-keybindings-with-prefix "C-c C-m")
            (setq cljr-eagerly-build-asts-on-startup nil
                  cljr-warn-on-eval nil)))

(use-package slime
  :commands (slime slime-mode slime-connect)
  :mode     (("\\.cl\\'" . lisp-mode)
             ("\\.lisp\\'" . lisp-mode))
  :init     (progn
              (setq inferior-lisp-program "sbcl --noinform --no-linedit")
              (add-hook 'lisp-mode-hook '(lambda () (slime-mode +1)) t))
  :config   (progn
              (setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))
              (make-directory "/tmp/slime-fasls/" t)

              (slime-setup '(slime-repl slime-fancy slime-autodoc))

              (setq slime-net-coding-system 'utf-8-unix
                    slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
              (add-hook 'slime-mode-hook 'slime-redirect-inferior-output)

              (defun db/slime-reload ()
                (interactive)
                (mapc 'load-library
                      (reverse (cl-remove-if-not
                                (lambda (feature) (string-prefix-p "slime" feature))
                                (mapcar 'symbol-name features))))
                (setq slime-protocol-version (slime-changelog-date))
                (load-slime))

              (setq slime-lisp-implementations
                    '((sbcl  ("sbcl")  :coding-system utf-8-unix)
                      (cmucl ("cmucl") :coding-system utf-8-unix)
                      (ccl   ("ccl")   :coding-system utf-8-unix)))

              (setq slime-repl-history-remove-duplicates t
                    slime-repl-history-trim-whitespaces t)))

(use-package hy-mode
  :commands (hy-mode org-babel-execute:hy)
  :config (progn
            (add-hook 'hy-mode-hook #'lispy-mode)
            (add-hook 'hy-mode-hook #'inferior-lisp)

            (defun org-babel-execute:hy (body params)
              ;; http://kitchingroup.cheme.cmu.edu/blog/2016/03/30/OMG-A-Lisp-that-runs-python/
              (let* ((temporary-file-directory ".")
                     (tempfile (make-temp-file "hy-")))
                (with-temp-file tempfile
                  (insert body))
                (unwind-protect
                     (shell-command-to-string
                      (format "hy %s" tempfile))
                  (delete-file tempfile))))))


;; * TeX

(use-package reftex
  :commands (turn-on-reftex)
  :init (add-hook 'latex-mode-hook 'turn-on-reftex)  ; with Emacs latex mode
  :config (progn
            (eval-after-load 'helm-mode
              '(add-to-list
                'helm-completing-read-handlers-alist
                '(reftex-citation . nil)))
            (setq reftex-plug-into-AUCTeX t)
            (setq reftex-default-bibliography
                  '("~/Documents/uni/research/references.bib"))))

(eval-after-load 'tex-mode
  '(use-package db-latex))

(use-package ebib
  :commands (ebib))

(use-package helm-bibtex
  :commands (helm-bibtex)
  :config (progn
            (setq bibtex-completion-bibliography "~/Documents/uni/research/references.bib"
                  bibtex-completion-library-path "~/Documents/library/.bibtex-pdfs/"
                  bibtex-completion-notes-path "~/Documents/uni/research/references.org")))


;; * Various Mode Configurations

;; These are packages that are not essential, but still nice to have.  They
;; provide optional functionality and may redefine builtin commands.

(use-package cperl-mode
  :commands (cperl-mode)
  :mode (("\\.plx\\’" . cperl-mode))
  :init (progn
          ;; replace perl-mode with cperl-mode
          (mapc
           (lambda (pair)
             (if (eq (cdr pair) 'perl-mode)
                 (setcdr pair 'cperl-mode)))
           (append auto-mode-alist interpreter-mode-alist)))
  :config (progn
            (add-hook 'cperl-mode-hook 'flycheck-mode)
            (add-hook 'cperl-mode-hook 'prettify-symbols-mode)
            (setq cperl-hairy nil)))

(use-package define-word
  :commands (define-word-at-point define-word))

(use-package dictcc
  :commands (dictcc))

(use-package edit-list
  :commands edit-list)

(use-package electric
  :commands (electric-quote-mode))

(use-package elec-pair
  :commands (electric-pair-mode
             db/turn-off-local-electric-pair-mode)
  :config   (progn
              (add-to-list 'electric-pair-pairs '(?“ . ?”))
              (add-to-list 'electric-pair-text-pairs '(?“ . ?”))
              (add-to-list 'electric-pair-pairs '(?„ . ?“))
              (add-to-list 'electric-pair-text-pairs '(?„ . ?“))

              (defun db/turn-off-local-electric-pair-mode ()
                "Turn off `electric-pair-mode’ locally."
                (interactive)
                (electric-pair-local-mode -1))))

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
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-mode))
  :defines (haskell-program-name)
  :config (progn
            (setq haskell-program-name "ghci")
            (add-hook 'haskell-mode-hook 'haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            (add-hook 'haskell-mode-hook 'company-mode)
            (add-hook 'haskell-mode-hook
                      (lambda ()
                        (set (make-local-variable 'company-backends)
                             (append '((company-capf company-dabbrev-code))
                                     company-backends))))
            (add-hook 'haskell-mode-hook 'flycheck-mode)

            (require 'haskell-indentation)
            (add-hook 'haskell-mode-hook
                      'haskell-indentation-mode)

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

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))

(use-package mastodon
  :commands (mastodon)
  :config (progn
            (setq mastodon-instance-url "https://mastodon.blue/")))

(use-package multiple-cursors
  :commands (mc/edit-lines
             mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this))

(use-package nxml
  :mode (("\\.html\\'" . nxml-mode)
         ("\\.xml\\'"  . nxml-mode)))

(use-package page-break-lines
  :commands (page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package python
  :defer t
  :config (progn
            (setq python-indent-offset 4
                  python-shell-interpreter "/usr/bin/python")
            (add-hook 'python-mode-hook #'highlight-indentation-mode)))

(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode)))

(use-package semantic
  :commands (semantic-mode)
  :config (progn
            (require 'semantic/ia)
            (require 'semantic/bovine/el)

            ;; recognize `use-package' as include statement; for some reason,
            ;; this form needs to be wrapped in a backquote so that the lambda
            ;; form is evaluated before parser is installed; otherwise, the
            ;; lambda-form is not recognized as a function and the parsing does
            ;; not work
            (eval-after-load 'semantic/bovine/el
              `(semantic-elisp-setup-form-parser
                   ,(lambda (form start end)
                      (semantic-tag-new-include (symbol-name (nth 1 form)) nil))
                 use-package))))

(use-package synonyms
  :config (setq synonyms-file "~/.local/share/thesaurus/mthesaur.txt"
                synonyms-cache-file "~/.emacs.d/mthesaur.txt.cache")
  :commands (synonyms))

(use-package undo-tree
  :commands (global-undo-tree-mode
             undo
             undo-tree-redo)
  :config   (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff t)
  :diminish undo-tree-mode)

(use-package wgrep
  :commands (wgrep-finish-edit
             wgrep-change-to-wgrep-mode))

(use-package which-key
  :commands (which-key-mode)
  :diminish which-key-mode
  :config   (progn (which-key-setup-side-window-bottom)
                   (setq which-key-side-window-max-width 0.33
                         which-key-side-window-max-height 0.25)))

(use-package yasnippet
  :commands (yas-minor-mode-on yas-minor-mode)
  :diminish yas-minor-mode
  :init   (add-hook 'text-mode-hook #'yas-minor-mode-on)
  :config (progn
            (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
            (yas-reload-all)))


;; * End

(provide 'init)

;;; init.el ends here
