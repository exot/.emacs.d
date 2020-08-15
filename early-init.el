;; -*- lexical-binding: t -*-

(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
