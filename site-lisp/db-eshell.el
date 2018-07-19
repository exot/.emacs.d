;;; db-eshell --- Configuration for eshell -*- lexical-binding: t -*-

;;; Commentary:

;; Parts inspired by:
;; - https://www.masteringemacs.org/article/complete-guide-mastering-eshell
;; - https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;;; Code:

(require 'em-prompt)
(require 'em-term)
(require 'em-cmpl)


;; Customization

(setq eshell-cmpl-cycle-completions nil
      eshell-scroll-to-bottom-on-input t
      eshell-prefer-lisp-functions nil
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-destroy-buffer-when-process-dies t)

(setenv "PAGER" "cat")

(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-to-list 'eshell-command-completions-alist
             '("gunzip" "gz\\'"))

(add-to-list 'eshell-command-completions-alist
             '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

(defun eshell/default-prompt-function ()
  "A prompt for eshell of the form

   ┌[$USER@$HOST] [$PWD]
   └──

"
  (let ((head-face '(:foreground "#859900")))
    (format (concat (propertize "┌" 'face head-face)
                    "[%s@%s] [%s]\n"
                    (propertize "└──" 'face head-face)
                    " ")
            (user-login-name)
            (system-name)
            (propertize (abbreviate-file-name (eshell/pwd))
                        'face '(:foreground "#dc322f")))))

(setq eshell-prompt-function #'eshell/default-prompt-function
      eshell-prompt-regexp "└── "
      eshell-highlight-prompt nil)

(add-hook 'eshell-mode-hook
          'with-editor-export-editor)

(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))

(defun eshell-insert-history ()
  "Displays the eshell history to select and insert back into your eshell."
  ;; directly taken from Howard Abrams
  (interactive)
  (insert (completing-read "Eshell history: "
                           (delete-dups
                            (ring-elements eshell-history-ring)))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (bind-key "C-a" #'eshell-bol eshell-mode-map)
            (bind-key "C-l" #'eshell-clear-buffer eshell-mode-map)
            (bind-key "M-r" #'eshell-insert-history eshell-mode-map)
            (bind-key "M-P" #'eshell-previous-prompt)
            (bind-key "M-N" #'eshell-next-prompt)))


;; Git Completion
;; https://tsdh.wordpress.com/2013/05/31/eshell-completion-for-git-bzr-and-hg/

(require 'pcomplete)

(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (if (not (zerop (call-process "git" nil (current-buffer) nil "help" "--all")))
        (warn "Cannot call `git’ to obtain list of available commands; completion won’t be available.")
      (goto-char 0)
      (search-forward "available git commands in")
      (let (commands)
        (while (re-search-forward
                "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
                nil t)
          (push (match-string 1) commands)
          (when (match-string 2)
            (push (match-string 2) commands)))
        (sort commands #'string<)))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
    ((pcomplete-match "help" 1)
     (pcomplete-here* pcmpl-git-commands))
    ((pcomplete-match (regexp-opt '("pull" "push")) 1)
     (pcomplete-here (pcmpl-git-remotes)))
    ;; provide branch completion for the command `checkout'.
    ((pcomplete-match (regexp-opt '("checkout" "co")) 1)
     (pcomplete-here* (append (pcmpl-git-get-refs "heads")
                              (pcmpl-git-get-refs "tags"))))
    (t
     (while (pcomplete-here (pcomplete-entries))))))


;; End

(provide 'db-eshell)
;;; db-eshell ends here
