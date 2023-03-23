;;; db-eshell --- Configuration for eshell -*- lexical-binding: t -*-

;;; Commentary:

;; Parts inspired by:
;; - https://www.masteringemacs.org/article/complete-guide-mastering-eshell
;; - https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;;; Code:

(require 'db-customize)
(require 'dash)
(require 'subr-x)
(require 'seq)
(require 'eshell)
(require 'em-basic)
(require 'em-dirs)
(require 'em-hist)
(autoload 'magit-status "magit")
(autoload 'db/find-window-by-buffer-mode "db-utils")


;; Various

(defun db/run-or-hide-eshell (arg)
  "Opens an eshell buffer if not already in one.
Otherwise moves the cursor to the window where we have been before."
  (interactive "P")
  (if (derived-mode-p 'eshell-mode)
      ;; bury buffer; reopen with current working directory if arg is given
      (progn
        (bury-buffer)
        (and arg (db/run-or-hide-eshell arg)))
    (if-let ((eshell-window (db/find-window-by-buffer-mode 'eshell-mode)))
        (select-window eshell-window)
      ;; No running eshell found, open new one.
      (let* ((current-dir (expand-file-name default-directory)))
        (--if-let (display-buffer (eshell 1))
            (select-window it)
          (error "Could not start eshell (`display-buffer' returned nil)"))
        (when arg
          (end-of-line)
          (eshell-kill-input)
          (insert (format "cd '%s'" current-dir))
          (eshell-send-input))))))

(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defcustom db/eshell-prompt-include-git-state (if (member system-type '(windows-nt cygwin)) nil t)
  "Whether to include git state information in the eshell prompt.

State information includes whether the worktree is dirty, whether
the index contains uncommitted changes, whether a merge is in
progress and whether the stash stack is non-empty.

Including this in the prompt might be slow on certain
systems (looking at you, Windows) and may thus not be desirable.
Set to nil to disable."
  :group 'personal-settings
  :type '(choice (const nil) (const t)))

(defun db/eshell-git-branch-string ()
  ;; Inspired by https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#special-prompt
  "Return name of git branch of current directory, as a string.

The format will be BASE-NAME@BASE-DIR[STATE], where BASE-DIR is
the directory containing the .git directory or link file of the
current git repository, and BRANCH-NAME is the name of the
current branch.  STATE will display information about whether the
worktree is dirty or whether the repository needs pushing.  When
no extra state information is available, STATE will be empty and
the brackets will be ommitted.

Return the empty string if the current directory is not part of a
git repository."
  (interactive)
  (let ((pwd (eshell/pwd))
        repo-dir)
    (when (and (not (file-remote-p pwd))
               (eshell-search-path "git")
               (setq repo-dir (locate-dominating-file (file-truename pwd) ".git")))

      (if (string-prefix-p (file-truename (file-name-concat repo-dir ".git"))
                           (file-truename pwd))
          "GIT_DIR"

        (save-match-data
          (let* ((has-HEAD nil)
                 (git-branch (with-temp-buffer
                               (if (zerop (call-process "git" nil t nil
                                                        "rev-parse" "--abbrev-ref" "HEAD"))
                                   (progn
                                     (setq has-HEAD t)
                                     (string-trim (buffer-string)))

                                 ;; When resolving HEAD fails, we do not have any commits yet.
                                 "UNINITIALIZED")))
                 state-list)

            (when (and db/eshell-prompt-include-git-state
                       has-HEAD)

              ;; XXX: This does not work in repositories with gitdir files
              (when (file-exists-p (file-name-concat repo-dir ".git" "MERGE_HEAD"))
                (push "merge" state-list))

              (when (= 1 (call-process "git" nil nil nil
                                       "diff" "--no-ext-diff" "--quiet" "--exit-code"))
                (push "dirty" state-list))

              (when (= 1 (call-process "git" nil nil nil
                                       "diff" "--no-ext-diff" "--quiet" "--exit-code" "--cached"))
                (push "uncommitted" state-list))

              (when (with-temp-buffer
                      (and (= 0 (call-process "git" nil t nil
                                              "stash" "list"))
                           (not (= 0 (buffer-size)))))
                (push "stash" state-list)))

            (let ((base-dir (file-name-nondirectory (directory-file-name repo-dir))))
              (if state-list
                  (format "%s@%s[%s]" git-branch base-dir (apply #'concat (-interpose "|" state-list)))
                (format "%s@%s" git-branch base-dir)))))))))

(defun eshell/default-prompt-function ()
  "A prompt for eshell of the form

   ┌─$USER@$HOST $PWD (current-git-branch)
   └─

Information about the current git branch will be empty when the
current directory is not part of a git repository.  See
`db/eshell-git-branch-string' for more details about the
formatting."
  (let ((head-face '(:foreground "#859900")))
    (concat (propertize "┌─" 'face head-face)
            (user-login-name)
            "@"
            (system-name)
            " "
            (propertize (abbreviate-file-name (eshell/pwd))
                        'face '(:foreground "#dc322f"))
            (when-let ((git-branch (db/eshell-git-branch-string)))
              (format " (git:%s)" git-branch))
            "\n"
            (propertize "└─" 'face head-face)
            (if (zerop (user-uid)) "#" "$")
            (propertize " " 'face '(:weight bold)))))

(defun eshell-insert-history ()
  "Displays the eshell history to select and insert back into your eshell."
  ;; directly taken from Howard Abrams
  (interactive)
  (insert (completing-read "Eshell history: "
                           (seq-uniq (ring-elements eshell-history-ring)))))


;; Git Completion
;; https://tsdh.wordpress.com/2013/05/31/eshell-completion-for-git-bzr-and-hg/

(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (if (not (zerop (call-process "git" nil (current-buffer) nil "help" "--all")))
        (warn "Cannot call `git’ to obtain list of available commands; completion won’t be available.")
      (goto-char 0)
      (let (commands)
        (while (re-search-forward
                "^[[:blank:]]\\{3\\}\\([[:word:]-.]+\\)[[:blank:]]+"
                nil t)
          (push (match-string 1) commands))
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
