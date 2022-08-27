;;; db-eshell --- Configuration for eshell -*- lexical-binding: t -*-

;;; Commentary:

;; Parts inspired by:
;; - https://www.masteringemacs.org/article/complete-guide-mastering-eshell
;; - https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;;; Code:

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
  "Opens an eshell buffer if not already in one, and otherwise
  returns to where we have been before."
  ;; idea to split the current window is from
  ;; http://howardism.org/Technical/Emacs/eshell-fun.html
  (interactive "P")
  (if (string= "eshell-mode" major-mode)
      ;; bury buffer; reopen with current working directory if arg is given
      (progn
        (bury-buffer)
        (delete-window)
        (and arg (db/run-or-hide-eshell arg)))
    (if-let ((eshell-window (db/find-window-by-buffer-mode 'eshell-mode)))
        (select-window eshell-window)
      ;; open eshell
      (let ((current-dir (expand-file-name default-directory))
            (height      (/ (window-total-height) 3)))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell 1)
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

(defun db/eshell-git-branch-string ()
  ;; Inspired by https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#special-prompt
  "Return name of git branch of current directory, as a string.
Return the empty string if the current directory is not part of a
git repository."
  (interactive)
  (let ((pwd (eshell/pwd)))
   (when (and (not (file-remote-p pwd))
              (eshell-search-path "git")
              (locate-dominating-file pwd ".git"))
     (save-match-data
      (string-trim
       (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))))

(defun eshell/default-prompt-function ()
  "A prompt for eshell of the form

   ┌─[$USER@$HOST] [$PWD] (current-git-branch)
   └─

Information about the current git branch will be empty when
the current directory is not part of a git repository.
"
  (let ((head-face '(:foreground "#859900")))
    (concat (propertize "┌─" 'face head-face)
            (user-login-name)
            "@"
            (system-name)
            " "
            (propertize (abbreviate-file-name (eshell/pwd))
                        'face '(:foreground "#dc322f"))
            (when-let ((git-branch (db/eshell-git-branch-string)))
              (format " (%s)" git-branch))
            "\n"
            (propertize "└─" 'face head-face)
            (if (zerop (user-uid)) "#" "$")
            (propertize " " 'face '(:weight bold)))))

(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))

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
