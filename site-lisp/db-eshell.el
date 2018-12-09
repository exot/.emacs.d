;;; db-eshell --- Configuration for eshell -*- lexical-binding: t -*-

;;; Commentary:

;; Parts inspired by:
;; - https://www.masteringemacs.org/article/complete-guide-mastering-eshell
;; - https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;;; Code:


;; Various

(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/default-prompt-function ()
  "A prompt for eshell of the form

   ┌─[$USER@$HOST] [$PWD]
   └─

"
  (let ((head-face '(:foreground "#859900")))
    (format (concat (propertize "┌─" 'face head-face)
                    "%s@%s %s\n"
                    (propertize "└─" 'face head-face)
                    (if (zerop (user-uid)) "#" "$")
                    (propertize " " 'face '(:weight bold)))
            (user-login-name)
            (system-name)
            (propertize (abbreviate-file-name (eshell/pwd))
                        'face '(:foreground "#dc322f")))))

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
