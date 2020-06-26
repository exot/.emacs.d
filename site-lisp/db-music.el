;;; db-music.el -- Music related stuff -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'subr-x)
(require 'seq)
(require 'emms)
(require 'emms-source-file)

(defgroup db-music nil
  "General configurations for music-related functionality."
  :prefix "db-music"
  :group 'convenience
  :tag "db-music")

(defcustom db/auto-playlist-file-function #'db/play-playlist-from-cache
  "Function that has to return a list of all music files that
should be included in the auto playlist."
  :group 'db-music
  :type 'function)

(defun db/play-auto-playlist ()
  "Generate playlist using `db/auto-playlist-file-function’ and
start playing it.

Current backend is EMMS."
  (interactive)
  (db/-emms-playlist-from-files (funcall db/auto-playlist-file-function)))

;; Idea: make this customizable, so that we can later switch to another backend
;; if necessary

(defun db/-emms-playlist-from-files (files)
  "Generate EMMS playlist from FILES.

Shuffle it and start playing it afterwards."
  (when (seq-empty-p files)
    (user-error "List of files is empty, nothing to do"))
  (save-window-excursion
    (let ((music-buffer-name "*EMMS Playlist* -- Personal"))
      (unless (get-buffer music-buffer-name)
        (emms-playlist-new music-buffer-name))
      (with-current-buffer (get-buffer music-buffer-name)
        (emms-stop)
        (emms-playlist-set-playlist-buffer)
        (emms-playlist-current-clear)
        (dolist (track files)
          (emms-playlist-current-insert-source 'emms-insert-file track))
        (goto-char (point-min))
        (emms-shuffle)
        (emms-playlist-select-first)
        (emms-start)))))

(defcustom db/playlist nil
  "List of songs to include in a random playlist."
  :group 'db-music
  :type '(alist :value-type (choice (const :tag "Undecided" :undecided)
                                    (const :tag "Include" :include)
                                    (const :tag "Exclude" :exclude))
                :key-type file))

(defun db/playlist-files-from-cache ()
  "Generate files for auto playlist from `db/playlist’ cache."
  (interactive)
  (->> db/playlist
       (cl-remove-if-not #'(lambda (track)
                             (eq (cdr track) :include)))
       (mapcar #'car)))

(defun db/update-playlist-cache-from-directory (directory)
  "Recursively traverse DIRECTORY and update `db/playlist’.

Files not present `db/playlist’ but that are found in DIRECTORY
are added to `db/playlist’ with tag :undecided, to show the user
that these files are new.  Tracks in `db/playlist’ that do not
exist anymore are removed from it."
  (interactive (list (expand-file-name "songs/"
                                       emms-source-file-default-directory)))
  ;; First convert to hash table for performance
  (let ((playlist-hash (make-hash-table :test #'equal)))
    (dolist (track db/playlist)
      (when (file-exists-p (car track))
        (puthash (car track) (cdr track) playlist-hash)))

    (let (new-playlist)
      ;; Iterate over files in DIRECTORY and add them to the playlist, with the
      ;; already known state whenever possible
      (dolist (file (directory-files-recursively directory ""))
        (message "Checking %s" file)
        (push (cons file (gethash file playlist-hash :undecided))
              new-playlist)
        (remhash file playlist-hash))

      ;; Keep all other tracks that are not in DIRECTORY
      (maphash #'(lambda (track state)
                   (push (cons track state) new-playlist))
               playlist-hash)

      ;; Sort to keep version control happy
      (setq new-playlist
            (sort new-playlist
                  #'(lambda (track-1 track-2)
                      (string< (car track-1) (car track-2)))))

      ;; Save and exit
      (customize-save-variable 'db/playlist new-playlist))))

(defun db/playlist-files-from-git-annex-find (match-expression)
  "Generate playlist from git annex find on MATCH-EXPRESSION.

Prompts for MATCH-EXPRESSION when called interactively.
Generates playlist that is comprised of exactly those files that
are match it.  Assumes `emms-source-file-default-directory’ to be
part of a git-annex repository, and will complain otherwise."
  (interactive "smatch expression: ")
  (let* ((default-directory emms-source-file-default-directory))
    (->> (split-string (with-output-to-string
                         (with-current-buffer standard-output
                           (let ((return-value (apply #'call-process
                                                      "git" nil t nil
                                                      "annex" "find"
                                                      (split-string match-expression))))
                             (unless (zerop return-value)
                               (error "Call to `git-annex-find’ failed: %s"
                                      (buffer-string))))))
                       "\n")
         (cl-remove-if-not #'(lambda (path)
                               (and (not (string-empty-p path))
                                    (file-exists-p path)
                                    (file-readable-p path))))
         (mapcar #'(lambda (path)
                     (expand-file-name
                      path
                      emms-source-file-default-directory))))))

(defun db/play-auto-playlist-from-git-annex-find ()
  "Interactively query user for a git-annex match expression and
  play resulting list of audio files.

See `db/playlist-files-from-git-annex-find’ for more details."
  (interactive)
  (db/-emms-playlist-from-files
   (call-interactively #'db/playlist-files-from-git-annex-find)))

(provide 'db-music)

;;; db-music ends here
