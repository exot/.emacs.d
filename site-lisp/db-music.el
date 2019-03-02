;;; db-music.el -- Music related stuff -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'emms)

(defun db/play-playlist ()
  "Start playing songs from `db/playlist’"
  (interactive)
  (save-window-excursion
    (let ((music-buffer-name "*EMMS Playlist* -- Personal"))
      (unless (get-buffer music-buffer-name)
        (emms-playlist-new music-buffer-name))
      (with-current-buffer (get-buffer music-buffer-name)
        (emms-stop)
        (emms-playlist-set-playlist-buffer)
        (emms-playlist-current-clear)
        (dolist (track db/playlist)
          (when (eq :include (cdr track))
            (emms-playlist-current-insert-source 'emms-insert-file (car track))))
        (goto-char (point-min))
        (emms-shuffle)
        (emms-playlist-select-first)
        (emms-start)))))

(defun db/update-playlist-from-directory (directory)
  "Recursively traverse DIRECTORY and update `db/playlist’.

Files not present `db/playlist’ but that are found in DIRECTORY
are added to `db/playlist’ with tag :undecided, to show the user
that these files are new.  Tracks in `db/playlist’ that are not
found in DIRECTORY are currently removed from it."
  (interactive (list (expand-file-name "songs/"
                                       emms-source-file-default-directory)))
  ;; First convert to hash table for performance
  (let ((playlist-hash (make-hash-table :test #'equal)))
    (dolist (track db/playlist)
      (when (file-exists-p (car track))
        (puthash (car track) (cdr track) playlist-hash)))
    (let (new-playlist)
      (dolist (file (directory-files-recursively directory ""))
        (message "Checking %s" file)
        (push (if-let ((state (gethash file playlist-hash nil)))
                  (cons file state)
                (cons file :undecided))
              new-playlist))
      (customize-save-variable 'db/playlist new-playlist))))

(provide 'db-music)

;;; db-music ends here
