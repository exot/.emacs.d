;;; db-music.el -- Music related stuff -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'subr-x)
(require 'seq)
(require 'emms)
(require 'emms-source-file)
(require 'emms-playlist-sort)
(require 'emms-info)
(require 'hydra)
(require 'db-emms)

(defgroup db-music nil
  "General configurations for music-related functionality."
  :prefix "db-music"
  :group 'convenience
  :tag "db-music")


;; Autogeneration of Playlist

(defcustom db/auto-playlist-file-function #'db/play-auto-playlist-from-git-annex-find
  "Function returning all music files of an automatically generated playlist.

This function should return a list of file names of music files."
  :group 'db-music
  :type 'function)

(defun db/play-auto-playlist ()
  "Automatically generate playlist and play it.

Will use `db/auto-playlist-file-function’ for generating that
playlist.  Current backend is EMMS."
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

(defun db/playlist-files-from-git-annex-find (match-expression)
  "Generate list of files from git annex find on MATCH-EXPRESSION.

Prompts for MATCH-EXPRESSION when called interactively.
Generates a list of absolute file names that is comprised of
exactly those files that match it.  Assumes the default EMMS file
directory as specified by `emms-source-file-default-directory’ to
be part of a git-annex repository, complaining otherwise."
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
  "Query for match expression and play resulting audio files.

The match expression must be suitable for git-annex to find the
desired files.  See `db/playlist-files-from-git-annex-find’ for
more details."
  (interactive)
  (db/-emms-playlist-from-files
   (call-interactively #'db/playlist-files-from-git-annex-find)))

(defhydra music-control (:color red :hint none)
  "\nPlaying: %s(db/emms-track-status)

  _n_: ?n?          _p_: ?p?
_RET_: ?RET?    _s_: ?s?
  _-_: lower volume  _+_: ?+?
  _P_: ?P?
  _M_: ?M?\n"
  ("n" emms-next         "next")
  ("p" emms-previous     "previous")
  ("RET" emms-pause      "play/pause")
  ("s" emms-stop         "stop playing")
  ("-" emms-volume-lower "lower volume")
  ("+" emms-volume-raise "raise volume")
  ("M" emms              "Show playlist in new EMMS buffer")
  ("P" db/play-auto-playlist "Play automatically generated playlist"))


;; Playlist management

(cl-defun db/write-m3u-playlist-from-git-annex-find
    (file match-expression
          &optional (base-dir emms-source-file-default-directory) overwrite)
  "Write an M3U playlist to FILE based on a git-annex MATCH-EXPRESSION.
The playlist will contain all files found by git-annex-find using
MATCH-EXPRESSION.  Conduct search with git-annex-find in
BASE-DIR.  Query for overwrite if FILE already exists, unless
OVERWRITE is non-nil."
  (interactive "FFile name of playlist: \nsPlaylist name: \nsgit annex match-expression: ")
  (let ((base-dir (expand-file-name base-dir)))
    (unless (file-accessible-directory-p base-dir)
      (user-error "Error: “%s” is not a valid directory" base-dir))
    (unless (or (not (file-exists-p file))
                overwrite
                (yes-or-no-p (format "File %s already exists, overwrite?" file)))
      (user-error "Error: %s exists and shall not be overwritten, aborting" file))
    (let ((default-directory base-dir))
      (let* ((return-code nil)
             (output (with-output-to-string
                       (with-current-buffer standard-output
                         (setq return-code (apply #'call-process
                                                  "git" nil t nil
                                                  "annex" "find"
                                                  (split-string match-expression)))))))
        (if (not (zerop return-code))
            (error "%s" output)
          (let ((emms-source-playlist-ask-before-overwrite nil)
                (emms-temp-playlist-buffer (emms-playlist-new " *EMMS Playlist Export*"))
                (emms-info-asynchronously nil))
            (with-current-buffer emms-temp-playlist-buffer
              (let ((emms-playlist-buffer (current-buffer)))
                (emms-playlist-clear)
                (dolist (track (split-string output "[\n\r]" 'omit-nulls))
                  (emms-insert-file track))
                (emms-playlist-sort-by-info-title)
                (emms-playlist-sort-by-info-artist)
                ;; When writing the playlist, we simulate the current buffer to be
                ;; the current playlist, as otherwise `emms-playlist-save' will
                ;; ask for confirmation.
                (emms-playlist-save 'm3u file)))
            (kill-buffer emms-temp-playlist-buffer)

            ;; Convert absolute file names to relative file names
            (with-current-buffer (or (find-buffer-visiting file)
                                     (find-file-noselect file))
              ;; Make sure the current buffer is up to date with the file on
              ;; disk, in case it had been visited before
              (revert-buffer 'ignore-auto 'noconfirm)
              (goto-char (point-min))
              (while (re-search-forward "^.+$" nil 'noerror)
                (replace-match (file-relative-name (match-string 0))))
              (save-buffer))))))))

(defun db/update-playlist-files ()
  "Update personal playlist files."
  (interactive)
  (message "Update favorites playlist")
  (db/write-m3u-playlist-from-git-annex-find
   "~/Documents/media/audio/others/daniels-favorite.m3u"
   ;; We explicitly make sure the values for the `rating-daniel' metadata entry
   ;; are numbers before comparing them to 0.9
   "../songs/ -( --metadata=rating-daniel=1.0 --or --metadata=rating-daniel=0.[0-9]* -) --and --metadata rating-daniel>=0.9 --and --not --metadata tag=exclude-favorite-playlist"
   "~/Documents/media/audio/others/"
   'overwrite)
  (message "Update work playlist")
  (db/write-m3u-playlist-from-git-annex-find
   "~/Documents/media/audio/others/daniels-work-list.m3u"
   "../songs/ --metadata db-work=include"
   "~/Documents/media/audio/others/"
   'overwrite))



(provide 'db-music)

;;; db-music.el ends here
