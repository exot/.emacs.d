;;; db-music.el -- Music related stuff -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'subr-x)
(require 'seq)
(require 'emms)
(require 'emms-source-file)
(require 'hydra)
(require 'db-emms)

(defgroup db-music nil
  "General configurations for music-related functionality."
  :prefix "db-music"
  :group 'convenience
  :tag "db-music")

(defcustom db/auto-playlist-file-function #'db/play-auto-playlist-from-git-annex-find
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

(defhydra music-control (:color red :hint none)
  "
Playing: %s(db/emms-track-status)

  _n_: ?n?          _p_: ?p?
_RET_: ?RET?    _M_: ?M?
  _-_: lower volume  _+_: ?+?
  _P_: ?P?

"
  ("n" emms-next         "next")
  ("p" emms-previous     "previous")
  ("RET" emms-pause      "play/pause")
  ("s" emms-show         "show title")
  ("-" emms-volume-lower "lower volume")
  ("+" emms-volume-raise "raise volume")
  ("M" emms              "show playlist")
  ("P" (db/play-auto-playlist)
   "Play automatically generated playlist"))

(provide 'db-music)

;;; db-music ends here
