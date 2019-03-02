;;; db-music.el -- Music related stuff -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Custom playlist

(defun db/play-playlist ()
  "Start playing songs from `db/playlist’"
  (interactive)
  (require 'emms)
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


;; End

(provide 'db-music)

;;; db-music ends here
