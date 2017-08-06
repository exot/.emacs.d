;;; db-emms.el -- EMMS configuration

;;; Commentary:

;;; Code:


;; Setup

(use-package emms-setup
  :ensure emms)

(emms-all)
(emms-default-players)


;; Basic configuration

(setq emms-source-file-default-directory "~/Documents/media/audio/")

(defadvice emms-tag-editor-submit (after delete-window activate)
  (delete-window))

(bind-key "S s" #'emms-shuffle emms-playlist-mode-map)

(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")

(emms-mode-line -1)
(emms-playing-time-enable-display)
(setq emms-player-list
      '(emms-player-mplayer emms-player-mplayer-playlist))


;; Custom playlist

(defun db/play-playlist ()
  (interactive)
  (save-window-excursion
    (let ((music-buffer-name "*EMMS Playlist* -- Misc"))
      (unless (get-buffer music-buffer-name)
        (emms-playlist-new music-buffer-name))
      (with-current-buffer (get-buffer music-buffer-name)
        (emms-stop)
        (emms-playlist-set-playlist-buffer)
        (emms-playlist-current-clear)
        (emms-playlist-current-insert-source
         'emms-insert-playlist
         (expand-file-name "private/playlist-daniel.pls"
                           emacs-d))
        (beginning-of-buffer)
        (emms-shuffle)
        ;; (emms-playlist-sort-by-play-count)
        (emms-playlist-select-first)
        (emms-start)))))


;; Custom file finder

(defun db/emms-source-file-directory-tree-find (dir regex)
  "Return a list of all files under DIR that match REGEX.
This function uses the external find utility. The name for GNU find
may be supplied using `emms-source-file-gnu-find'.

Difference to original `emms-source-file-directory-tree-find’ is
that we also follow symbolic links."
  (with-temp-buffer
    (call-process emms-source-file-gnu-find
                  nil t nil
                  "-L"
                  (expand-file-name dir)
                  "-type" "f"
                  "-iregex" (concat ".*\\(" regex "\\).*"))
    (delete ""
            (split-string (buffer-substring (point-min)
                                            (point-max))
                          "\n"))))

(unless (memq system-type '(windows-nt ms-dos))
  (setq emms-source-file-directory-tree-function
        #'db/emms-source-file-directory-tree-find))


;; Track description

(defun db/emms-track-description (track)
  (require 'seq)
  (let* ((artist    (propertize (emms-track-get track 'info-artist "")
                                'face 'emms-browser-artist-face))
         (composer  (propertize (emms-track-get track 'info-composer "")
                                'face 'emms-browser-composer-face))
         (performer (propertize (emms-track-get track 'info-performer "")
                                'face 'emms-browser-performer-face))
         (title     (propertize (emms-track-get track 'info-title "")
                                'face 'emms-browser-track-face))
         (note      (emms-track-get track 'info-note "")))
    (let ((main-description (if (not (seq-empty-p title))
                                (cond
                                  ((and (not (seq-empty-p composer))
                                        (not (seq-empty-p performer)))
                                   (if (string= composer performer)
                                       (format "“%s” by %s"
                                               title composer)
                                     (format "“%s” by %s, performed by %s"
                                             title
                                             composer
                                             performer)))
                                  ((not (seq-empty-p artist))
                                   (format "“%s” by %s" title artist))
                                  (t title))
                              (emms-track-simple-description track)))
          (note             (if (seq-empty-p note)
                                ""
                              (concat " [" note "]"))))
      (concat main-description note))))

(setq emms-track-description-function
      'db/emms-track-description)

;; don't set face in playlist to emms-playlist-track-face
(defun db/emms-playlist-mode-insert-track (track &optional no-newline)
  "Insert the description of TRACK at point.
When NO-NEWLINE is non-nil, do not insert a newline after the track."
  (emms-playlist-ensure-playlist-buffer)
  (emms-with-inhibit-read-only-t
   (insert (emms-propertize (emms-track-force-description track)
                            'emms-track track
                                        ;'face 'emms-playlist-track-face
                            ))
   (when (emms-playlist-selected-track-at-p)
     (emms-playlist-mode-overlay-selected))
   (unless no-newline
     (insert "\n"))))

(add-hook 'emms-playlist-mode-hook
          (lambda ()
            (setq emms-playlist-insert-track-function
                  #'db/emms-playlist-mode-insert-track)))


;; Streams

(require 'emms-stream-info)
(setq emms-stream-default-action "play")

(defun db/emms-track-status ()
  (if emms-player-playing-p
      (format "%s" (emms-track-description
                    (emms-playlist-current-selected-track)))
    "«nothing»"))


;; Hydra

(defhydra emms-control (:color red :hint none)
  "
Playing: %s(db/emms-track-status)

  _n_: ?n?          _p_: ?p?
_RET_: ?RET?    _M_: ?M?
  _-_: lower volume  _+_: ?+?

"
  ("n" emms-next         "next")
  ("p" emms-previous     "previous")
  ("RET" emms-pause      "play/pause")
  ("s" emms-show         "show title")
  ("-" emms-volume-lower "lower volume")
  ("+" emms-volume-raise "raise volume")
  ("M" emms              "show playlist"))


;; End

(provide 'db-emms)
;;; db-emms ends here
