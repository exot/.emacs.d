;;; db-emms.el -- EMMS configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'emms)
(require 'emms-source-file)
(require 'emms-playlist-mode)
(require 'emms-volume)


;; Custom file finder

(defun db/emms-source-file-directory-tree-find (dir regex)
  "Return a list of all files under DIR that match REGEX.
This function uses the external find utility.  The name for GNU
find may be supplied using `emms-source-file-gnu-find'.

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


;; Track description

(defun db/emms-track-description (track)
  "Return custom description of TRACK.
This function can be used as a value for `emms-track-description-function’."
  (require 'seq)
  (let* ((artist    (propertize (emms-track-get track 'info-artist "")
                                'face 'emms-browser-artist-face))
         (composer  (propertize (emms-track-get track 'info-composer "")
                                'face 'emms-browser-composer-face))
         (performer (propertize (emms-track-get track 'info-performer "")
                                'face 'emms-browser-performer-face))
         (title     (propertize (emms-track-get track 'info-title "")
                                'face 'emms-browser-track-face))
         (album     (propertize (emms-track-get track 'info-album "")
                                'face 'emms-browser-album-face)))
    (if (not (seq-empty-p title))
        (concat (format "“%s”" title)
                (let* ((first-entry (cond ((not (seq-empty-p composer))
                                           composer)
                                          ((not (seq-empty-p artist))
                                           artist)))
                       (second-entry (when (and (not (seq-empty-p performer))
                                                (not (string= performer first-entry)))
                                       performer)))
                  (when first-entry
                    (if second-entry
                        (format " by %s, performed by %s" first-entry second-entry)
                      (format " by %s" first-entry))))
                (and (not (seq-empty-p album))
                     (format " (%s)" album)))
      (string-remove-prefix (expand-file-name emms-source-file-default-directory)
                            (emms-track-simple-description track)))))

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

(defun db/emms-track-status ()
  "Return string displaying status of currently played track."
  (require 'emms)
  (if emms-player-playing-p
      (format "%s" (emms-track-description
                    (emms-playlist-current-selected-track)))
    "«nothing»"))


;; End

(provide 'db-emms)

;;; db-emms ends here
