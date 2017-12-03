(deftheme db-dark
    "Created 2017-12-02.")

(load-theme 'solarized-dark)

(custom-theme-set-faces
 'db-dark
 '(aw-leading-char-face ((t (:foreground "red" :height 1.0))))
 '(cursor ((t (:background "red" :foreground "#002b36" :inverse-video t :weight ultra-bold))))
 '(emms-browser-artist-face ((t (:foreground "RoyalBlue1"))))
 '(emms-browser-composer-face ((t (:foreground "RoyalBlue1"))))
 '(emms-browser-performer-face ((t (:foreground "#aaaaff"))))
 '(emms-browser-track-face ((t (:foreground "DodgerBlue3" :height 1.0))))
 '(emms-playlist-selected-face ((t (:foreground "medium sea green"))))
 '(org-agenda-date ((t (:background "#002b36" :foreground "#586e75" :inverse-video nil :box (:line-width 2 :color "#002b36") :overline nil :slant normal :weight bold :height 1.2))))
 '(org-agenda-dimmed-todo-face ((t nil)))
 '(org-agenda-structure ((t (:foreground "#93a1a1" :inverse-video nil :box (:line-width 2 :color "#002b36") :underline nil :slant normal :weight bold :height 1.1))))
 '(org-clock-overlay ((t (:background "black"))))
 '(org-column ((t (:strike-through nil :underline nil :slant normal :weight normal :height 110 :family "DejaVu Sans Mono"))))
 '(org-headline-done ((t (:foreground "#859900"))))
 '(org-level-1 ((t (:inherit default :foreground "#cb4b16" :height 1.0))))
 '(org-level-2 ((t (:inherit default :foreground "#859900" :height 1.0))))
 '(org-level-3 ((t (:inherit default :foreground "#268bd2" :height 1.0))))
 '(org-level-4 ((t (:inherit default :foreground "#b58900" :height 1.0))))
 '(org-level-5 ((t (:inherit default :foreground "#2aa198" :height 1.0))))
 '(org-warning ((t (:foreground "#dc322f" :weight normal))))
 '(outline-4 ((t (:inherit org-level-4))))
 '(flyspell-duplicate ((t (:underline (:color "orange red" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "orange red" :style wave)))))
 '(gnus-group-mail-2 ((t (:inherit outline-4 :weight normal))))
 '(gnus-group-mail-3 ((t (:inherit gnus-group-mail-3-empty :weight normal))))
 '(gnus-group-news-3 ((t (:inherit outline-5 :weight normal))))
 '(gnus-summary-high-ancient ((t (:foreground "#268bd2" :weight normal))))
 '(gnus-summary-high-read ((t (:foreground "#859900" :weight normal))))
 '(gnus-summary-high-unread ((t (:foreground "#839496" :weight normal))))
 '(gnus-summary-low-ancient ((t (:foreground "dim gray"))))
 '(gnus-summary-low-unread ((t (:foreground "dim gray"))))
 '(default ((t (:height 108)))))

(provide-theme 'db-dark)
