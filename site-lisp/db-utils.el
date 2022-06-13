;;; db-utils.el --- Utility Functions for Daniel's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some functions used in my ~/.emacs.d/init.el.  Most of them are copied from
;; various sources around the internet.
;;

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'dash)
(require 'db-customize)
(require 'bookmark)
(require 'term)
(require 'nsm)
(require 'compile)
(require 'calc)
(require 'calc-forms)
(require 'ert)
(require 's)
(require 'shr)

(autoload 'async-start "async")
(autoload 'lispy-mode "lispy")
(autoload 'ldap-search "ldap")

(declare-function w32-shell-execute "w32fns.c")


;;; Application Shortcuts

(defun db/run-or-hide-ansi-term ()
  "Find `*ansi-term*' or run `ansi-term' with `explicit-shell-file-name'.
If already in `*ansi-term*' buffer, bury it."
  (interactive)
  (if (string= "term-mode" major-mode)
      (bury-buffer)
    (if (get-buffer "*ansi-term*")
        (switch-to-buffer "*ansi-term*")
      (ansi-term explicit-shell-file-name))))

(defun db/gnus ()
  "Switch to the `*Group*' buffer, starting `gnus' if not existent."
  (interactive)
  (require 'gnus)
  (if (get-buffer "*Group*")
      (switch-to-buffer "*Group*")
    (gnus)))

(defun db/org-agenda ()
  "Show the main `org-agenda'."
  (interactive)
  (org-agenda nil "A"))

(defun db/scratch ()
  "Switch to `*scratch*'."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun db/find-user-init-file ()
  "Edit `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun db/run-or-hide-shell (arg)
  "Opens a shell buffer in new window if not already in one.
  Otherwise, closes the current shell window.  With ARG, switch
  to `default-directory' of the current buffer first."
  ;; idea to split the current window is from
  ;; http://howardism.org/Technical/Emacs/eshell-fun.html
  (interactive "P")
  (cl-flet ((change-to-shell ()
              (if-let ((shell-window (db/find-window-by-buffer-mode 'shell-mode)))
                  (select-window shell-window)
                ;; open shell in buffer with height of ⅓ of current window
                (let ((height (/ (window-total-height) 3)))
                  (shell)
                  (enlarge-window (- height (window-total-height)))))))
    (if (not arg)
        ;; toggle shell window
        (if (not (derived-mode-p 'shell-mode))
            (change-to-shell)
          (bury-buffer)
          (delete-window))

      ;; unconditionally go to shell, and also change to cwd
      (let ((current-dir (expand-file-name default-directory)))
        (change-to-shell)
        (end-of-line)
        (comint-kill-input)
        (insert (format "cd '%s'" current-dir))
        (comint-send-input)))))


;;; General Utilities

(defun db/get-url-from-link ()
  "Copy url of link under point into clipboard."
  (interactive)
  (let ((url (plist-get (text-properties-at (point)) 'help-echo)))
    (if url
        (kill-new url)
      (error "No link found"))))

(defun db/test-emacs ()
  ;; from oremacs
  "Test whether emacs' configuration is not throwing any errors."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs.d/init.el\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

(defun db/isearch-forward-symbol-with-prefix (p)
  ;; http://endlessparentheses.com/quickly-search-for-occurrences-of-the-symbol-at-point.html
  "Like `isearch-forward', unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p
         #'isearch-forward-symbol-at-point
         #'isearch-forward))))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  ;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun db/delete-trailing-whitespace-maybe ()
  "Call `delete-trailing-whitespace', but not in `message-mode'."
  (unless (derived-mode-p 'message-mode)
    (delete-trailing-whitespace)))

(defun db/find-window-by-buffer-mode (mode)
  "Return first window in current frame displaying a buffer with
major mode MODE."
  (cl-find-if (lambda (window)
                (with-current-buffer (window-buffer window)
                  (eq major-mode mode)))
              (window-list-1)))

(defun db/hex-to-ascii (hex-string)
  "Convert HEX-STRING to its ASCII equivalent.
Allowed characters in hex-string are hexadecimal digits and
whitespaces.  If region is active, replace region by the
corresponding ASCII string, otherwise query for input and display
the result in the minibuffer."
  ;; https://stackoverflow.com/questions/12003231/how-do-i-convert-a-string-of-hex-into-ascii-using-elisp
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "String (hex): "))))
  (cl-assert (not (string-match-p "[^A-Fa-e0-9 \t\n]" hex-string))
             "String contains invalid characters.")
  (let ((result (->> hex-string
                     (replace-regexp-in-string "[ \t\n]" "")
                     (string-to-list)
                     (-partition 2)
                     (--map (string-to-number (concat it) 16))
                     concat)))
    (if (use-region-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (dolist (char (string-to-list result))
            (insert-byte char 1)))
      (message result))))

(defun db/text-to-hex (text-string)
  "Convert TEXT-STRING to its hexadecimal representation.
This function will return hexadecimal numbers with more than two
digits if the input string contains wide characters.  The result
might depend on the coding system of the current buffer."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "String (ascii): "))))
  (let ((result (->> text-string
                     (--map (format "%02X " it))
                     (apply #'concat)
                     (string-trim-right))))
    (if (use-region-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (insert result))
      (message result))))

(defun db/ntp-to-time (high low &optional format-string)
  "Format NTP time given by HIGH and LOW to time as given by FORMAT-STRING.
HIGH and LOW must both be 8 digit hex strings.  If not given,
FORMAT-STRING defaults to some ISO 8601-like format."
  (interactive (cl-flet ((read-hex (prompt)
                                   (let ((input-proper (->> prompt
                                                            (read-string)
                                                            (replace-regexp-in-string "[\n\t ]" ""))))
                                     (if (not (string-match-p "[0-9a-fA-F]\\{8\\}" input-proper))
                                         (user-error "Input invalid, must be an 8 digit hex string.")
                                       (string-to-number input-proper 16)))))
                 (list (read-hex "High (hex): ")
                       (read-hex "Low (hex): "))))
  (let* ((calc-internal-prec 30)
         (unix-time (calcFunc-unixtime (calc-eval (format "%s - 2208988800 + (%s/4294967296)" high low)
                                                  'raw)
                                       ;; we explicitly call `calcFunc-unixtime'
                                       ;; here to set the time zone to UTC
                                       0))
         (time-string (format (or format-string
                                  "%04d-%02d-%02dT%02d:%02d:%012.9fZ")
                              (calcFunc-year unix-time)
                              (calcFunc-month unix-time)
                              (calcFunc-day unix-time)
                              (calcFunc-hour unix-time)
                              (calcFunc-minute unix-time)
                              ;; `seconds' will be a floating point number, and we need to format
                              ;; it with a precision that is high enough; apparently, we also need
                              ;; to truncate the number of seconds to nine digits, at least that
                              ;; is what has been done in the test example we use in the
                              ;; corresponding regression test …
                              (string-to-number
                               (calc-eval "trunc(second($), 9)" 'num unix-time)))))
    (if (called-interactively-p 'interactive)
        (message time-string)
      time-string)))

(defun conditionally-enable-lispy ()
  "Enable lispy-mode when in `eval-expression’ or in
`pp-eval-expression’.  lispy must have been loaded for this
first, i.e., this function will not automatically load
lispy."
  (when (and (featurep 'lispy)
             (or (eq this-command 'eval-expression)
                 (eq this-command 'pp-eval-expression)))
    (lispy-mode 1)))

(defun turn-on-lispy-when-available ()
  "Activate `lispy’ in current buffer when possible.
Will print a warning in case of failure."
  (interactive)
  (with-demoted-errors "Cannot activate lispy: %s"
    (require 'lispy)
    (lispy-mode)))

(defun turn-on-flycheck-when-file ()
  "Turn on `flycheck-mode' when buffer is associated with a file."
  (when buffer-file-name
    (flycheck-mode +1)))

(defun db/sort-nsm-permanent-settings ()
  "Sort values in `nsm-permanent-host-settings’."
  (setq nsm-permanent-host-settings
        (cl-sort nsm-permanent-host-settings
                 #'string<
                 :key #'cl-second)))

(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun db/turn-off-local-electric-pair-mode ()
  "Locally turn off electric pair mode."
  (interactive)
  (electric-pair-local-mode -1))

(defun db/pretty-print-xml ()
  "Stupid function to pretty print XML content in current buffer."
  ;; We assume that < and > only occur as XML tag delimiters, not in strings;
  ;; this function is not Unicode-safe
  (interactive)

  (unless (eq major-mode 'nxml-mode)
    (require 'nxml-mode)
    (nxml-mode))

  (save-mark-and-excursion

   ;; First make it all into one line
   (goto-char (point-min))
   (while (re-search-forward "\n[\t ]*" nil 'no-error)
     ;; In case there was a space, we have to keep at least one as a separator
     (if (save-match-data (looking-back "[\t ]" 1))
         (replace-match " ")
       (replace-match "")))

   ;; Next break between tags
   (goto-char (point-min))
   (while (re-search-forward ">[\t ]*<" nil 'no-error)
     (replace-match ">\n<"))

   ;; Move opening and closing tags to same line in case there’s nothing in
   ;; between
   (goto-char (point-min))
   (while (re-search-forward "<\\([^>]*\\)>\n</\\1>" nil 'no-error)
     (replace-match "<\\1></\\1>"))

   ;; Indent
   (indent-region (point-min) (point-max))))

(defun db/lookup-smime-key (mail)
  "Look up `MAIL' on ldap-server of the DFN.

If found, imports the certificate via gpgsm."
  ;; inspired by https://www.emacswiki.org/emacs/ExtendSMIME
  (interactive "sMail: ")
  (require 'ldap)
  (when (get-buffer " *ldap-value*")
    (kill-buffer " *ldap-value*"))
  (ldap-search (format "(mail=%s)" mail))
  (let ((bufval (get-buffer " *ldap-value*")))
    (when bufval
      (with-current-buffer bufval
        (save-restriction
          (widen)                       ; just to be sure
          (let ((result (call-process-region (point-min) (point-max)
                                             "gpgsm"
                                             nil nil nil
                                             "--import")))
            (if (zerop result)
                (message "Successfully imported certificate for <%s>" mail)
              (error "Could not import certificate for <%s>" mail))))))))

;; https://emacs.stackexchange.com/questions/3089/how-can-i-create-a-dired-buffer-listing-all-open-files
;; https://emacs.stackexchange.com/questions/2567/programmatically-insert-files-into-dired-buffer
(defun db/dired-from-shell-command (command &optional directory)
  "Run COMMAND in DIRECTORY and display resulting list of files via `dired’.

COMMAND must be a shell command that produces a list of files as
output, separated by \\n, when called with
`shell-command-to-string’.  DIRECTORY defaults to
`default-directory’."
  (interactive "sCommand: ")
  (when (and directory (not (directory-name-p directory)))
    (user-error "Value for DIRECTORY is not a directory name: %s"
                directory))
  (let* ((default-directory (or directory default-directory))
         (list-of-files (cl-remove-if-not
                         (lambda (entry)
                           (and (not (string-empty-p entry))
                                (or (file-exists-p entry)
                                    (file-symlink-p entry))))
                         (split-string (shell-command-to-string command)
                                       "\n"))))
    (if (null list-of-files)
        (message "No files return by command “%s”" command)
      (dired (cons "Command output" list-of-files)))))

(defun db/dired-from-git-annex (matching-options)
  "Display files found by git annex with MATCHING-OPTIONS.
This runs “git annex find” with MATCHING-OPTIONS (a string) in
`default-directory' and displays the resulting set of files using
`dired'."
  (interactive (list (read-string (format "Matching Options (in %s): "
                                          default-directory))))
  (db/dired-from-shell-command (format "git annex find . %s" matching-options)
                               default-directory))

(defun db/system-open (path)
  "Open PATH with default program as defined by the underlying system."
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" path))
   ((eq system-type 'cygwin)
    (start-process "" nil "cygstart" path))
   (t
    (start-process "" nil "xdg-open" path))))

(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added.  When the minibuffer is active, quit it
regardless of the currently selected window."
  ;; https://with-emacs.com/posts/tips/quit-current-context/
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))

(defun db/convert-lf-to-crlf-in-buffer (&rest _stuff)
  "Convert all LF to CRLF in current buffer.
Does not replace CRLF with CRCRLF, and so on."
  (save-mark-and-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "\n" nil 'noerror)
        (unless (looking-back "\r\n" 2)
          (replace-match "\r\n"))))))

(defun db/convert-crlf-to-lf-in-buffer (&rest _stuff)
  "Convert all CRLF to LF in current buffer."
  (save-mark-and-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "\r\n" nil 'noerror)
        (replace-match "\n")))))

(defun db/sync-magit-repos-from-projectile ()
  "Update repositories known to magit from projectile's."
  (interactive)
  (eval-when-compile                    ; to silence the byte compiler
    (require 'projectile)
    (require 'magit))
  (setq magit-repository-directories
        (mapcar
         (lambda (dir)
           (cons (substring dir 0 -1) 0))
         (cl-remove-if-not
          (lambda (project)
            (unless (file-remote-p project)
              (file-exists-p (concat project "/.git"))))
          projectile-known-projects))))

(defun db/shr-render-file (file)
  "Display the HTML rending of the contents of FILE."
  (interactive "f")
  (unless (file-readable-p file)
    (user-error "Cannot read file: %s" file))
  (shr-render-buffer (find-file-noselect file))
  (delete-trailing-whitespace))


;;; Base45 Decoding

;; This is based on https://datatracker.ietf.org/doc/draft-faltstrom-base45/,
;; which in turned may be used in data encoded for QR codes.

(let ((decode-hash-table (make-hash-table))
      (base45-alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:"))

  (-each-indexed (string-to-list base45-alphabet)
    (-lambda (index char)
      (puthash char index decode-hash-table)
      ;; Add an encode-hash-table here in case base45-encode-string will ever be
      ;; written, like so: (puthash index char encode-hash-table)
      ))

  (defun db/base45-decode-string (str)
    "Decode base45 string STR and return the result as a string."

    (when (= 1 (% (length str) 3))
      (user-error "Input string has invalid length for base45 decoding; must be 0 or 2 modulo 3"))

    (let* ((list-of-numbers (->> str

                                 s-upcase ; also allow lower-case input characters

                                 ;; convert all characters to their code values
                                 (-map (lambda (char)
                                         (or (gethash char decode-hash-table)
                                             (user-error "Invalid character in string for base45 decoding: %c" char))))

                                 (-partition-all 3)

                                 ;; Interpret tuples as base45 numbers and
                                 ;; compute their decimal values
                                 (-map (lambda (block)
                                         (+ (* 45 45 (or (nth 2 block) 0))
                                            (* 45 (nth 1 block))
                                            (nth 0 block))))))

           (list-of-bytes (nconc (-mapcat (lambda (num)
                                            (list (/ num 256)
                                                  (% num 256)))
                                          (-butlast list-of-numbers))
                                 (let ((last-one (-last-item list-of-numbers)))
                                   (if (< last-one 256)
                                       ;; When the last element represents only
                                       ;; one byte, discard the extra 0 that (/
                                       ;; last-one 256) would produce …
                                       (list last-one)
                                     ;; … else handle the last element like all
                                     ;; the others
                                     (list (/ last-one 256)
                                           (% last-one 256)))))))

      (apply #'string list-of-bytes))))

(ert-deftest db/base45-decode-string--basic-tests ()
  "Test basic decoding examples"
  ;; dash is funny :)
  (-each `(("QED8WEX0" "ietf!")
           ("X.CT3EGEC" "foobar")
           ("x.ct3egec" "foobar")
           ("/Y81EC.OE+EDR342%EX0" "Emacs is Fun!")
           ("1A6VF61:64R6F4F%EDM-C6H6 8DKQEWF6V4761" "19287349wjiqf72yhasd29823")
           ;; Bytes in the returned strings are actually numbers between 0 and
           ;; 255; no character conversion (or something like that) is conducted
           ;; here.
           ("6BFOXN" ,(string 120 156 187 212))
           ;; Test cases from https://github.com/Netnod/base45.
           ("%69 VD92EX0" "Hello!!")
           ("VV4:97Y+AHA7MY831" "%69 VD92EX0"))
    (-lambda ((in out))
      (should (equal out (db/base45-decode-string in))))))

(defun db/base45-decode-region (beg end)
  "Base45-decode region between BEG and END.

Replaces the region by the result of the decoding."
  (interactive "r")
  (let ((replace-string (db/base45-decode-string (buffer-substring-no-properties beg end))))
    (kill-region beg end)
    ;; Using `insert' and `insert-char' directly uses character conversion and
    ;; may scramble bytes with the eight bit set; let's try `insert-byte'
    ;; instead.
    (dolist (char (string-to-list replace-string))
      (insert-byte char 1))))

(ert-deftest db/base45-decode-region--insert-correct-bytes ()
  "Test whether bytes are always inserted.
Take the start of a compressed EU Digital Covid Certificate and
insert into a temporary buffer; check that indeed the expected
number of bytes has been inserted."
  (let ((encoded-string "6BFOXN"))
    (with-temp-buffer
      (insert encoded-string)
      (db/base45-decode-region (point-min) (point-max))
      (message "%s" (string-to-list (buffer-string)))
      ;; (120 4194204 4194235 4194260) is Emacs' internal representation of
      ;; x\234\273\324, where the last three bytes are raw-byte; when
      ;; non-raw-bytes would have been inserted, it would be (120 156 187 212).
      (should (equal '(120 4194204 4194235 4194260)
                     (string-to-list (buffer-string)))))))


;;; Extend Input Methods

(defun db/add-symbols-to-TeX-input-method ()
  "Add some new symbols to TeX input method."
  (when (string= current-input-method "TeX")
    (let ((quail-current-package (assoc "TeX" quail-package-alist)))
      (quail-define-rules
       ((append . t))
       ("\\land" ?∧)
       ("\\lor" ?∨)
       ("\\lnot" ?¬)
       ("\\implies" ?⇒)
       ("\\powerset" ?𝔓)
       ("\\mathbbK" ?𝕂)
       ("\\mathbbR" ?ℝ)
       ("\\mathbbN" ?ℕ)
       ("\\mathbbZ" ?ℤ)
       ("\\mathbbP" ?ℙ)
       ("\\mathcalA" ?𝒜)
       ("\\mathcalB" ?ℬ)
       ("\\mathcalC" ?𝒞)
       ("\\mathcalD" ?𝒟)
       ("\\mathcalE" ?ℰ)
       ("\\mathcalH" ?ℋ)
       ("\\mathcalI" ?ℐ)
       ("\\mathcalJ" ?𝒥)
       ("\\mathcalK" ?𝒦)
       ("\\mathcalL" ?ℒ)
       ("\\mathcalM" ?ℳ)
       ("\\mathcalR" ?ℛ)
       ("\\mathcalQ" ?𝒬)
       ("\\mathcalS" ?𝒮)
       ("\\mathfrakP" ?𝔓)))))


;;; Wrappers for external applications

(defun db/two-monitors-xrandr ()
  "Activate second monitor using xrandr."
  (call-process "xrandr" nil nil nil
                "--output" "HDMI-3" "--primary" "--right-of" "LVDS-1" "--auto"))

(defun db/one-monitor-xrandr ()
  "Deactivate all additional monitors."
  (call-process "xrandr" nil nil nil
                "--output" "HDMI-3" "--off"))


;;; Bookmarks

(defun db/bookmark-add-with-handler (name location handler)
  "Add NAME as bookmark to LOCATION and use HANDLER to open it.
HANDLER is a function receiving a single argument, namely
LOCATION.  If a bookmark named NAME is already present, replace
it.  The bookmarks will finally be sorted by their name."
  (setq bookmark-alist
        (cl-delete-if #'(lambda (bmk) (equal (car bmk) name))
                      bookmark-alist))
  (push `(,name
          (filename . ,location)
          (handler . ,#'(lambda (arg)
                          (funcall handler (cdr (assoc 'filename arg))))))
        bookmark-alist)
  (setq bookmark-alist (cl-sort bookmark-alist #'string-lessp :key #'car)))

(defun db/bookmark-add-external (location name)
  "Add NAME as bookmark to LOCATION that is opened by the operating system.
Offers simple completing from the list of recently opened files.
In dired, offer all marked files or the currently selected file
as completing instead."
  (interactive (list (completing-read "Location: " (if (derived-mode-p 'dired-mode)
                                                       (dired-get-marked-files)
                                                     recentf-list))
                     (read-string "Name: ")))
  (db/bookmark-add-with-handler name location #'db/system-open))

(defun db/bookmark-add-url (url name)
  "Add NAME as bookmark to URL that is opened by `browse-url’."
  (interactive "sURL: \nsName: ")
  (db/bookmark-add-with-handler name url #'browse-url))

(defun db/bookmark-add-eww (url name)
  "Add NAME as bookmark to URL to be opened with `eww’."
  (interactive "sURL: \nsName: ")
  (db/bookmark-add-with-handler name url #'eww))


;;; Appearance

(defun db/switch-to-dark-theme ()
  "Switch to dark theme.
This is `db-dark' and `solarized-dark'."
  (interactive)
  (load-theme 'solarized-dark)
  (load-theme 'db-dark))

(defun db/switch-to-light-theme ()
  "Switch to dark theme.
This is `db-light' and `solarized-light'."
  (interactive)
  (load-theme 'solarized-light)
  (load-theme 'db-light))


;;; End

(provide 'db-utils)

;;; db-utils.el ends here
