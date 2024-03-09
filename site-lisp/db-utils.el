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
(require 'recentf)

(autoload 'async-start "async")
(autoload 'lispy-mode "lispy")
(autoload 'ldap-search "ldap")
(autoload 'find-libary-name "find-func")
(autoload 'lm-header "lisp-mnt")

(declare-function w32-shell-execute "w32fns.c")
(declare-function org-password-manager-get-password-by-id nil)


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

Otherwise, closes the current shell window.

The buffer's name has to start with “*shell*” to be recognized
by this function.  Otherwise the current buffer is not treated as
a shell buffer.

With ARG, switch to `default-directory' of the current buffer first."
  (interactive "P")
  (cl-flet ((change-to-shell ()
              (if-let ((shell-window (cl-find-if (lambda (window)
                                                   (with-current-buffer (window-buffer window)
                                                     (and (derived-mode-p 'shell-mode)
                                                          (string-match-p "^\\*shell\\*" (buffer-name)))))
                                                 (window-list-1))))
                  (select-window shell-window)
                (--if-let (display-buffer (shell))
                    (select-window it)
                  (error "Could not start shell (`display-buffer' returned nil)")))))
    (if (not arg)
        ;; toggle shell window
        (if (and (derived-mode-p 'shell-mode)
                 (string-match-p "^\\*shell\\*" (buffer-name)))
            (bury-buffer)
          (change-to-shell))

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
  "Test whether Emacs' configuration is not throwing any errors."
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
  "Like the function `isearch-forward', unless prefix argument is provided.
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
  "Return first window in current frame displaying a buffer with major mode MODE."
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
                                 (user-error "Input invalid, must be an 8 digit hex string")
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
     ;; Somehow calling xdg-open via `start-process' does not start any
     ;; application, the subprocess spawned by xdg-open seems to be die
     ;; somewhen; the same behavior can be observed when using
     ;; `async-shell-command' instead of `start-process' with the appropriate
     ;; command.  However, using `shell-command' or `call-process' seem to work,
     ;; but they are blocking Emacs while the programm is running.  Wrapping
     ;; those in an `async-start' does the trick then.
     (async-start
      #'(lambda ()
          (call-process "xdg-open" nil nil nil path))))))

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

(defun db/shr-render-file (file)
  "Display the HTML rending of the contents of FILE."
  (interactive "f")
  (unless (file-readable-p file)
    (user-error "Cannot read file: %s" file))
  (shr-render-buffer (find-file-noselect file))
  (delete-trailing-whitespace))

(defun db/replace-variables-in-string (string var-map)
  "Replace variables in STRING as per VAR-MAP.
VAR-MAP is an alist mapping variable names (strings or symbols)
to values.  Variables are strings of alphabetic characters (no
numbers allowed)."
  (replace-regexp-in-string "[[:alpha:]]+"
                            #'(lambda (var)
                                (format "%s" (alist-get var var-map
                                                        var ; default value
                                                        nil ; not relevant REMOVE parameter
                                                        #'string=)))
                            string))

(defun db/dired-ediff-files ()
  "Compare marked files in Dired with ediff.

From: https://oremacs.com/2017/03/18/dired-ediff/."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    #'(lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
      (error "No more than 2 files should be marked"))))

(defun db/grep-read-files (_ regexp)
  "As for file pattern similar to `grep-read-files' but more direct.

This function is meant as a replacement for `grep-read-files',
replacing it by not calling calling `read-file-name-internal'.

REGEXP is only used for display at the completion prompt, the
same way `grep-read-files' does.

Also add the default as initial input instead of as default
proper.  The latter does not play well with my current completion
framework, as it always tries to match my input with default
entries, even if I want to use the input directly."
  (let* ((bn (funcall grep-read-files-function))
         (fn (and bn
                  (stringp bn)
                  (file-name-nondirectory bn)))
         (default-alias
          (and fn
               (let ((aliases (remove (assoc "all" grep-files-aliases)
                                      grep-files-aliases))
                     alias)
                 (while aliases
                   (setq alias (car aliases)
                         aliases (cdr aliases))
                   (if (string-match (mapconcat
                                      #'wildcard-to-regexp
                                      (split-string (cdr alias) nil t)
                                      "\\|")
                                     fn)
                       (setq aliases nil)
                     (setq alias nil)))
                 (cdr alias))))
         (default-extension
          (and fn
               (let ((ext (file-name-extension fn)))
                 (and ext (concat "*." ext)))))
         (default
          (or default-alias
              default-extension
              (car grep-files-history)
              (car (car grep-files-aliases))))
         (files (completing-read
                 (format "Search for \"%s\" in files matching wildcard (default: %s): "
                         regexp
                         default)
                 (delete-dups
                  (delq nil
                        (append (list default default-alias default-extension)
                                grep-files-history
                                (mapcar #'car grep-files-aliases))))
                 nil nil nil
                 'grep-files-history
                 default)))
    (and files
         (or (cdr (assoc files grep-files-aliases))
             files))))

(defun db/make-selector-from-table-header (header)
  "Return selector function based on names contained in HEADER.

A selector function is a function that receives a KEY (a symbol)
and a ROW (list of values) and returns the value in ROW with the
same index that KEY has in HEADER.  A use-case for such a
selector function is to have a table represented as a list of
lists (rows), where the first list (row) is the header and all
subsequent lists (rows) are the actual values; to access values
in all subsequent rows by name, one can use a selector function
on the header to do so.

HEADER must be a list of strings or symbols and must not contain
duplicates when elements are considered as symbols."

  (unless (listp header)
    (user-error "Header is not a list, cannot create selector"))

  (unless (-all? (-orfn #'stringp #'symbolp) header)
    (user-error "Header must consist of strings or symbols, cannot create selector"))

  (let ((header (-map #'(lambda (elt)
                          (cond
                            ((symbolp elt) elt)
                            ((stringp elt) (intern (downcase elt)))))
                      header)))

    ;; Check for duplicates in HEADER
    (when (-reduce-from #'(lambda (val tail)
                            (or val (memq (cl-first tail)
                                          (cl-rest tail))))
                        nil
                        (-tails header))
      (user-error "Header contains duplicates, cannot create selector"))

    ;; Return actual selector
    (let* ((lookup-table (make-hash-table)))

      (mapc #'(lambda (idx)
                (puthash (nth idx header)
                         idx
                         lookup-table))
            (-iota (length header)))

      #'(lambda (column row)
          (let ((key (if (symbolp column)
                         column
                       (user-error "Unknow key type %s of key %s"
                                   (type-of column)
                                   column))))
            (if-let ((idx (gethash key lookup-table)))
                (nth idx row)
              (user-error "Unknow column name %s" column)))))))

(defun db/get-library-version (library)
  ;; From bbatsov: https://emacs.stackexchange.com/a/69923
  "Return a version string for LIBRARY."
  (with-temp-buffer
    (insert-file-contents (find-library-name library))
    (or (lm-header "package-version")
        (lm-header "version"))))


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
  "Test basic decoding examples."
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
it."
  (let ((new-record `((filename . ,location)
                      (handler . ,handler))))
    (bookmark-update-last-modified new-record)
    (bookmark-store name new-record nil)))

(defun db/bookmark-browse-url (bmk)
  "Extract filename from bookmark BMK and apply `browse-url' to it."
  (browse-url (bookmark-get-filename bmk)))

(defun db/bookmark-system-open (bmk)
  "Extract filename from bookmark BMK and apply `db/system-open' to it."
  (db/system-open (bookmark-get-filename bmk)))

(defun db/bookmark-eww (bmk)
  "Extract filename from bookmark BMK and apply `eww' to it."
  (eww (bookmark-get-filename bmk)))

(defun db/bookmark-add-external (location name)
  "Add NAME as bookmark to LOCATION that is opened by the operating system.
Offers simple completing from the list of recently opened files.
In Dired, offer all marked files or the currently selected file
as completing instead."
  (interactive (list (completing-read "Location: " (if (derived-mode-p 'dired-mode)
                                                       (dired-get-marked-files)
                                                     recentf-list))
                     (read-string "Name: ")))
  (db/bookmark-add-with-handler name location #'db/bookmark-system-open))

(defun db/bookmark-add-url (url name)
  "Add NAME as bookmark to URL that is opened by `browse-url’."
  (interactive "sURL: \nsName: ")
  (db/bookmark-add-with-handler name url #'db/bookmark-browse-url))

(defun db/bookmark-add-eww (url name)
  "Add NAME as bookmark to URL to be opened with `eww’."
  (interactive "sURL: \nsName: ")
  (db/bookmark-add-with-handler name url #'db/bookmark-eww))


;;; Switching Themes

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


;;; SSH-Key-Handling

(defun db/add-ssh-key-with-password (key-file password-fn)
  "Synchronously add key in KEY-FILE to currently running ssh-agent.

PASSWORD-FN is supposed to be a function returning the password
for KEY-FILE; PASSWORD-FN is called on demand.  If KEY-FILE is
not readable, this function errors out.

This function uses ssh-add to add the key to the currently
running ssh-agent and waits for the process to finish."
  (let ((key-file (expand-file-name key-file)))

    (unless (file-readable-p key-file)
      (user-error "SSH key %s does not exist, aborting" key-file))

    (with-environment-variables (("SSH_ASKPASS_REQUIRE" "never"))

      (let* ((ssh-add-handle-output #'(lambda (process output)
                                        (cond
                                          ((string= (format "Enter passphrase for %s: "
                                                            key-file)
                                                    output)
                                           (process-send-string process (funcall password-fn))
                                           (process-send-string process "\n"))
                                          ((or (save-match-data
                                                 (string-match (format "^Identity added: %s" key-file)
                                                               output))
                                               (string= output "\n"))
                                           ;; Ignore harmless output
                                           t)
                                          (t (message "Unknown output received from ssh-agent: %s" output)))))

             (ssh-add-handle-event-change #'(lambda (_ event)
                                              (cond
                                                ((string= event "finished\n")
                                                 (message "Successfully added %s to local SSH agent"
                                                          key-file))
                                                (t (message "Adding SSH key %s failed, ssh-add process reached state %s"
                                                            key-file
                                                            event)))))

             (proc (make-process :name "ssh-add"
                                 :buffer nil
                                 :command (list "ssh-add" key-file)
                                 :filter ssh-add-handle-output
                                 :sentinel ssh-add-handle-event-change)))

        ;; We are waiting for the process to finish, to not let its output
        ;; intermingle with others.  XXX: is there a more standard way to wait for
        ;; a process to finish?
        (while (process-live-p proc)
          (sit-for 0.2))))))

(defun db/ssh-key-hash-from-filename (key-file)
  "Return the SHA256 hash value of the SSH key located in KEY-FILE.

Return nil if KEY-FILE is not readable or does not contain an SSH key."
  (let* ((key-file (expand-file-name key-file)))
    (and (file-exists-p key-file)
         (file-readable-p key-file)
         (->> (shell-command-to-string (format "ssh-keygen -l -E sha256 -f %s" key-file))
              (split-string)
              (cl-second)))))

(defun db/ssh-loaded-key-hashes ()
  "Return list of SHA256 hash values of all keys that are currently loaded."
  (mapcar #'(lambda (line)
              (cl-second (split-string line)))
          (split-string (shell-command-to-string "ssh-add -E sha256 -l") "\n" t)))

(defcustom db/known-ssh-keys nil
  "A alist mapping SSH key-files to their password entries.
This alist maps key-files (file-names) to pass password entries
holding the password to unlock the key."
  :group 'personal-settings
  :type '(alist
          :key-type (file :tag "SSH key file")
          :value-type (choice :tag "Password Storage Type"
                       (list :tag "UNIX pass"
                        (const :pass)
                        (string :tag "pass key"))
                       (list :tag "Org Password Manager"
                        (const :org-password-manager)
                        (string :tag "Entry ID property")))))

;; XXX: could we implement this via `auth-source' and additional backends?

(defun db/load-known-ssh-keys (arg)
  "Add all keys from `db/known-ssh-keys' to currently running ssh-agent.

With non-nil ARG, readd SSH keys irregardless of whether they are
already present in the current agent or not."
  (interactive "P")
  (let ((loaded-ssh-key-hashes (db/ssh-loaded-key-hashes)))
    (pcase-dolist (`(,ssh-key . ,pass-entry) db/known-ssh-keys)
      (let ((ssh-key-hash (db/ssh-key-hash-from-filename ssh-key)))
        (cond
          ((null ssh-key-hash)
           (warn "SSH key file %s is not readable or does not exist, skipping" ssh-key))
          ((and (not arg)
                (cl-member ssh-key-hash loaded-ssh-key-hashes :test #'string=))
           (message "SSH key file %s already loaded, skipping" ssh-key))
          (t
           (db/add-ssh-key-with-password ssh-key
                                         #'(lambda ()
                                             (apply #'db/password-from-storage pass-entry)))))))
    (message "Finished adding known SSH keys to current SSH agent.")))

(cl-defgeneric db/password-from-storage (type entry-key)
  "Retrieve password from storage of type TYPE with lookup key ENTRY-KEY.")

(cl-defmethod db/password-from-storage ((_ (eql :pass)) pass-entry)
  "Retrieve password via the UNIX password manager and PASS-ENTRY key."
  (auth-source-pass-get 'secret pass-entry))

(cl-defmethod db/password-from-storage ((_ (eql :org-password-manager)) org-id)
  "Retrieve password via Org password manager :ID: property ORG-ID."
  (org-password-manager-get-password-by-id org-id))


;;; Dired

(declare-function dired-next-line "dired.el")

(defun db/dired-back-to-top ()
  "Jump to first non-trivial line in Dired."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1))

(defun db/dired-jump-to-bottom ()
  "Jump to last non-trivial line in Dired."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun db/dired-get-size ()    ; from emacswiki, via oremacs
  "Print size of all files marked in the current Dired buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[0-9.,]+[a-za-z]+\\).*total$")
         (match-string 1))))))


;;; End

(provide 'db-utils)

;;; db-utils.el ends here
