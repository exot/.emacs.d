;;; timeline-tools-test.el -- Tests for timeline-tools -*- lexical-binding: t -*-

;;; Commentary:

;; Most of the times in the test cases are not really reasonable, but if we can
;; cope with those, reasonable one should also work ;)

;;; Code:

(require 'ert)
(require 'timeline-tools)
(require 'cl-lib)

;; Basic parsing tests

(ert-deftest timeline-tools-test-parse-clocklines-1 ()
  "Test `timeline-tools-clocklines-in-range’ with simple setup."
  (let ((result (with-temp-buffer
                  (insert "* Task 1\n")
                  (insert ":LOGBOOK:\n")
                  (insert "CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45\n")
                  (insert ":END:\n")
                  (org-mode)
                  (timeline-tools-clocklines-in-range 1515279600.0 1515366000.0))))
    (should (equal 1 (length result)))
    (should (equal 2 (length (car result))))
    (should (markerp (car (car result))))
    (should (equal (car (cdr (car result)))
                   (cons 1515327300.0 1515330000.0)))))

(ert-deftest timeline-tools-test-parse-clocklines-2 ()
  "Test `timeline-tools-clocklines-in-range’ with multiple clocklines."
  (let ((result (with-temp-buffer
                  (insert "* Task 1\n")
                  (insert ":LOGBOOK:\n")
                  (insert "CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45\n")
                  (insert "CLOCK: [2018-01-08 Mon 16:15]--[2018-01-10 Wed 13:10] => 44:55\n")
                  (insert "CLOCK: [2018-01-10 Wed 10:07]--[2018-01-12 Fri 14:00] => 51:53\n")
                  (insert ":END:\n")
                  (org-mode)
                  (timeline-tools-clocklines-in-range 1515279600.0 1515600000.0))))
    (should (equal 1 (length result)))
    (should (equal 4 (length (car result))))
    (should (markerp (car (car result))))
    (should (equal (cdr (car result)) 
                   (list (cons 1515327300.0 1515330000.0)
                         (cons 1515424500.0 1515586200.0)
                         (cons 1515575220.0 1515600000.0))))))

(ert-deftest timeline-tools-test-parse-clocklines-3 ()
  "Test `timeline-tools-clocklines-in-range’ with multiple tasks."
  (let ((result (with-temp-buffer
                  (insert "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45
CLOCK: [2018-01-08 Mon 16:15]--[2018-01-10 Wed 13:10] => 44:55
CLOCK: [2018-01-10 Wed 10:07]--[2018-01-12 Fri 14:00] => 51:53
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-07 Sun 15:13]--[2018-01-07 Sun 16:17] =>  1:04
CLOCK: [2018-01-08 Mon 16:00]--[2018-01-08 Mon 16:15] =>  0:15
:END:
")
                  (org-mode)
                  (timeline-tools-clocklines-in-range 1515279600.0 1515600000.0))))
    (should (equal 2 (length result)))
    (should (equal '(4 3) (mapcar #'length result)))
    (should (cl-every #'markerp (mapcar #'car result)))
    (should (equal (mapcar #'cdr result)
                   '(((1515327300.0 . 1515330000.0)
                      (1515424500.0 . 1515586200.0)
                      (1515575220.0 . 1515600000.0))
                     ((1515334380.0 . 1515338220.0)
                      (1515423600.0 . 1515424500.0)))))))

(ert-deftest timeline-tools-test-parse-clocklines-4 ()
  "Test `timeline-tools-clocklines-in-range’ with extended time range."
  (let ((result (with-temp-buffer
                  (insert "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45
CLOCK: [2018-01-08 Mon 16:15]--[2018-01-10 Wed 13:10] => 44:55
CLOCK: [2018-01-10 Wed 10:07]--[2018-01-12 Fri 14:00] => 51:53
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-07 Sun 15:13]--[2018-01-07 Sun 16:17] =>  1:04
CLOCK: [2018-01-08 Mon 16:00]--[2018-01-08 Mon 16:15] =>  0:15
:END:
")
                  (org-mode)
                  (timeline-tools-clocklines-in-range 1515279600.0 1515700000.0))))
    (should (equal 2 (length result)))
    (should (equal '(4 3) (mapcar #'length result)))
    (should (cl-every #'markerp (mapcar #'car result)))
    (should (equal (mapcar #'cdr result)
                   '(((1515327300.0 . 1515330000.0)
                      (1515424500.0 . 1515586200.0)
                      (1515575220.0 . 1515700000.0))
                     ((1515334380.0 . 1515338220.0)
                      (1515423600.0 . 1515424500.0)))))))

(ert-deftest timeline-tools-test-parse-clocklines-5 ()
  "Test `timeline-tools-clocklines-in-range’ without org-mode."
  (let ((result (should-error (with-temp-buffer
                                (insert "* Task 1\n")
                                (insert ":LOGBOOK:\n")
                                (insert "CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45\n")
                                (insert ":END:\n")
                                (timeline-tools-clocklines-in-range 1515279600.0 1515366000.0))
                              :type 'user-error)))
    (should (equal (cadr result)
                   "Not in Org mode buffer, cannot parse clocklines"))
    (should (equal (car result)
                   'user-error))))


;; Conflict resolution tests

(ert-deftest timeline-tools-test-clockline-no-conflict-1 ()
  "Test `timeline-tools-clockline-no-conflict’ with complex example."
  (with-temp-buffer
    (insert "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45
CLOCK: [2018-01-08 Mon 16:15]--[2018-01-10 Wed 13:10] => 44:55
CLOCK: [2018-01-10 Wed 10:07]--[2018-01-12 Fri 14:00] => 51:53
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-07 Sun 15:13]--[2018-01-07 Sun 16:17] =>  1:04
CLOCK: [2018-01-08 Mon 16:00]--[2018-01-08 Mon 16:15] =>  0:15
:END:
")
    (org-mode)
    (should (equal (timeline-tools-clockline-no-conflict
                    (org-time-string-to-seconds "[2018-01-07 Sun 13:00]")
                    (org-time-string-to-seconds "[2018-01-11 Thu 13:33]")
                    (current-buffer))
                   "CLOCK: [2018-01-07 Sun 13:00]--[2018-01-11 Thu 13:33] => 96:33"))
    (should (equal (buffer-string)
                   "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-11 Thu 13:33]--[2018-01-12 Fri 14:00] => 24:27
:END:

* Task 2
:LOGBOOK:
:END:
"))))

(ert-deftest timeline-tools-test-add-clockline-to-marker-1 ()
  "Test `timeline-tools-add-clockline-to-marker’ without running
clock."
  (with-temp-buffer
    (insert "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45
CLOCK: [2018-01-08 Mon 16:15]--[2018-01-10 Wed 13:10] => 44:55
CLOCK: [2018-01-10 Wed 10:07]--[2018-01-12 Fri 14:00] => 51:53
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-07 Sun 15:13]--[2018-01-07 Sun 16:17] =>  1:04
CLOCK: [2018-01-08 Mon 16:00]--[2018-01-08 Mon 16:15] =>  0:15
:END:
")
    (org-mode)
    (goto-char 216)
    (let ((result (timeline-tools-add-clockline-to-marker
                   (point-marker)
                   (org-time-string-to-seconds "[2018-01-07 Sun 13:00]")
                   (org-time-string-to-seconds "[2018-01-11 Thu 13:33]"))))

      (should (null result))
      (should (equal (buffer-string)
                     "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-11 Thu 13:33]--[2018-01-12 Fri 14:00] => 24:27
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-07 Sun 13:00]--[2018-01-11 Thu 13:33] => 96:33
:END:
")))))

(ert-deftest timeline-tools-test-add-clockline-to-marker-2 ()
  "Test `timeline-tools-add-clockline-to-marker’ with running
clock at same task."
  (with-temp-buffer
    (insert "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-10 Wed 10:07]--[2018-01-12 Fri 14:00] => 51:53
CLOCK: [2018-01-08 Mon 16:15]--[2018-01-10 Wed 13:10] => 44:55
CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-10 Wed 13:10]
CLOCK: [2018-01-08 Mon 16:00]--[2018-01-08 Mon 16:15] =>  0:15
CLOCK: [2018-01-07 Sun 15:13]--[2018-01-07 Sun 16:17] =>  1:04
:END:
")
    (org-mode)
    (let (;; simulate running clock at Task 2
          (org-clock-hd-marker (progn (goto-char 216) (point-marker)))
          (org-clock-marker (progn (goto-char 264) (point-marker)))
          (org-clock-start-time (org-time-string-to-time "[2018-01-10 Wed 13:10]")))
      (let ((result (timeline-tools-add-clockline-to-marker
                     org-clock-hd-marker
                     (org-time-string-to-seconds "[2018-01-07 Sun 13:00]")
                     (org-time-string-to-seconds "[2018-01-11 Thu 13:33]"))))

        (should (null result))
        (should (equal org-clock-start-time
                       (append (org-time-string-to-time "[2018-01-11 Thu 13:33]")
                               '(0 0))))
        (should (equal (buffer-string)
                       "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-11 Thu 13:33]--[2018-01-12 Fri 14:00] => 24:27
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-11 Thu 13:33]
CLOCK: [2018-01-07 Sun 13:00]--[2018-01-11 Thu 13:33] => 96:33
:END:
"))))))

(ert-deftest timeline-tools-test-add-clockline-to-marker-3 ()
  "Test `timeline-tools-add-clockline-to-marker’ with running
clock at same task."
  (with-temp-buffer
    (insert "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-08 Mon 16:15]--[2018-01-10 Wed 13:10] => 44:55
CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-10 Wed 13:10]
CLOCK: [2018-01-08 Mon 16:00]--[2018-01-08 Mon 16:15] =>  0:15
CLOCK: [2018-01-07 Sun 15:13]--[2018-01-07 Sun 16:17] =>  1:04
:END:
")
    (org-mode)
    (let (;; simulate running clock at Task 2
          (org-clock-hd-marker (progn (goto-char 153) (point-marker)))
          (org-clock-marker (progn (goto-char 201) (point-marker)))
          (org-clock-start-time (org-time-string-to-time "[2018-01-10 Wed 13:10]")))
      (let ((result (timeline-tools-add-clockline-to-marker
                     (progn (goto-char 2) (point-marker))
                     (org-time-string-to-seconds "[2018-01-07 Sun 13:00]")
                     (org-time-string-to-seconds "[2018-01-11 Thu 13:33]"))))

        (should (null result))
        (should (equal org-clock-start-time
                       (append (org-time-string-to-time "[2018-01-11 Thu 13:33]")
                               '(0 0))))
        (should (equal (buffer-string)
                       "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-07 Sun 13:00]--[2018-01-11 Thu 13:33] => 96:33
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-11 Thu 13:33]
:END:
"))))))

(ert-deftest timeline-tools-test-add-clockline-to-marker-4 ()
  "Test `timeline-tools-add-clockline-to-marker’ with running
clock at other task, and where afterwards only the running clock
line is left."
  (with-temp-buffer
    (insert "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-10 Wed 10:07]--[2018-01-12 Fri 14:00] => 51:53
CLOCK: [2018-01-08 Mon 16:15]--[2018-01-10 Wed 13:10] => 44:55
CLOCK: [2018-01-07 Sun 13:15]--[2018-01-07 Sun 14:00] => 0:45
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-10 Wed 13:10]
CLOCK: [2018-01-08 Mon 16:00]--[2018-01-08 Mon 16:15] =>  0:15
CLOCK: [2018-01-07 Sun 15:13]--[2018-01-07 Sun 16:17] =>  1:04
:END:
")
    (org-mode)
    (let (;; simulate running clock at Task 2
          (org-clock-hd-marker (progn (goto-char 216) (point-marker)))
          (org-clock-marker (progn (goto-char 264) (point-marker)))
          (org-clock-start-time (org-time-string-to-time "[2018-01-10 Wed 13:10]")))
      (let ((result (timeline-tools-add-clockline-to-marker
                     (progn (goto-char 2) (point-marker))
                     (org-time-string-to-seconds "[2018-01-07 Sun 13:00]")
                     (org-time-string-to-seconds "[2018-01-11 Thu 13:33]"))))

        (should (null result))
        (should (equal org-clock-start-time
                       (append (org-time-string-to-time "[2018-01-11 Thu 13:33]")
                               '(0 0))))
        (should (equal (buffer-string)
                       "
* Task 1
:LOGBOOK:
CLOCK: [2018-01-07 Sun 13:00]--[2018-01-11 Thu 13:33] => 96:33
CLOCK: [2018-01-11 Thu 13:33]--[2018-01-12 Fri 14:00] => 24:27
:END:

* Task 2
:LOGBOOK:
CLOCK: [2018-01-11 Thu 13:33]
:END:
"))))))

;; XXX: timeline-tools-add-clockline-to-marker (including updating current clock)
