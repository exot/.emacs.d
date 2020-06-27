;;; db-utils-test.el -- Tests for db-utils -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'db-utils)
(require 'cl-lib)

(ert-deftest db-utils-db/ntp-to-time ()
  "Test NTP conversion to readable time with `db/ntp-to-time'."
  (should (equal (db/ntp-to-time #xdcd2ac0c #x05c6dbac)
                 "2017-05-26T13:28:44.022565583Z"))
  (should (equal (db/ntp-to-time #xbd5927ee #xbc616000)
                 "2000-08-31T18:52:30.735860824Z"))
  (should (equal (db/ntp-to-time #x00000000 #x0000000)
                 "1900-01-01T00:00:00.000000000Z"))
  (should (equal (db/ntp-to-time #xd36968b7 #xe74172bf)
                 "2012-05-25T02:11:03.903342410Z"))
  (should (equal (db/ntp-to-time #xe2a16f23 #x80270e76)
                 "2020-06-27T07:09:23.500595954Z")))

;;; db-utils-test.el ends here
