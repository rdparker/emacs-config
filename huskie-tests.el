;;; Tests for huskie

(require 'ert)
(require 'huskie)

(ert-deftest huskie/make-log-process ()
  "Test the filename safety."
  (should (huskie/make-log-process "testlog" "/tmp/filename"))
  (should-error (huskie/make-log-process
                 "testlog" "/tmp/filename ; rm -rf /tmp")))


;;; huskie-tests.el ends here
