(ert-deftest elpy-module-company ()
  (elpy-testcase ()
    (elpy-module-company 'global-init)
    (python-mode)
    (elpy-mode)
    (elpy-module-company 'buffer-init)

    (should company-mode)
    (should (eq company-idle-delay t))
    (should (eq company-tooltip-align-annotations t))
    (should (member 'elpy-company-backend company-backends))

    (elpy-module-company 'buffer-stop)

    (should-not company-mode)))
