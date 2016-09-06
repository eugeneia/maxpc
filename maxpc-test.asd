;;;; System definition for MaxPC test and benchmark suite.

(asdf:defsystem maxpc-test
  :description
  "Test and benchmark suite for MaxPC."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU Affero General Public License"
  :components ((:file "test")
               (:file "bench")
               (:file "example-sexp"))
  :depends-on ("maxpc")
  :perform (asdf:test-op (o s) (uiop:symbol-call :maxpc.test :run-tests)))
