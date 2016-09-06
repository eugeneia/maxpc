;;;; System definition for MaxPC.

(asdf:defsystem maxpc
  :description
  "Max’s Parser Combinators: a simple and pragmatic library for writing parsers
  and lexers based on combinatory parsing."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU Affero General Public License"
  :components ((:file "packages")
               (:file "input"
                      :depends-on ("packages"))
               (:file "input/index"
                      :depends-on ("packages" "input"))
               (:file "input/list"
                      :depends-on ("packages" "input" "input/index"))
               (:file "input/vector"
                      :depends-on ("packages" "input" "input/index"))
               (:file "input/stream"
                      :depends-on ("packages" "input" "input/index"))
               (:file "interface"
                      :depends-on ("packages" "input"))
               (:file "primitives"
                      :depends-on ("packages" "input"))
               (:file "more"
                      :depends-on ("packages" "primitives"))
               (:file "char"
                      :depends-on ("packages" "primitives" "more"))
               (:file "digit"
                      :depends-on ("packages" "primitives" "more")))
  :in-order-to ((asdf:test-op (asdf:test-op :maxpc-test))))
