;;;; System definition for MaxPC.

(defsystem maxpc
  :description
  "Max’s Parser Combinators: a simple and pragmatic library for writing parsers
  and lexers based on combinatory parsing."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU Affero General Public License"
  :components ((:file "packages")
               (:file "input"
                      :depends-on ("packages"))
               (:file "interface"
                      :depends-on ("packages" "input"))
               (:file "primitives"
                      :depends-on ("packages" "input"))
               (:file "more"
                      :depends-on ("packages" "input" "primitives"))
               (:file "char"
                      :depends-on ("packages" "input" "primitives" "more"))
               (:file "digit"
                      :depends-on ("packages" "input" "primitives" "more"))))
