(defpackage maxpc
  (:documentation
   "Max’s Parser Combinators. MaxPC is a complete rewrite of MPC with was in
turn a fork of Drew Crampsie’s “Smug”.")
  (:use :cl)
  (:export :parse
           :get-input-position
           :%handler-case
           :%restart-case
           :?end
           :=element
           :?fail
           :?satisfies
           :?test
           :?eq
           :?not
           :=subseq
           :=list
           :?list
           :%any
           :%some
           :%or
           :%and
           :%diff
           :%maybe
           :=transform
           :=destructure))

(defpackage maxpc.char
  (:documentation
   "Parsers for character inputs.")
  (:use :cl :maxpc)
  (:export :?char
           :?string
           :*whitespace*
           :?whitespace
           :%skip-whitespace
           :?newline
           :=line))

(defpackage maxpc.digit
  (:documentation
   "Parsers for digit numerals in character inputs.")
  (:use :cl :maxpc)
  (:export :?digit
           :=natural-number
           :=integer-number))
