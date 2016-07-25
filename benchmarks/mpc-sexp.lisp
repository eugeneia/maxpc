;;;; S-Expression parser from “esrap/example-sexp.lisp” ported to MPC.
(defpackage mpc.example-sexp
  (:use :cl :mpc :mpc.characters :mpc.numerals)
  (:export :=sexp))

(in-package :mpc.example-sexp)

;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

;;; Utility rules.

(defun =alphanumeric ()
  (=satisfies 'alphanumericp))

(defun =string-char ()
  (=or (=satisfies 'not-doublequote)
       (=list (=character #\\) (=character #\"))))

;;; Here we go: an S-expression is either a list or an atom, with possibly
;;; leading whitespace.

(defun =atom ()
  (=or (=str) (=integer-number) (=symbol)))

(defun =str ()
  (=prog2 (=character #\")
          (=string-of (=string-char))
          (=character #\")))

(defun =symbol ()
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before a
  ;; STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (=funcall (=let* ((sym (=string-of (=alphanumeric))))
              (if (not-integer sym)
                  (=result sym)
                  (=fail)))
            'intern))

(defun =sexp ()
  (=or '=slist/parser (=atom)))

(defun =slist ()
  (=prog2 (=character #\()
          (=zero-or-more (=skip-whitespace '=sexp/parser))
          (=zero-or-more (=whitespace))
          (=character #\))))

;; Recursive parsers hack.
(setf (fdefinition '=sexp/parser) (=sexp)
      (fdefinition '=slist/parser) (=slist))

