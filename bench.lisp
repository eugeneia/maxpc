;;;; Various microbenchmarks.

(defpackage maxpc.bench
  (:use :cl :maxpc)
  (:export :bench-=destructure))

(in-package :maxpc.bench)

(defun bench-=destructure (iterations)
  (let ((input (with-output-to-string (out)
                 (loop repeat iterations do (write-string "ab" out))))
        (parser (%any (?list (=destructure (_ _)
                                 (=list (=element) (=element)))))))
    (time (parse input parser))))
