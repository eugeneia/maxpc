;;;; Various microbenchmarks.

(defpackage maxpc.bench
  (:use :cl :maxpc)
  (:export :bench-=destructure))

(in-package :maxpc.bench)

(defun bench-=destructure (iterations)
  (let ((input (make-array (truncate (* 2 iterations))
                           :element-type 'base-char
                           :initial-element #\x))
        (parser (%any (?list (=destructure (_ _)
                                 (=list (=element) (=element)))))))
    (time (parse input parser))))
