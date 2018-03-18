;;;; Primitive backtracking combinators.

(in-package :maxpc)

(defun ?plus (a b)
  (lambda (input &aux (a-more (lambda () (funcall a input))) b-more)
    (labels ((more (&aux suffix rest value present-p)
               (declare (ignorable value present-p))
               (cond (b-more
                      (setf (values rest value present-p b-more)
                            (funcall b-more))
                      (if rest
                          (values rest nil nil #'more)
                          (more)))
                     (a-more
                      (setf (values suffix value present-p a-more)
                            (funcall a-more))
                      (when suffix
                        (setf b-more (lambda () (funcall b suffix)))
                        (more))))))
      (more))))
#|
  (labels ((alternatives (suffix &optional value present-p a-alt)
             (labels ((b-alternatives (rest &optional value present-p b-alt)
                        (if rest
                            (values rest value present-p
                                    (lambda ()
                                      (if b-alt
                                          (multiple-value-call #'b-alternatives
                                            (funcall b-alt))
                                          (when a-alt
                                            (multiple-value-call #'alternatives
                                              (funcall a-alt))))))
                            (when a-alt
                              (multiple-value-call #'alternatives
                                (funcall a-alt))))))
               (when suffix
                 (multiple-value-call #'b-alternatives (funcall b suffix))))))
    (lambda (input)
      (multiple-value-call #'alternatives (funcall a input)))))
|#

(defun ?alternate (x y)
  (lambda (input &aux (x-more (lambda () (funcall x input))))
    (labels ((more (&aux rest value present-p)
               (declare (ignorable value present-p))
               (when x-more
                 (setf (values rest value present-p x-more)
                       (funcall x-more)))
               (if rest
                   (values rest nil nil #'more)
                   (funcall y input))))
      (more))))

(defun ?optional (parser)
  (?alternate parser (?seq)))

(defun ?all (parser)
  (?optional (?plus parser (lambda (input)
                             (funcall (?all parser) input)))))

(defun ?path (&rest parsers)
  (if parsers
      (reduce '?plus parsers)
      'identity))

(defun ?either (&rest parsers)
  (if parsers
      (reduce '?alternate parsers)
      (constantly nil)))
