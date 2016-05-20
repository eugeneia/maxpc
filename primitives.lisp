(in-package :maxpc)

(defun ?end  ()
  (lambda (input)
    (when (input-empty-p input)
      input)))

(defun %value (&optional (value 'no-value))
  (case value
    (no-value 'identity)
    (otherwise (lambda (input)
                 (values input value t)))))

(defun =element ()
  (lambda (input)
    (unless (input-empty-p input)
      (values (input-rest input) (input-first input) t))))

(defmacro ?fail (&body forms)
  `(lambda (*input-fail*) ,@forms nil))

(defun ?satisfies (predicate &optional (parser (=element)))
  (lambda (input)
    (multiple-value-bind (rest value) (funcall parser input)
      (when (and rest (funcall predicate value))
        rest))))

;; Return matched sequence
(defun =subseq (parser)
  (lambda (input)
    (let ((rest (funcall parser input)))
      (when rest
        (values rest
                (input-sequence
                 input (- (input-position rest) (input-position input)))
                t)))))

(defun =list (&rest parsers)
  (lambda (input)
    (loop for parser in parsers
       for value =
         (multiple-value-bind (rest value) (funcall parser input)
           (unless rest (return))
           (setf input rest)
           value)
       collect value into list
       finally (return (values input list t)))))

(defun ?list (&rest parsers)
  (lambda (input)
    (loop for parser in parsers
         do (setf input (funcall parser input))
         unless input return nil
         finally (return input))))

(defun %any (parser)
  (lambda (input)
    (let (rest value present-p)
      (loop do (setf (values rest value present-p) (funcall parser input))
         if rest do (setf input rest)
         else return (values input list (not (null list)))
         when present-p collect value into list))))

;; Set union
(defun %or (&rest parsers)
  (lambda (input)
    (loop for parser in parsers do
         (multiple-value-bind (rest value present-p) (funcall parser input)
           (when rest
             (return (values rest value present-p)))))))

;; Set intersection
(defun %and (&rest parsers)
  (lambda (input)
    (let (rest value present-p)
      (loop for parser in parsers do
           (setf (values rest value present-p) (funcall parser input))
         unless rest return nil
         finally (return (values rest value present-p))))))

;; Set difference
(defun %diff (parser &rest parsers)
  (let ((punion (apply '%or parsers)))
    (lambda (input)
      (multiple-value-bind (rest value present-p) (funcall parser input)
        (when (and rest (null (funcall punion input)))
          (values rest value present-p))))))

(defun =transform (parser function)
  (lambda (input)
    (multiple-value-bind (rest value) (funcall parser input)
      (when rest
        (values rest (funcall function value) t)))))

