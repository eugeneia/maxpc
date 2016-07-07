;;;; Input primitives.

(in-package :maxpc.input)


;;; Input interface

(defgeneric make-input (input-source)
  (:documentation
   "*Arguments and Values:*

    _input-source_—an _object_.

    *Description:*

    {make-input} returns an _input_ for _input-source_."))

(defgeneric input-empty-p (input)
  (:documentation
   "→ _empty-p_

    *Arguments and Values:*

    _input-_—an _input_.

    _empty-p-_—a _generalized boolean_.

    *Description:*

    {input-empty-p} returns _true_ if _input_ is empty."))

(defgeneric input-first (input)
  (:documentation
   "*Arguments and Values:*

    _input-_—an _input_.

    *Description:*

    {input-first} returns the first element in _input_.

    *Exceptional Situations:*

    If _input_ is empty an _error_ of _type_ {error} may be signaled."))

(defgeneric input-rest (input)
  (:documentation
   "*Arguments and Values:*

    _input-_—an _input_.

    *Description:*

    {input-rest} returns a copy of _input_ with the first element stripped.

    *Exceptional Situations:*

    If _input_ is empty an _error_ of _type_ {error} may be signaled."))

(defgeneric input-position (input)
  (:documentation
   "→ _position_

    *Arguments and Values:*

    _input-_—an _input_.

    _position-_—an _integer_ between 0 and {array-dimension-limit} inclusively.

    *Description:*

    {input-position} returns the _position_ of _input_."))

(defgeneric input-element-type (input)
  (:documentation
   "→ _typespec_

    *Arguments and Values:*

    _input-_—an _input_.

    _typespec-_—a _type specifier_.

    *Description:*

    {input-element-type} returns a _type specifier_ that designated the _type_
    of the elements in _input_."))

(defgeneric input-sequence (input length)
  (:documentation
   "→ _sequence_

    *Arguments and Values:*

    _input-_—an _input_.

    _length-_—an _integer_ between 0 and {array-dimension-limit} inclusively.

    _sequence-_—a _sequence_.

    *Description:*

    {input-sequence} returns a _sequence_ of the next _length_ elements in
    _input_.

    *Exceptional Situations:*

    If the number of elements in _input_ are less than _length_ an _error_ of
    _type_ {error} may be signaled."))


;;; Generic implementation for optional methods

(defmethod input-element-type ((input t)) t)

(defmethod input-position ((input t)) 0)

(defmethod input-sequence ((input t) (length fixnum))
  (loop for i from 1 to length
        for input = input then (input-rest input)
     collect (input-first input)))


;;; Generic index implementation

(deftype position ()
  "Array index type used in index structure."
  (upgraded-array-element-type
   `(integer 0 ,array-dimension-limit)))

(defstruct index
  "Generic index."
  (position 0 :type position :read-only t))

(defmethod input-position ((input index))
  (index-position input))


;;; Implementation for lists

(defstruct (index-list (:include index))
  "Index list."
  (list (error "Must supply LIST.") :type list :read-only t))

(defmethod make-input ((input-source list))
  (make-index-list :list input-source))

(defmethod input-empty-p ((input index-list))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (null (the list (index-list-list input))))

(defmethod input-first ((input index-list))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (car (the list (index-list-list input))))

(defmethod input-rest ((input index-list))
  (declare (optimize (speed 3) (debug 0)))
  (make-index-list :list (cdr (the list (index-list-list input)))
		   :position (1+ (the position (index-position input)))))

(defmethod input-sequence ((input index-list) (length fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (subseq (the list (index-list-list input)) 0 length))


;;; Implementation for arrays

(defstruct (index-array (:include index))
  "Index array."
  (array (error "Must supply ARRAY.") :type array :read-only t))

(defstruct (index-simple-array (:include index-array))
  "Index simple array.")

(defstruct (index-simple-string (:include index-array))
  "Index simple string.")

(defmethod make-input ((input-source array))
  (etypecase input-source
    (simple-string (make-index-simple-string :array input-source))
    (string        (make-index-array :array input-source))
    (simple-array (make-index-simple-array :array input-source))
    (array        (make-index-array :array input-source))))

(defmethod input-empty-p ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (= (the position (index-position input))
     (the position (length (the array (index-array-array input))))))

(defmethod input-first ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the array (index-array-array input))
        (index-position input)))

(defmethod input-first ((input index-simple-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-array (index-array-array input))
        (the position (index-position input))))

(defmethod input-first ((input index-simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-string (index-array-array input))
        (the position (index-position input))))

(defmethod input-rest ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-array
   :array (the array (index-array-array input))
   :position (1+ (the position (index-position input)))))

(defmethod input-rest ((input index-simple-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-simple-array
   :array (the simple-array (index-array-array input))
   :position (1+ (the position (index-position input)))))

(defmethod input-rest ((input index-simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-simple-string
   :array (the simple-string (index-array-array input))
   :position (1+ (the position (index-position input)))))

(defmethod input-element-type ((input index-array))
  (array-element-type (index-array-array input)))

(defmethod input-sequence ((input index-array) (length fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-array length
              :element-type (input-element-type input)
              :displaced-to (index-array-array input)
              :displaced-index-offset (index-array-position input)))


;;; Implementation for streams

(defstruct (index-stream (:include index))
  (stream (error "Must supply STREAM.") :type stream :read-only t)
  (buffer (error "Must supply ARRAY.") :type array :read-only t))

(defparameter *chunk-size* (* 5 1000 1000)) ; 5 Mega

(defun fill-buffer (buffer stream)
  (let* ((old-buffer-size (length buffer))
         (new-buffer-size (+ old-buffer-size *chunk-size*)))
    (adjust-array buffer new-buffer-size)
    (setf (fill-pointer buffer) new-buffer-size)
    (setf (fill-pointer buffer) (read-sequence buffer stream
                                               :start old-buffer-size))))

(defun eofp (stream)
  (eq (peek-char nil stream nil 'eof) 'eof))

(defmethod make-input ((input-source stream))
  (let ((buffer (make-array 0
                            :element-type (stream-element-type input-source)
                            :adjustable t
                            :fill-pointer t)))
    (fill-buffer buffer input-source)
    (make-index-stream :stream input-source :buffer buffer)))

(defmethod input-empty-p ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (= (the position (index-stream-position input))
     (the position (length (the array (index-stream-buffer input))))))

(defmethod input-first  ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the array (index-stream-buffer input))
        (the position (index-stream-position input))))

(defmethod input-rest  ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((position (index-stream-position input))
        (buffer (index-stream-buffer input))
        (stream (index-stream-stream input)))
    (unless (or (< (the position position)
                   (1- (the position (length (the array buffer)))))
                (eofp stream))
      (fill-buffer buffer stream))
    (make-index-stream :stream (the stream stream)
                       :buffer (the array buffer)
                       :position (1+ (the position position)))))

(defmethod input-position  ((input index-stream))
  (index-stream-position input))

(defmethod input-element-type ((input index-stream))
  (stream-element-type (the stream (index-stream-stream input))))

(defmethod input-sequence ((input index-stream) (length fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((position (index-stream-position input))
        (buffer (index-stream-buffer input))
        (stream (index-stream-stream input)))
    (loop while (and (< (the position (length buffer))
                        (+ (the position position) length))
                     (not (eofp stream))) do
         (fill-buffer buffer stream))
    (make-array length
                :element-type (stream-element-type (the stream stream))
                :displaced-to (the array buffer)
                :displaced-index-offset (the position position))))
