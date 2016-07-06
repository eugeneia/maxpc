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

(defmethod make-input ((input-source file-stream))
  (let ((array (make-array (file-length input-source)
			   :element-type (stream-element-type input-source))))
    (make-input (subseq array 0 (read-sequence array input-source)))))
