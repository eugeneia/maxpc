;;;; Input primitives.

(in-package :maxpc.input)


(defvar *input*)

;;; Input interface

(defgeneric input-empty-p (input position)
  (:documentation
   "→ _empty-p_

    *Arguments and Values:*

    _input-_—an _input_.

    _position-_—an _integer_ between 0 and {array-dimension-limit} inclusively.

    _empty-p-_—a _generalized boolean_.

    *Description:*

    {input-empty-p} returns _true_ if _input_ is empty."))

(defgeneric input-first (input position)
  (:documentation
   "*Arguments and Values:*

    _input-_—an _input_.

    *Description:*

    {input-first} returns the first element in _input_.

    *Exceptional Situations:*

    If _input_ is empty an _error_ of _type_ {error} may be signaled."))

(defgeneric input-element-type (input)
  (:documentation
   "→ _typespec_

    *Arguments and Values:*

    _input-_—an _input_.

    _typespec-_—a _type specifier_.

    *Description:*

    {input-element-type} returns a _type specifier_ that designated the _type_
    of the elements in _input_."))

(defgeneric input-sequence (input position length)
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


;;; Implemetation for arrays

(defmethod input-empty-p ((input array) (position fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (not (< position (length input))))

(defmethod input-first ((input array) (position fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref input position))

(defmethod input-element-type ((input array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (array-element-type input))

(defmethod input-sequence ((input array) (position fixnum) (length fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-array length
              :element-type (input-element-type input)
              :displaced-to input
              :displaced-index-offset position))

(defmethod input-empty-p ((input simple-string) (position fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (not (< (the fixnum position)
          (the fixnum (length (the simple-string input))))))

(defmethod input-first ((input simple-string) (position fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-string input) (the fixnum position)))

(defmethod input-element-type ((input simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  'character)

(defmethod input-sequence ((input simple-array) (position fixnum) (length
                                                                   fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-array (the fixnum length)
              :element-type 'character
              :displaced-to (the simple-string input)
              :displaced-index-offset (the fixnum position)))
