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
