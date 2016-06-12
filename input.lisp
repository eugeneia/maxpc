;;;; Input primitives.

(in-package :maxpc)


;;; Input interface

(defgeneric make-input (source)
  (:documentation "Returns input object for SOURCE."))

(defgeneric input-position (input)
  (:documentation "Returns index position of INPUT."))

(defgeneric input-element-type (input)
  (:documentation "Returns element type of INPUT."))

(defgeneric input-empty-p (input)
  (:documentation "Predicate to test if INPUT is empty."))

(defgeneric input-first (input)
  (:documentation "Returns first element of INPUT."))

(defgeneric input-rest (input)
  (:documentation "Returns INPUT with its first element stripped."))

(defgeneric input-sequence (input length)
  (:documentation "Returns subsequence of INPUT with LENGTH."))


;;; Generic INPUT-ELEMENT-TYPE implementation

(defmethod input-element-type ((input t)) t)


;;; Generic index implementation

(deftype array-index ()
  "Array index type used in index structure."
  `(integer 0 ,array-dimension-limit))

(defstruct index
  "Generic index."
  (position 0 :type array-index :read-only t))

(defmethod input-position ((input index))
  (index-position input))


;;; Implementation for lists

(defstruct (index-list (:include index))
  "Index list."
  (list (error "Must supply LIST.") :type list :read-only t))

(defmethod make-input ((input list))
  (check-type input list)
  (make-index-list :list input))

(defmethod input-empty-p ((input index-list))
 (declare (optimize (speed 3) (debug 0) (safety 0)))
  (null (the list (index-list-list input))))

(defmethod input-first ((input index-list))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (car (the list (index-list-list input))))

(defmethod input-rest ((input index-list))
  (declare (optimize (speed 3) (debug 0)))
  (make-index-list :list (cdr (the list (index-list-list input)))
		   :position (1+ (the array-index (index-position input)))))

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

(defmethod make-input ((input array))
  (etypecase input
    (simple-array (make-index-simple-array :array input))
    (array        (make-index-array :array input))))

(defmethod make-input ((input string))
  (etypecase input
    (simple-string (make-index-simple-string :array input))
    (string        (make-index-array :array input))))

(defmethod input-element-type ((input index-array))
  (array-element-type (index-array-array input)))

(defmethod input-empty-p ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (= (the array-index (index-position input))
     (the array-index (length (the array (index-array-array input))))))

(defmethod input-first ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the array (index-array-array input))
        (index-position input)))

(defmethod input-first ((input index-simple-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-array (index-array-array input))
        (index-position input)))

(defmethod input-first ((input index-simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-string (index-array-array input))
        (index-position input)))

(defmethod input-rest ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-array
   :array (the array (index-array-array input))
   :position (1+ (the array-index (index-position input)))))

(defmethod input-rest ((input index-simple-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-simple-array
   :array (the simple-array (index-array-array input))
   :position (1+ (the array-index (index-position input)))))

(defmethod input-rest ((input index-simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-simple-string
   :array (the simple-string (index-array-array input))
   :position (1+ (the array-index (index-position input)))))

(defmethod input-sequence ((input index-array) (length fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-array length
              :element-type (input-element-type input)
              :displaced-to (index-array-array input)
              :displaced-index-offset (index-array-position input)))


;;; Implementation for streams

(defmethod make-input ((input file-stream))
  (let ((array (make-array (file-length input)
			   :element-type (stream-element-type input))))
    (make-input (subseq array 0 (read-sequence array input)))))
