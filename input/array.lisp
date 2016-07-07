(in-package :maxpc.input.array)

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
