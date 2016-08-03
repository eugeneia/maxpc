(in-package :maxpc.input.index)

;;; Generic index implementation

(deftype index-position ()
  "Array index type used in index structure."
  (upgraded-array-element-type
   `(integer 0 ,array-dimension-limit)))

(defstruct index
  "Generic index."
  (position 0 :type index-position :read-only t))

(defmethod input-position ((input index))
  (index-position input))
