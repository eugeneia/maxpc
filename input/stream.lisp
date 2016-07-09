(in-package :maxpc.input.stream)

(defstruct (index-stream (:include index))
  (stream (error "Must supply STREAM.") :type stream :read-only t)
  (buffer (error "Must supply ARRAY.") :type array :read-only t))

(defparameter *chunk-size* (* 5 1000 1000)) ; 5 Mega

(defgeneric fill-buffer (buffer stream))

(defmethod fill-buffer ((buffer array) (stream stream))
  (handler-case (vector-push-extend
                 (ecase (stream-element-type stream)
                   (character (read-char stream))
                   (signed-byte (read-byte stream)))
                 buffer
                 (the fixnum *chunk-size*))
    (end-of-file (error) (declare (ignore error)))))

(defmethod fill-buffer ((buffer array) (stream file-stream))
  (let* ((file-length (file-length stream))
         (old-buffer-size (length buffer))
         (new-buffer-size (min (+ old-buffer-size *chunk-size*) file-length)))
    (unless (= file-length old-buffer-size)
      (adjust-array buffer new-buffer-size)
      (setf (fill-pointer buffer)
            new-buffer-size)
      (setf (fill-pointer buffer)
            (read-sequence buffer stream :start old-buffer-size)))))

(defmethod make-input ((input-source stream))
  (let ((buffer (make-array 0
                            :element-type (stream-element-type input-source)
                            :adjustable t
                            :fill-pointer t)))
    (fill-buffer buffer input-source)
    (make-index-stream :stream input-source :buffer buffer)))

(defmethod input-empty-p ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (= (the position (index-position input))
     (the position (length (the array (index-stream-buffer input))))))

(defmethod input-first  ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the array (index-stream-buffer input))
        (the position (index-position input))))

(defmethod input-rest  ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((position (index-position input))
        (buffer (index-stream-buffer input))
        (stream (index-stream-stream input)))
    (let ((next-position (1+ (the position position))))
      (unless (< (the position next-position)
                 (the position (length (the array buffer))))
        (fill-buffer buffer stream))
      (make-index-stream :stream (the stream stream)
                         :buffer (the array buffer)
                         :position (the position next-position)))))

(defmethod input-element-type ((input index-stream))
  (stream-element-type (the stream (index-stream-stream input))))

(defmethod input-sequence ((input index-stream) (length fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((position (index-position input))
        (buffer (index-stream-buffer input))
        (stream (index-stream-stream input)))
    (make-array length
                :element-type (stream-element-type (the stream stream))
                :displaced-to (the array buffer)
                :displaced-index-offset (the position position))))
