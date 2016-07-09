(in-package :maxpc.input.stream)

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
  (= (the position (index-position input))
     (the position (length (the array (index-stream-buffer input))))))

(defmethod input-first  ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the array (index-stream-buffer input))
        (the position (index-position input))))

(defmethod input-rest  ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-slots (position buffer stream) input
    (unless (or (< (the position position)
                   (1- (the position (length (the array buffer)))))
                (eofp stream))
      (fill-buffer buffer stream))
    (make-index-stream :stream (the stream stream)
                       :buffer (the array buffer)
                       :position (1+ (the position position)))))

(defmethod input-element-type ((input index-stream))
  (stream-element-type (the stream (index-stream-stream input))))

(defmethod input-sequence ((input index-stream) (length fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-slots (position buffer stream) input
    (loop while (and (< (the position (length buffer))
                        (+ (the position position) length))
                     (not (eofp stream))) do
         (fill-buffer buffer stream))
    (make-array length
                :element-type (stream-element-type (the stream stream))
                :displaced-to (the array buffer)
                :displaced-index-offset (the position position))))
