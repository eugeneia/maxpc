(in-package :maxpc)

(defun %maybe (parser)
  (%or parser (%value)))

(defun ?not (parser)
  (%diff (=transform (=element) (constantly nil)) parser))

(defun %some (parser)
  (%and parser (%any parser)))

(defun ?test (parser predicate &rest arguments)
  (?satisfies (lambda (value) (apply predicate value arguments))
              parser))

(defun ?eq (x &optional (parser (=element)))
  (?test parser 'eq x))

(defun =list-form-p (form)
  (and (listp form) (eq (first form) '=list)))

;; This is a “primitive” strictly speaking.
(defun special-case-=list (bindings ignorable-syms parsers body
                           &aux (input-sym (gensym "input"))
                                (rest-sym (gensym "rest"))
                                (value-sym (gensym "value"))
                                (df-sym (gensym "df")))
  `(let ((,df-sym (lambda ,bindings
                    (declare (ignore ,@ignorable-syms))
                    ,@body)))
     (lambda (,input-sym)
       (block =destructure-=list
         (let ((,value-sym
                (funcall ,df-sym
                         ,@(loop for parser in parsers collect
                                `(multiple-value-bind (,rest-sym ,value-sym)
                                     (funcall ,parser ,input-sym)
                                   (unless ,rest-sym
                                     (return-from =destructure-=list))
                                   (setf ,input-sym ,rest-sym)
                                   ,value-sym)))))
         (values ,input-sym ,value-sym t))))))

(defmacro =destructure ((&rest bindings) parser &body forms
                        &aux (result-sym (gensym "result")))
  (let* ((ignorable-syms)
         (bindings (loop for binding in bindings collect
                        (if (string= "_" (symbol-name binding))
                            (car (push (gensym "_") ignorable-syms))
                            binding)))
         (special-syms (list* '&optional '&rest '&key '&allow-other-keys '&aux
                              ignorable-syms))
         (body (or forms (set-difference bindings special-syms))))
    (if (=list-form-p parser)
        (special-case-=list bindings ignorable-syms (rest parser) body)
        `(=transform ,parser
                     (lambda (,result-sym)
                       (destructuring-bind ,bindings ,result-sym
                         (declare (ignore ,@ignorable-syms))
                         ,@body))))))
