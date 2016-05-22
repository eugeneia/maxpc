(in-package :maxpc)

(defun %maybe (parser)
  (%or parser (?list)))

(defun ?not (parser)
  (%diff (=transform (=element) (constantly nil)) parser))

(defun %some (parser)
  (%and parser (%any parser)))

(defun ?test (parser predicate &rest arguments)
  (?satisfies (lambda (value) (apply predicate value arguments))
              parser))

(defun ?eq (x &optional (parser (=element)))
  (?test parser 'eq x))

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
    `(=transform ,parser
                 (lambda (,result-sym)
                   (destructuring-bind ,bindings ,result-sym
                     (declare (ignore ,@ignorable-syms))
                     ,@body)))))
