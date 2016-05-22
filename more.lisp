;;;; Composite combinators and macros of the core library.

(in-package :maxpc)

(defun %maybe (parser)
  (%or parser (?list)))

(defun ?not (parser)
  (%diff (=transform (=element) (constantly nil)) parser))

(defun %some (parser)
  (%and parser (%any parser)))

(defmacro ?test ((predicate &rest arguments) &optional (parser (=element))
                 &aux (value-sym (gensym "value")))
  `(?satisfies (lambda (,value-sym)
                 (funcall ,predicate ,value-sym ,@arguments))
               ,parser))

(defun ?eq (x &optional (parser (=element)))
  (?test ('eq x) parser))

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
