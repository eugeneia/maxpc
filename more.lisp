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

(defmacro =destructure ((&rest lambda-list) parser &body forms
                        &aux (result-sym (gensym "result")))
  (let* ((ignorable-syms)
         (lambda-list (loop for symbol in lambda-list collect
                           (if (string= "_" (symbol-name symbol))
                               (car (push (gensym "_") ignorable-syms))
                               symbol)))
         (special-syms (list* '&optional '&rest '&key '&allow-other-keys '&aux
                              ignorable-syms))
         (body (or forms (remove-if (lambda (x) (member x special-syms))
                                    lambda-list))))
    `(=transform ,parser
                 (lambda (,result-sym)
                   (destructuring-bind ,lambda-list ,result-sym
                     (declare (ignore ,@ignorable-syms))
                     ,@body)))))
