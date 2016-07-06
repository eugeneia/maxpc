;;;; Composite combinators and macros of the core library.

(in-package :maxpc)

(defun %maybe (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {%maybe} matches _parser_ or nothing all, but always succeeds. If _parser_
   matched and produced a result value then {%maybe} produces the value as its
   result value.

   *Examples:*

   #code#
   (parse '() (%maybe (=element))) → NIL, T, T
   #"
  (%or parser (?list)))

(defun ?not (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {%not} matches the next element unless _parser_ matches.

   *Examples:*

   #code#
   (parse '(:foo :baz) (?not (?list (?eq :foo) (?eq :bar))))
   → NIL, T, NIL
   (parse '() (?not (?eq :baz))
   → NIL, NIL, NIL
   #"
  (%diff (=transform (=element) (constantly nil)) parser))

(defun %some (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {%some} matches _parser_ in sequence one or more times. If _parser_ produces
   a result value then {%some} produces a _list_ of the values as its result
   value.

   *Examples:*

   #code#
   (parse '(a b c) (%some (=element))) → (A B C), T, T
   (parse '() (%some (=element))) → NIL, NIL, T
   #"
  (%and parser (%any parser)))

(defmacro ?test ((test &rest arguments) &optional (parser '(=element))
                 &aux (value-sym (gensym "value")))
  "*Arguments and Values:*

   _test_—a _designator_ for a _function_ that returns a _generalized boolean_.

   _arguments_—_objects_.

   _parser_—a _parser_. The default is {(=element)}.

   *Description:*

   {?test} matches _parser_ if its result value _satisfies the test_ with
   _arguments_ as additional arguments to _test_.


   *Notes:*

   #code#
   (?test ({fun} {args}*}))
   ≡ (?satisfies (lambda (x)
                   (funcall {fun} x {args}*)))
   #

   *Exceptional Situations:*

   If _test_ accepts less arguments than the number of _arguments_ plus one an
   _error_ of _type_ {program‑error} is signaled."
  `(?satisfies (lambda (,value-sym)
                 (funcall ,test ,value-sym ,@arguments))
               ,parser))

(defun ?eq (x &optional (parser (=element)))
  "*Arguments and Values:*

   _x_—an _object_.

   _parser_—a _parser_. The default is {(=element)}.

   *Description:*

   {?eq} matches _parser_ if its result value is {eq} to _x_."
  (?test ('eq x) parser))

(defun =list-form-p (form)
  (and (listp form) (eq (first form) '=list)))

;; This is a “primitive” strictly speaking.
(defun special-case-=list (lambda-list ignorable-syms parsers body
                           &aux (input-sym (gensym "input"))
                                (rest-sym (gensym "rest"))
                                (value-sym (gensym "value"))
                                (df-sym (gensym "df")))
  `(let ((,df-sym (lambda ,lambda-list
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

(defmacro =destructure ((&rest lambda-list) parser &body forms
                        &aux (result-sym (gensym "result")))
  "*Arguments and Values:*

   _lambda‑list_—a _destructuring lambda list_.

   _parser_—a _parser_.

   _forms_—an _implicit progn_.

   *Description:*

   {=destructure} matches _parser_ and destructures its result value as if by
   {destructuring‑bind}. The {_} (underscore) symbol can occur in _lambda‑list_
   any number of times, and is substituted with a _fresh_, _uninterned symbol_
   and declared {ignorable}. If _parser_ matches {=destructure} evaluates
   _forms_ and produces the value of the last _form_ as its result value. If no
   _forms_ are supplied the value of the last, _interned_ variable defined in
   _lambda‑list_ is produced as the result value instead.

   *Examples:*

   #code#
   (parse \"a,\" (=destructure (x _)
                     (=list (=element) (?eq #\\,))
                   (char-upcase x)))
   → #\\A, T, T

   (parse \"a,\" (=destructure (x _)
                     (=list (=element) (?eq #\\,)))
   → #\\a, T, T

   (parse \"abc\" (=destructure (x &rest xs)
                      (%any (=element)))
   → '(#\\b #\\c), T, T
   #

   *Exceptional Situations:*

   If the result value of _parser_ does not match the destructuring pattern, an
   _error_ of _type_ {program‑error} is signaled.

   *See Also:*

   {destructuring‑bind}"
  (let* ((ignorable-syms)
         (lambda-list (loop for symbol in lambda-list collect
                           (if (string= "_" (symbol-name symbol))
                               (car (push (gensym "_") ignorable-syms))
                               symbol)))
         (special-syms (list* '&optional '&rest '&key '&allow-other-keys '&aux
                              ignorable-syms))
         (body (or forms (remove-if (lambda (x) (member x special-syms))
                                    lambda-list))))
    (if (=list-form-p parser)
        (special-case-=list lambda-list ignorable-syms (rest parser) body)
        `(=transform ,parser
                     (lambda (,result-sym)
                       (destructuring-bind ,lambda-list ,result-sym
                         (declare (ignore ,@ignorable-syms))
                         ,@body))))))
