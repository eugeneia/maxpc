(in-package :maxpc)

;;; Parser interface is...
;;;
;;;    (FUNCTION (INPUT) INPUT * BOOLEAN)
;;;
;;; ...where the first return value is the remaining input, the second return
;;; value is the parser’s “result value” or NIL and the third return value is a
;;; boolean that indicates if a result value is present.

(defvar *input-start*)

(defvar *input-fail*)

(defun parse (source parser)
  (let ((*input-start* (make-input source)))
    (multiple-value-bind (rest value) (funcall parser *input-start*)
      (values value
              (not (null rest))
              (or (input-empty-p *input-start*)
                  (and rest (input-empty-p rest)))))))

(defun parse-line-position (input position)
  "Parses line position of POSITION in INPUT."
  (loop for i from 0 to position
     for in = input then (input-rest in)
     for newline-p = (unless (input-empty-p in)
                       (char= #\Newline (input-first in)))
     for character = 0 then (if newline-p 0 (1+ character))
     for line = 1 then (+ line (if newline-p 1 0))
     when (= i position) return (values line character)))

(defun get-input-position ()
  "→ _position_

   → _position_, _line_, _column_

   *Arguments and Values:*

   _position_, _column_—non-negative _integers_.

   _line_—a positive _integer_.

   *Description:*

   {get-input-position} returns the number of items read from the
   input. Additionally, _line_ and _column_ positions are returned if the
   input's _element type_ is {character}. Lines are counted starting at 1
   while columns are counted starting from 0.

   {get-input-position} may only be called from within the body of
   {?fail}, the handlers of {%handler-case} or the restarts of
   {%restart-case}.

   *Exceptional Situations:*

   {get-input-position} signals an _error_ of _type_ {simple-error}
   unless called within {?fail}, {%handler-case} or {%restart-case}."
  (unless *input-fail*
    (error "GET-INPUT-POSITION may only be called inside =FAIL,
=HANDLER-CASE and =RESTART-CASE."))
  (let ((position (input-position *input-fail*)))
    (if (eq (input-element-type *input-fail*) 'character)
	(multiple-value-bind (line character)
	    (parse-line-position *input-fail* position)
	  (values position line character))
	position)))

(defun cases-to-parser-cases (cases input)
  "Utility macro function for =HANDLER-CASE and =RESTART-CASE."
  (loop for case in cases do
       (assert (= 3 (length case)) (case) "Invalid case: ~a" case)
     collect
       `(,(first case)
	 ,(second case)
	  (funcall ,(third case) ,input))))

(defmacro %handler-case (parser &rest handlers)
  "*Arguments and Values:*

   _parser_—a _parser_.

   _handlers_—handler clauses for {handler-case}.

   *Description:*

   {%handler-case} establishes _handlers_ as if by {handler-case} before
   applying _parser_ to the input. _Handlers_ must return _parsers_. If
   _parser_ signals an _error_ matched by a _handler_, the _parser_
   returned by the _handler_ will be applied to the input."
  (let ((parser-sym (gensym "PARSER"))
	(input (gensym "INPUT")))
    `(let ((,parser-sym ,parser))
       (lambda (,input)
	 (let ((*input-fail* ,input))
	   (handler-case (funcall ,parser-sym ,input)
	     ,@(cases-to-parser-cases handlers input)))))))

(defmacro %restart-case (parser &rest restarts)
  "*Arguments and Values:*

   _parser_—a _parser_.

   _restarts_—restart clauses for {restart-case}.

   *Description:*

   {%restart-case} establishes _restarts_ as if by {restart-case} before
   applying _parser_ to the input. _Restarts_ must return _parsers_. If
   _parser_ signals an _error_ matched by a _restart_, the _parser_
   returned by the _restart_ will be applied to the input."
  (let ((parser-name (gensym "PARSER"))
	(input (gensym "INPUT")))
    `(let ((,parser-name ,parser))
       (lambda (,input)
	 (let ((*input-fail* ,input))
           (restart-case (funcall ,parser-name ,input)
             ,@(cases-to-parser-cases restarts input)))))))
