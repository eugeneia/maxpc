;;;; Parsers for character inputs.

(in-package :maxpc.char)

(defun ?char (char &optional (case-sensitive-p t))
  "*Arguments and Values:*

   _char_—a _character_.

   _case-sensitive-p_—a _generalized boolean_. The default is _true_.

   *Description:*

   {?char} matches _char_. {?char} is case sensitive unless _case-sensitive-p_
   is _false_."
  (if case-sensitive-p
      (?test ('char= char))
      (?test ('char-equal char))))

(defun ?string (string &optional (case-sensitive-p t))
  "*Arguments and Values:*

   _string_—a _string_.

   _case-sensitive-p_—a _generalized boolean_. The default is _true_.

   *Description:*

   {?string} matches _string_. {?string} is case sensitive unless
   _case-sensitive-p_ is _false_."
  (apply '?list (loop for char across string collect
                     (?char char case-sensitive-p))))

(defparameter *whitespace* '(#\Tab #\Newline #\Vt #\Ff #\Return #\Space)
  "*Value Type:*

   a _list_ of _characters_.

   *Description:*

   The _value_ of {*whitespace*} is a _list_ of _characters_ considered
   to be _whitespace characters_.")

(defun ?whitespace ()
  "*Description:*

   {?whitespace} matches an atom that is a member of {*whitespace*}."
  (?test ('member *whitespace* :test 'char=)))

(defun %skip-whitespace (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=skip-whitespace} matches zero or more atoms that are members of
   {*whitespace*} and then applies _parser_ to the remaining input. If _parser_
   succeeds {=skip-whitespace} returns its result, otherwise it fails."
  (=destructure (_ result) (=list (%any (?whitespace)) parser)))

(defun ?newline ()
  "*Description:*

   {?newline} matches a {#\\\\Newline} _character_."
  (?char #\Newline))

(defun =line (&optional keep-newline-p)
  "*Arguments and Values:*

   _keep-newline-p_—a _generalized boolean_. The default is _false_.

   *Description:*

   {=line} matches a sequence of zero or more _characters_ terminated by a
   {#\\\\Newline} _character_ or end of input, and returns the _character
   sequence_ as a _string_. The terinating {#\\\\Newline} _character_ is not
   included in _string_ unless _keep-newline-p_ is _true_."
  (=destructure (line _)
      (=list (=subseq (%any (?not (%or (?newline) (?end)))))
             (%maybe (?newline)))
    (if keep-newline-p
        (format nil "~a~%" #1=(coerce line 'string))
        #1#)))
