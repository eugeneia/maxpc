;;;; Parsers for digit numerals.

(in-package :maxpc.digit)

(defun ?digit (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {?digit} matches a single digit _character_ in the specified _radix_."
  (?test ('digit-char-p radix)))

(defun =natural-number (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {=natural-number} matches a sequence of one or more digit _characters_ in
   the specified _radix_ and returns the natural _number_ represented by that
   sequence."
  (=transform (=subseq (%some (?digit radix)))
              (lambda (digits)
                (parse-integer digits :radix radix))))

(defun =integer-number (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {=integer-number} matches a signed sequence of one or more digit
   _characters_ in the specified _radix_ and returns the _integer_ represented
   by that sequence. The leading sign is optional and can be {#\\\\+} and
   {#\\\\-} for positive and negative values respectively. The default is a
   positive value."
  (=destructure (sign number)
      (=list (%maybe (%and (?test ('member '(#\+ #\-))) (=element)))
             (=natural-number radix))
    (ecase sign
    (case sign
      (#\- (- number))
      ((#\+ nil) number))))
