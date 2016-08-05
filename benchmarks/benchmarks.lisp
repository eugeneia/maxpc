(ql:quickload '(:maxpc :esrap :maxpc-test :mpc))

(defun spath (p &optional (system "maxpc"))
  (merge-pathnames
   p
   (make-pathname :defaults (asdf:system-definition-pathname system)
                  :type nil
                  :name nil)))

(load (spath #p"examples/sexp.lisp" "esrap"))
(load (spath #p"benchmarks/mpc-sexp.lisp"))

(defmacro measure (&body body &aux (start (gensym "start")))
  `(let ((,start (get-internal-real-time)))
     (progn ,@body)
     (/ (- (get-internal-real-time) ,start)
        internal-time-units-per-second)))

(defparameter *sample-size* 10)

(defmacro benchmark (&body body)
  `(format t " ~f" (loop for i from 1 to *sample-size*
                      do (gc) minimize (measure ,@body))))

(defun gen-test-sexp (x y)
  (loop for i from 1 to x
     collect 'foo
     collect "bar"
     collect (loop for i from 1 to y
                collect '(1 2 (3 4 ("foo" "bar"))))))

(defun slurp (p)
  (with-open-file (in p :element-type 'character)
    (let ((s (make-array (file-length in) :element-type 'character)))
      (read-sequence s in)
      s)))

(defun run-sexp-benchmarks ()
  (format t "~&# size read-from-string maxpc mpc esrap~%")
  (loop for series in '(#p"benchmarks/sexp1"
                        #p"benchmarks/sexp2"
                        #p"benchmarks/sexp3"
                        #p"benchmarks/sexp4"
                        #p"benchmarks/sexp5"
                        #p"benchmarks/sexp6"
                        #p"benchmarks/sexp7"
                        #p"benchmarks/sexp8") do
       (let ((sexp (slurp (spath series))))
         (format t "~a" (length sexp))
         (benchmark (read-from-string sexp))
         (let ((maxpc-sexp (maxpc.example-sexp:=sexp)))
           (benchmark (maxpc:parse sexp maxpc-sexp)))
         (let ((mpc-sexp (mpc.example-sexp:=sexp)))
           (benchmark(mpc:run mpc-sexp sexp)))
         (benchmark (esrap:parse 'sexp-grammar::sexp sexp)))
       (terpri)))

(defun run-input-benchmarks ()
  (let ((any (maxpc:%any (maxpc:?satisfies (constantly t)))))
    (format t "~&# size list vector stream file-stream~%")
    (loop for series in '(#p"benchmarks/zero1"
                          #p"benchmarks/zero2"
                          #p"benchmarks/zero3"
                          #p"benchmarks/zero4") do
         (let ((zero (slurp (spath series))))
           (format t "~a" (length zero))
           (let ((lzero (coerce zero 'list)))
             (benchmark (maxpc:parse lzero any)))
           (benchmark (maxpc:parse zero any))
           (benchmark (with-input-from-string (in zero)
                        (maxpc:parse in any))))
         (benchmark (with-open-file (in (spath series))
                      (maxpc:parse in any)))
         (terpri))))
