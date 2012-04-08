(defun tostr (x)
   (declare (xargs :mode :program))
   (mv-let (col str)
           (fmt1-to-string "~x0"
                           (list (cons #\0 x))
                           0)
      (declare (ignore col))
      str))

(defmacro cat (&rest args)
   `(concatenate 'string ,@args))

(defun poly-print-r (fs p)
   (declare (xargs :mode :program))
   (if (endp fs)
       ""
       (cat (poly-print-r (rest fs) (+ p 1))
            (if (/= (first fs) 1)
                (tostr (first fs))
                "")
            (cond ((= p 0) "")
                  ((= p 1) "x + ")
                  ((> p 1)
                   (cat "x^" (tostr p)
                        " + "))))))

(defun poly-print (fs)
   (declare (xargs :mode :program))
   (poly-print-r fs 0))

(defun poly-eval (fs x)
   (if (endp fs)
       0
       (+ (first fs)
          (* x (poly-eval (rest fs) x)))))

(defun poly+ (fs gs)
   (cond ((endp fs) gs)
         ((endp gs) fs)
         (t (cons (+ (first fs) (first gs))
                  (poly+ (rest fs) (rest gs))))))

(defun poly-negate (fs)
   (if (endp fs)
       nil
       (cons (- (first fs))
             (poly-negate (rest fs)))))

(defun poly- (fs gs)
   (poly+ fs (poly-negate gs)))

(defun poly-deriv-r (fs i)
   (if (endp fs)
       nil
       (cons (* (first fs) i)
             (poly-deriv-r (rest fs) (+ i 1)))))

(defun poly-deriv (fs)
   (poly-deriv-r (rest fs) 1))

(defun rep (n x)
   (if (zp n)
       nil
       (cons x (rep (1- n) x))))

(defun *-each (xs y)
   (if (endp xs)
       nil
       (cons (* (first xs) y)
             (*-each (rest xs) y))))

(defun poly*-term (fs g p)
   (append (rep p 0)
           (*-each fs g)))

(defun poly*-r (fs gs p)
   (if (endp gs)
       nil
       (poly+ (poly*-term fs (first gs) p)
              (poly*-r fs (rest gs) (+ p 1)))))

(defun poly* (fs gs)
   (poly*-r fs gs 0))
