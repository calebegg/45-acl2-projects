(include-book "arithmetic-3/top" :dir :system)

(defun dgts (x)
   (if (zp x)
       nil
       (cons (mod x 10)
             (dgts (floor x 10)))))

(defun sum (xs)
   (if (endp xs)
       0
       (+ (first xs)
          (sum (rest xs)))))

(sum (dgts (expt 2 1000)))
