(include-book "arithmetic-3/top" :dir :system)

(defun sum-dgts^5 (x)
   (if (zp x)
       0
       (+ (expt (mod x 10) 5)
          (sum-dgts^5 (floor x 10)))))

(defun =sum-dgts^5 (limit)
   (if (zp (1- limit)) ; exclude 1
       nil
       (if (= limit (sum-dgts^5 limit))
           (cons limit (=sum-dgts^5 (1- limit)))
           (=sum-dgts^5 (1- limit)))))

(defun sum (xs)
   (if (endp xs)
       0
       (+ (first xs)
          (sum (rest xs)))))

;; We can limit our search to 6 digit numbers, because for
;; longer numbers, d * 9^5 < 10^d; i.e., no larger numbers
;; will meet the criteria.
(sum (=sum-dgts^5 1000000))
