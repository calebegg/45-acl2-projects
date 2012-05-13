(include-book "arithmetic-3/top" :dir :system)

(defun log10 (n)
   (if (zp n)
       0
       (1+ (log10 (floor n 10)))))

;; From Day 24
(defun fast-fib-r (n a b)
   (if (or (zp n) (zp (1- n)))
       b
       (fast-fib-r (1- n) b (+ a b))))

(defun fast-fib (n)
   (fast-fib-r n 1 1))

(defun first-fib-with-1000-dgts (limit i)
   (declare (xargs :measure (nfix (- limit i))))
   (if (zp (- limit i))
       nil
       (if (= (log10 (fast-fib i)) 1000)
           i
           (first-fib-with-1000-dgts limit (1+ i)))))

(first-fib-with-1000-dgts 10000 0)
