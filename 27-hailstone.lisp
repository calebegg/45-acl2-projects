(defun hailstone (n)
   (declare (xargs :mode :program))
   (cond ((zp (1- n))
          (list 1))
         ((evenp n)
          (cons n (hailstone (floor n 2))))
         (t (cons n (hailstone (1+ (* 3 n)))))))
 
;; Must be tail recursive
(defun max-hailstone-start (limit mx curr)
   (declare (xargs :mode :program))
   (if (zp limit)
       (mv mx curr)
       (let ((new-mx (len (hailstone limit))))
          (if (> new-mx mx)
              (max-hailstone-start (1- limit) new-mx limit)
              (max-hailstone-start (1- limit) mx curr)))))
