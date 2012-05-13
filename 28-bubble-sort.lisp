(defun bubble (xs)
   (if (endp (rest xs))
       (mv nil xs)
       (let ((x1 (first xs))
             (x2 (second xs)))
         (if (> x1 x2)
             (mv-let (_ ys)
                     (bubble (cons x1 (rest (rest xs))))
                (declare (ignore _))
                (mv t (cons x2 ys)))
             (mv-let (has-changed ys)
                     (bubble (rest xs))
                (mv has-changed (cons x1 ys)))))))

(defun bsort-r (xs limit)
   (declare (xargs :measure (nfix limit)))
   (if (zp limit)
       xs
       (mv-let (has-changed ys)
               (bubble xs)
          (if has-changed
              (bsort-r ys (1- limit))
              ys))))

(defun bsort (xs)
   (bsort-r xs (len xs)))