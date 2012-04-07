(include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)

(defun dgts (n)
   (if (zp n)
       nil
       (cons (mod n 10)
             (dgts (floor n 10)))))

(defun num10 (ds)
   (if (endp ds)
       0
       (+ (first ds)
          (* 10 (num10 (rest ds))))))

(defun look-and-say-next (curr cnt ds)
   (cond ((endp ds) (list curr cnt))
         ((= (first ds) curr)
          (look-and-say-next curr (1+ cnt) (rest ds)))
         (t (append (list curr cnt)
                    (look-and-say-next (first ds) 1 (rest ds))))))

(defun look-and-say (start n)
   (if (zp n)
       nil
       (let ((ds (dgts start)))
            (cons start
                  (look-and-say
                   (num10 (look-and-say-next (first ds)
                                              1 (rest ds)))
                   (1- n))))))
