(defun remove-every (skip xs i)
   (cond ((endp xs)
          (mv i nil))
         ((= (mod i skip) 0)
              (remove-every skip (rest xs) 1))
             (t (mv-let (j ys)
                        (remove-every skip
                                        (rest xs)
                                        (+ i 1))
                  (mv j (cons (first xs) ys))))))

(defun nats-up-to (n)
   (if (zp n)
       nil
       (append (nats-up-to (- n 1)) (list n))))

(defun josephus-r (skip xs i)
   (if (endp (rest xs))
       (first xs)
       (mv-let (j ys)
               (remove-every skip xs i)
         (josephus-r skip ys j))))

(defun josephus (n skip)
   (josephus-r skip (nats-up-to n) 1))
