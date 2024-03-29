
(defun cons-each (xs xss)
   (if (or (endp xs) (endp xss))
       nil
       (cons (cons (first xs) (first xss))
             (cons-each (rest xs) (rest xss)))))

(defun list-each (xs)
   (if (endp xs)
       nil
       (cons (list (first xs))
             (list-each (rest xs)))))

(defun transpose-list (xss)
   (if (endp (rest xss))
       (list-each (first xss))
       (cons-each (first xss)
                  (transpose-list (rest xss)))))