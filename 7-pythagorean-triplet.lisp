(defun sq (x) (* x x))

(defun pythag-triplep (a b c)
   (= (+ (sq a) (sq b)) (sq c)))

(defun floor-sqrt-r (n i)
   (cond ((zp i) nil)
         ((= n (sq i)) i)
         (t (floor-sqrt-r n (1- i)))))

(defun floor-sqrt (n)
   (floor-sqrt-r n n))

(defun pythag-triples-on-b (a b)
   (if (zp b)
       nil
       (let ((c? (floor-sqrt (+ (sq a) (sq b)))))
            (if c?
                (cons (list a b c?)
                      (pythag-triples-on-b a (- b 1)))
                (pythag-triples-on-b a (- b 1))))))

(defun pythag-triples-on-a (a m)
   (if (zp a)
       nil
       (append (pythag-triples-on-b a m)
               (pythag-triples-on-a (1- a) (1- a)))))

(defun pythag-triples (m)
   (pythag-triples-on-a m m))

(defun sum (xs)
   (if (endp xs)
       0
       (+ (first xs) (sum (rest xs)))))

(defun filter-sum=1000 (ts)
   (if (endp ts)
       nil
       (if (= (sum (first ts)) 1000)
           (first ts)
           (filter-sum=1000 (rest ts)))))

:set-state-ok t

(defun main (state)
   (mv state (cw "~x0" (filter-sum=1000 (pythag-triples 500)))))