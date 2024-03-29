(defun fac (n)
   (if (zp n)
       1
       (* n (fac (1- n)))))

(defconst *pi-approx*
   (/ 3141592653589793238462643383279
      (expt 10 30)))

(include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)

(defun dgt-to-str (d)
   (case d
         (1 "1") (2 "2") (3 "3") (4 "4") (5 "5")
         (6 "6") (7 "7") (8 "8") (9 "9") (0 "0")))

(defmacro cat (&rest args)
   `(concatenate 'string ,@args))
 
(defun num-to-str-r (n)
   (if (zp n)
       ""
       (cat (num-to-str-r (floor n 10))
            (dgt-to-str (mod n 10)))))

(defun num-to-str (n)
   (cond ((= n 0) "0")
         ((< n 0) (cat "-" (num-to-str-r (- n))))
         (t (num-to-str-r n))))

(defun pad-with-zeros (places str lngth)
   (declare (xargs :measure (nfix (- places lngth))))
   (if (zp (- places lngth))
       str
       (pad-with-zeros places (cat "0" str) (1+ lngth))))

(defun as-decimal-str (r places)
   (let ((before (floor r 1))
         (after (floor (* (expt 10 places) (mod r 1)) 1)))
        (cat (num-to-str before)
             "."
             (let ((afterstr (num-to-str after)))
                  (pad-with-zeros places afterstr
                             (length afterstr))))))

(defun taylor-sine (theta terms term)
   (declare (xargs :measure (nfix (- terms term))))
   (if (zp (- terms term))
       0
       (+ (/ (*(expt -1 term) (expt theta (1+ (* 2 term))))
             (fac (1+ (* 2 term))))
          (taylor-sine theta terms (1+ term)))))

(defun sine (theta)
   (taylor-sine (mod theta (* 2 *pi-approx*))
                 20 0)) ; About 30 places of accuracy

(defun cosine (theta)
   (sine (+ theta (/ *pi-approx* 2))))

(defun tangent (theta)
   (/ (sine theta) (cosine theta)))

(defun rad->deg (rad)
   (* 180 (/ rad *pi-approx*)))

(defun deg->rad (deg)
   (* *pi-approx* (/ deg 180)))

(defun trig-demo ()
   (progn$ (cw "sine of pi / 4 radians:    ")
           (cw (as-decimal-str (sine (/ *pi-approx* 4)) 20))
           (cw "~%sine of 45 degrees:        ")
           (cw (as-decimal-str (sine (deg->rad 45)) 20))
           (cw "~%cosine of pi / 4 radians:  ")
           (cw (as-decimal-str (cosine (/ *pi-approx* 4)) 20))
           (cw "~%tangent of pi / 4 radians: ")
           (cw (as-decimal-str (tangent (/ *pi-approx* 4)) 20))
           (cw "~%")))
