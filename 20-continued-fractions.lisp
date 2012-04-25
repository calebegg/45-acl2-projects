(defun sqrt-2-approx-r (iters i)
   (if (zp iters)
       1
       (+ (if (= i 0) 1 2)
          (/ 1 (sqrt-2-approx-r (1- iters) (1+ i))))))

(defun e-approx-r (iters i)
   (if (zp iters)
       1
       (+ (if (= i 0) 2 i)
          (/ (if (= i 0) 1 i)
             (e-approx-r (1- iters) (1+ i))))))

(defun pi-approx-r (iters i)
   (if (zp iters)
       1
       (+ (if (= i 0) 3 6)
          (/ (let ((x (- (* 2 (1+ i)) 1))) (* x x))
             (pi-approx-r (1- iters) (1+ i))))))

(defun sqrt-2-approx (iters) (sqrt-2-approx-r iters 0))
(defun e-approx (iters) (e-approx-r iters 0))
(defun pi-approx (iters) (pi-approx-r iters 0))

;; Below is from day 16.

(defmacro cat (&rest args)
   `(concatenate 'string ,@args))

(defun dgt-to-str (d)
   (case d
         (1 "1") (2 "2") (3 "3") (4 "4") (5 "5")
         (6 "6") (7 "7") (8 "8") (9 "9") (0 "0")))

(defun num-to-str-r (n)
   (if (zp n)
       ""
       (cat (num-to-str-r (floor n 10))
            (dgt-to-str (mod n 10)))))

(defun num-to-str (n)
   (cond ((= n 0) "0")
         ((< n 0) (cat "-" (num-to-str-r (- n))))
         (t (num-to-str-r n))))

(defun as-decimal-str (r places)
   (let ((before (floor r 1))
         (after (floor (* (expt 10 places) (mod r 1)) 1)))
        (cat (num-to-str before)
             "."
             (num-to-str after))))