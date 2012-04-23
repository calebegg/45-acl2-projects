(in-package "ACL2")

(include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)

(set-state-ok t)

(defun sum-n-randoms (n places state)
   (if (zp n)
       (mv 0 state)
       (mv-let (rnd state)
               (random$ (expt 10 places) state)
          (mv-let (total state)
                  (sum-n-randoms (- n 1) places state)
             (mv (+ (/ rnd (expt 10 places)) total) state)))))

;; Algorithm via http://en.wikipedia.org/wiki/Normal_distribution
(defun std-gaussian (places state)
   (mv-let (rnd state)
           (sum-n-randoms 12 places state)
      (mv (- rnd 6) state)))

(defun generate-normal-nums (n mean stdev places state)
   (if (zp n)
       (mv nil state)
       (mv-let (std state)
               (std-gaussian places state)
          (mv-let (xs state)
                  (generate-normal-nums (- n 1) mean stdev places state)
             (mv (cons (+ mean (* stdev std)) xs) state)))))

(defun generate-normals (places state)
   (generate-normal-nums 1000 1 1/2 places state))

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

(defun print-decimal-list (rs places)
   (if (endp rs)
       nil
       (prog2$ (cw "~@0~%" (as-decimal-str (first rs) places))
               (print-decimal-list (rest rs) places))))

(defun print-normals (places state)
   (mv-let (xs state)
           (generate-normals places state)
      (prog2$ (print-decimal-list xs places)
              state)))
