(include-book "testing" :dir :teachpacks)
(include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)

(defun btn (n low high)
   (and (natp n)
        (>= n low)
        (<= n high)))

(defmacro cat (&rest args)
   `(concatenate 'string ,@args))

(defun cat-if-not-0 (a b)
   (if (equal b "zero")
       ""
       (cat a b)))

(defun digits-in (n)
   (if (zp n)
       0
       (1+ (digits-in (floor n 10)))))

(defun first-dgts (m n)
   (if (or (zp m) (zp n) (zp (floor n (expt 10 m))))
       (mod n (expt 10 m))
       (first-dgts m (floor n 10))))

(defun remove-first-dgts (m n)
   (if (or (zp n) (zp (floor n (expt 10 m))))
       0
       (+ (mod n 10)
          (* 10 (remove-first-dgts m (floor n 10))))))

:program

(defun num-to-human (n)
   (case n (0 "zero")
           (1 "one")
           (2 "two")
           (3 "three")
           (4 "four")
           (5 "five")
           (6 "six")
           (7 "seven")
           (8 "eight")
           (9 "nine")
           (10 "ten")
           (11 "eleven")
           (12 "twelve")
           (13 "thirteen")
           (14 "fourteen")
           (15 "fifteen")
           (16 "sixteen")
           (17 "seventeen")
           (18 "eighteen")
           (19 "nineteen")
           (otherwise
            (cond ((btn n 20 99)
                   (cat (case (floor n 10)
                              (2 "twenty")
                              (3 "thirty")
                              (4 "fourty")
                              (5 "fifty")
                              (6 "sixty")
                              (7 "seventy")
                              (8 "eighty")
                              (9 "ninety"))
                        (cat-if-not-0
                         "-" (num-to-human (mod n 10)))))
                  ((natp n)
                   (mv-let (m name)
                           (cond ((btn n 100 999)
                                  (mv 1 "hundred"))
                                 ((btn n 1000 999999)
                                  (mv (- (digits-in n) 3) "thousand"))
                                 ((btn n 1000000 999999999)
                                  (mv (- (digits-in n) 6) "million"))
                                 ((btn n 1000000000 999999999999)
                                  (mv (- (digits-in n) 9) "billion"))
                                 ((btn n 1000000000000 999999999999999)
                                  (mv (- (digits-in n) 12) "trillion"))
                                 (t (mv 0 "")))
                     (cat (num-to-human (first-dgts m n))
                          " "
                          name
                          (cat-if-not-0
                           " "
                           (num-to-human
                            (remove-first-dgts m n))))))))))

(check-expect (num-to-human 10) "ten")
(check-expect (num-to-human 34) "thirty-four")
(check-expect (num-to-human 100) "one hundred")
(check-expect (num-to-human 423) "four hundred twenty-three")
(check-expect (num-to-human 6666) "six thousand six hundred sixty-six")
(check-expect (num-to-human 123456789)
              "one hundred twenty-three million four hundred fifty-six thousand seven hundred eighty-nine")
