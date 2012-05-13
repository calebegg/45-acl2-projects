(defun is-curious (n d)
   (or (and (/= (mod d 10) 0)
            (= (mod n 10) (floor d 10))
            (= (/ n d) (/ (floor n 10) (mod d 10))))
       (and (/= (floor d 10) 0)
            (= (floor n 10) (mod d 10))
            (= (/ n d) (/ (mod n 10) (floor d 10))))))

(defun curious-fracs-r2 (limit d)
   (if (zp (- limit 9))
       nil
       (append (if (is-curious limit d)
                   (list (cons limit d))
                   nil)
               (curious-fracs-r2 (1- limit) d))))

(defun curious-fracs-r (limit)
   (if (zp (- limit 9))
       nil
       (append (curious-fracs-r2 (1- limit) limit)
               (curious-fracs-r (1- limit)))))

(defun curious-fracs ()
   (curious-fracs-r 99))
