
:set-state-ok t

(defun array-swap (name array i j)
   (let ((ai (aref1 name array i))
         (aj (aref1 name array j)))
      (aset1 name
             (aset1 name array j ai)
             i aj)))

(defun shuffle-r (name array m state)
   (if (zp m)
       (mv array state)
       (mv-let (i state)
               (random$ m state)
          (shuffle-r name
                     (array-swap name array i m)
                     (1- m)
                     state))))

(defun shuffle (name array state)
   (shuffle-r name
              array
              (1- (first (dimensions name array)))
              state))
