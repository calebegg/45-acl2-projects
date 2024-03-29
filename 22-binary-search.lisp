(defun defarray (name size initial-element)
   (cons name
         (compress1 name
                    (cons (list :HEADER
                                :DIMENSIONS (list size)
                                :MAXIMUM-LENGTH (1+ size)
                                :DEFAULT initial-element
                                :NAME name)
                                nil))))

(defconst *dim* 100000)

(defun array-name (array)
   (first array))
       
(defun set-at (array i val)
   (cons (array-name array)
         (aset1 (array-name array)
                (cdr array)
                i
                val)))

(defun populate-array-ordered (array n)
   (if (zp n)
       array
       (populate-array-ordered (set-at array
                                       (- *dim* n)
                                       (- *dim* n))
                               (1- n))))
(include-book "arithmetic-3/top" :dir :system)

(defun binary-search-r (needle haystack low high)
   (declare (xargs :measure (nfix (1+ (- high low)))))
   (let* ((mid (floor (+ low high) 2))
          (current (aref1 (array-name haystack)
                          (cdr haystack)
                          mid)))
         (cond ((not (and (natp low) (natp high))) nil)
               ((= current needle)
                mid)
               ((zp (1+ (- high low))) nil)
               ((> current needle)
                (binary-search-r needle
                                 haystack
                                 low
                                 (1- mid)))
               (t (binary-search-r needle
                                   haystack
                                   (1+ mid)
                                   high)))))

(defun binary-search (needle haystack)
   (binary-search-r needle haystack 0
                    (maximum-length (array-name haystack)
                                    (cdr haystack))))

(defun test-bsearch (needle)
   (binary-search needle
                  (populate-array-ordered
                   (defarray 'haystack *dim* 0)
                   *dim*)))
