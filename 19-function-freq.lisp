(in-package "ACL2")

(set-state-ok t)

(defun read-all-objects (limit channel state)
   (mv-let (eof obj state)
           (read-object channel state)
      (if (or eof (zp limit))
          (mv nil state)
          (mv-let (so-far state)
                  (read-all-objects (- limit 1) channel state)
             (mv (cons obj so-far) state)))))

(defun list-starters (xs)
   (cond ((endp xs) nil)
         ((consp (first xs))
          (append (if (symbolp (first (first xs)))
                      (list (first (first xs)))
                      nil)
                  (list-starters (rest (first xs)))
                  (list-starters (rest xs))))
         (t (list-starters (rest xs)))))

(defun invoked-functions (filename state)
   (mv-let (channel state)
           (open-input-channel filename :object state)
      (mv-let (code state)
              (read-all-objects 1000 channel state)
         (mv (list-starters code) state))))

(defun increment-for (key alist)
   (cond ((endp alist) (list (cons key 1)))
         ((equal key (car (first alist)))
          (cons (cons key (1+ (cdr (first alist))))
                (rest alist)))
         (t (cons (first alist)
                  (increment-for key (rest alist))))))

(defun symbol-freq-table (symbols)
   (if (endp symbols)
       nil
       (increment-for (first symbols)
                      (symbol-freq-table (rest symbols)))))

(defun insert-freq-table (pair alist)
   (cond ((endp alist)
          (list pair))
         ((> (cdr pair) (cdr (first alist)))
          (cons pair alist))
         (t (cons (first alist)
                  (insert-freq-table pair (rest alist))))))

(defun isort-freq-table (alist)
   (if (endp alist)
       nil
       (insert-freq-table (first alist)
                          (isort-freq-table (rest alist)))))

(defun main (state)
   (mv-let (fns state)
           (invoked-functions "19-function-freq.lisp" state)
      (mv (take 10 (isort-freq-table
                    (symbol-freq-table fns))) state)))
