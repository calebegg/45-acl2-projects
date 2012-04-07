(defun cat (a b) (concatenate 'string a b))

(defun hamming-seq-r (so-far n h)
   (if (zp n)
       (list so-far)
       (append
        (if (> (- n h) 0)
            (hamming-seq-r (cat so-far "0") (- n 1) h)
            nil)
        (if (> h 0)
            (hamming-seq-r (cat so-far "1") (- n 1) (- h 1))
            nil))))

(defun hamming-seq (n h)
   (hamming-seq-r "" n h))
