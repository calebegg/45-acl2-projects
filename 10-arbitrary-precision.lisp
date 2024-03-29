(in-package "ACL2")

(include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)

(set-print-length 0 state)

(defun arbitrary-precision ()
   (declare (xargs :mode :program))
   (let* ((x (expt 5 (expt 4 (expt 3 2))))
          (s (mv-let (col str)
                     (fmt1-to-string "~xx" 
                                     (list (cons #\x x))
                                     0)
                (declare (ignore col))
                str)))
         (cw "~s0 ... ~x1 (~x2 digits)~%"
             (subseq s 1 21)
             (mod x (expt 10 20))
             (1- (length s)))))
