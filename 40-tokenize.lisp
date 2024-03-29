(defun split-at (xs delim)
   (if (or (endp xs) (eql (first xs) delim))
       (mv nil (rest xs))
       (mv-let (before after)
               (split-at (rest xs) delim)
          (mv (cons (first xs) before) after))))

(defun split (xs delim)
   (if (endp xs)
       nil
       (mv-let (before after)
               (split-at xs delim)
          (cons before (split after delim)))))

(defun css->strs (css)
   (if (endp css)
       nil
       (cons (coerce (first css) 'string)
             (css->strs (rest css)))))

(defun split-str (str delim)
   (css->strs (split (coerce str 'list) delim)))

(defun print-with (strs delim)
   (if (endp strs)
       (cw "~%")
       (progn$ (cw (first strs))
               (cw (coerce (list delim) 'string))
               (print-with (rest strs) delim))))

