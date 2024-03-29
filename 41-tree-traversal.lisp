(defun flatten-preorder (tree)
   (if (endp tree)
       nil
       (append (list (first tree))
               (flatten-preorder (second tree))
               (flatten-preorder (third tree)))))

(defun flatten-inorder (tree)
   (if (endp tree)
       nil
       (append (flatten-inorder (second tree))
               (list (first tree))
               (flatten-inorder (third tree)))))

(defun flatten-postorder (tree)
   (if (endp tree)
       nil
       (append (flatten-postorder (second tree))
               (flatten-postorder (third tree))
               (list (first tree)))))

(defun flatten-level-r1 (tree level levels)
   (if (endp tree)
       levels
       (let ((curr (cdr (assoc level levels))))
            (flatten-level-r1
             (second tree)
             (1+ level)
             (flatten-level-r1
              (third tree)
              (1+ level)
              (put-assoc level
                         (append curr (list (first tree)))
                         levels))))))

(defun flatten-level-r2 (levels max-level)
   (declare (xargs :measure (nfix (1+ max-level))))
   (if (zp (1+ max-level))
       nil
       (append (flatten-level-r2 levels
                                 (1- max-level))
               (reverse (cdr (assoc max-level levels))))))
               

(defun flatten-level (tree)
   (let ((levels (flatten-level-r1 tree 0 nil)))
      (flatten-level-r2 levels (len levels))))

(defconst *tree* '(1 (2 (4 (7 nil nil)
                           nil)
                        (5 nil nil))
                     (3 (6 (8 nil nil) (9 nil nil)) nil)))
