(defun parse-code-r (cs stack)
   (cond ((endp cs) (first stack))
         ((eql (first cs) #\[)
          (parse-code-r (rest cs) (cons nil stack)))
         ((eql (first cs) #\])
          (parse-code-r (rest cs)
                        (cons (cons (reverse (first stack))
                              (second stack))
                        (rest (rest stack)))))
         (t (parse-code-r (rest cs)
                        (cons (cons (first cs)
                                    (first stack))
                              (rest stack))))))

(defun parse-code (str)
   (reverse (parse-code-r (coerce str 'list) '(nil))))

(defun execute-bf-r (code block left curr right input limit)
   (cond ((zp limit) (list left curr right input))
         ((and (endp code) (consp block))
          (if (= curr 0)
              (list left curr right input)
              (execute-bf-r block block left
                            curr right input (1- limit))))
         ((endp code)
          (list left curr right input))
         ((consp (first code))
          (let ((tape (execute-bf-r (first code)
                                    (first code)
                                    left curr
                                    right input (1- limit))))
             (execute-bf-r (rest code)
                           block
                           (first tape)
                           (second tape)
                           (third tape)
                           (fourth tape) (1- limit))))
         ((eql (first code) #\<)
          (if (endp left) ; Ran out of tape
              (execute-bf-r (rest code) block
                            nil 0 (cons curr right) input (1- limit))
              (execute-bf-r (rest code) block
                            (rest left) (first left)
                            (cons curr right) input (1- limit))))
         ((eql (first code) #\>)
          (if (endp right) ; Ran out of tape
              (execute-bf-r (rest code) block
                            (cons curr left) 0 nil input (1- limit))
              (execute-bf-r (rest code) block
                            (cons curr left) (first right)
                            (rest right) input (1- limit))))
         ((eql (first code) #\-)
          (execute-bf-r (rest code) block
                        left (1- curr) right input (1- limit)))
         ((eql (first code) #\+)
          (execute-bf-r (rest code) block
                        left (1+ curr) right input (1- limit)))
         ((eql (first code) #\.) ; Write output as char
          (prog2$ (cw (coerce (list (code-char curr))
                              'string))
                  (execute-bf-r (rest code) block
                                left curr right input (1- limit))))
         ((eql (first code) #\,) ; Read input to tape
          (execute-bf-r (rest code) block left
                        (if (endp input)
                            0
                            (char-code (first input)))
                        right (rest input) (1- limit)))
         (t (execute-bf-r (rest code) block left
                          curr right input (1- limit)))))

(defun execute-bf (str input)
   (prog2$ (cw "~%")
           (execute-bf-r (parse-code str) nil nil 0 nil
                         (coerce input 'list) 10000)))

