(defun is-triangle-num-r (x i)
   (declare (xargs :measure (nfix x)))
   (cond ((zp i) nil)
         ((< x 0) nil)
         ((zp x) t)
         (t (is-triangle-num-r (- x i) (1+ i)))))

(defun is-triangle-num (x)
   (is-triangle-num-r x 1))

(defun word-num-r (cs)
   (if (endp cs)
       0
       (+ 1 (- (char-code (first cs))
               (char-code #\A))
          (word-num-r (rest cs)))))

(defun word-num (str)
   (word-num-r (coerce str 'list)))

(defun count-triangle-words (words)
   (if (endp words)
       0
       (+ (if (is-triangle-num (word-num (first words)))
              1
              0)
          (count-triangle-words (rest words)))))

:set-state-ok t
(defun main (state)
   (mv-let (channel state)
           ;; words has been modified: commas replaced with
           ;; spaces and ( and ) added to beginning and end
           (open-input-channel "words.txt" :object state)
      (mv-let (eofp obj state)
              (read-object channel state)
         (declare (ignore eofp))
         (mv (count-triangle-words obj) state))))
