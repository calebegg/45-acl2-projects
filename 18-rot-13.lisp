(include-book "arithmetic-3/top" :dir :system)

(defun char-btn (c low high)
   (and (char>= c low)
        (char<= c high)))

(defun rot-13-cs (cs)
   (cond ((endp cs) nil)
         ((or (char-btn (first cs) #\a #\m)
              (char-btn (first cs) #\A #\M))
          (cons (code-char (+ (char-code (first cs)) 13))
                (rot-13-cs (rest cs))))
         ((or (char-btn (first cs) #\n #\z)
              (char-btn (first cs) #\N #\Z))
          (cons (code-char (- (char-code (first cs)) 13))
                (rot-13-cs (rest cs))))
         (t (cons (first cs) (rot-13-cs (rest cs))))))

(defun rot-13 (s)
   (coerce (rot-13-cs (coerce s 'list)) 'string))

(defthm rot-13-twice-cs
   (implies (character-listp cs)
            (equal (rot-13-cs (rot-13-cs cs)) cs)))

(defthm rot-13-cs-type
   (implies (character-listp cs)
            (character-listp (rot-13-cs cs))))

(defthm rot-13-twice
   (implies (stringp s)
            (equal (rot-13 (rot-13 s)) s)))
