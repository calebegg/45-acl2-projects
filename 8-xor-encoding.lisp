(defun cs->codes (cs)
   (if (endp cs)
       nil
       (cons (char-code (first cs))
             (cs->codes (rest cs)))))

(defun str->codes (str)
   (cs->codes (coerce str 'list)))

(defun codes->cs (codes)
   (if (endp codes)
       nil
       (cons (code-char (first codes))
             (codes->cs (rest codes)))))

(defun codes->str (codes)
   (coerce (codes->cs codes) 'string))

(defun cycle (part orig)
   (if (endp (rest part))
       orig
       (rest part)))

(defun encode-r (part key msg)
   (if (endp msg)
       nil
       (cons (logxor (first part) (first msg))
             (encode-r (cycle part key) key (rest msg)))))

(defun encode (key str)
   (encode-r (str->codes key)
             (str->codes key)
             (str->codes str)))

(defun decode (key codes)
   (codes->str (encode-r (str->codes key)
                         (str->codes key)
                         codes)))
