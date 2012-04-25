(in-package "ACL2")

:set-state-ok t

(defun print-str-list (xs)
   (if (endp xs)
       nil
       (progn$ (cw (first xs))
               (cw "<br />~%")
               (print-str-list (rest xs)))))

;; Get guestbook entries from a file (read the first object,
;; which is a list of strings)
(defun get-entries (fn state)
   (mv-let (channel state)
           (open-input-channel fn :object state)
      (mv-let (eofp obj state)
              (read-object channel state)
         (declare (ignore eofp))
         (mv obj state))))

(defun save-entries (fn entries state)
   (mv-let (channel state)
           (open-output-channel fn :object state)
      (print-object$ entries channel state)))

;; Parse a query string into an alist
(defun parse-qstring-r (cs keyorval key val)
   (cond ((endp cs) (list (cons (coerce (reverse key) 'string)
                                (coerce (reverse val) 'string))))
         ((equal (first cs) #\=) ; Switch to a value
          (parse-qstring-r (rest cs) 'val key nil))
         ((equal (first cs) #\&) ; Finished a pair
          (cons (cons (coerce (reverse key) 'string)
                      (coerce (reverse val) 'string))
                (parse-qstring-r (rest cs) 'key nil nil)))
         ((eq keyorval 'key) ; Continue reading key
          (parse-qstring-r (rest cs) 'key
                           (cons (first cs) key) val))
         ((eq keyorval 'val) ; Continue reading value
          (parse-qstring-r (rest cs) 'val
                           key (cons (first cs) val)))))

(defun parse-qstring (str)
   (parse-qstring-r (coerce str 'list) 'key nil nil))

(defun main (state)
   (mv-let (entries state)
           (get-entries "gb.txt" state)
      (mv-let (err qstring state)
              (getenv$ "QUERY_STRING" state)
         (declare (ignore err))
         (if (search "sign" qstring)
             (let ((state
                    (save-entries "gb.txt"
                                  (cons (cdr
                                         (assoc "name"
                                                (parse-qstring qstring)
                                                :test 'equal))
                                        entries) state)))
                  (mv (progn$ (cw "Status: 302 Moved~%")
                              (cw "Location: gb.cgi~%~%"))
                      state))
             (mv (progn$ (cw "Content-type: text/html~%~%")
                         (cw "Sign my guestbook:<br />~%")
                         (cw "<form action=\"gb.cgi\">")
                         (cw "Name: <input type=\"text\" name=\"name\" />")
                         (cw "<input type=\"hidden\" name=\"sign\" value=\"true\" />")
                         (cw "<input type=submit />")
                         (cw "</form>")
                         (print-str-list entries))
                 state)))))
