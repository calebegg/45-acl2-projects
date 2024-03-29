:set-state-ok t

(defun read-channel (channel limit state)
   (mv-let (ch state)
           (read-char$ channel state)
      (if (or (null ch)
              (zp limit))
          (let ((state (close-input-channel channel state)))
             (mv nil state))
          (mv-let (so-far state)
                  (read-channel channel (1- limit) state)
             (mv (cons ch so-far) state)))))

(defun read-from-file (filename limit state)
   (mv-let (channel state)
           (open-input-channel filename :character state)
      (mv-let (contents state)
              (read-channel channel limit state)
         (mv (coerce contents 'string) state))))

(defun write-channel (channel cs state)
   (if (endp cs)
       (close-output-channel channel state)
       (let ((state (write-byte$ (char-code (first cs))
                                 channel state)))
          (let ((state (write-channel channel
                                      (rest cs)
                                      state)))
              state))))

(defun write-to-file (filename str state)
   (mv-let (channel state)
           (open-output-channel filename :byte state)
      (write-channel channel (coerce str 'list) state)))

(defun copy-file (in out state)
   (mv-let (contents state)
           (read-from-file in (expt 2 40) state)
      (write-to-file out contents state)))