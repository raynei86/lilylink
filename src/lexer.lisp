(in-package #:lilylink)

(defstruct token type value line col)

;;; A pitch token's value: STEP is a diatonic index 0..6 (c=0), ALTER the
;;; accidental (-2..2), OCTAVE-MARK the net ' / , count, and DURATION an
;;; adjacent (LOG . DOTS) note value or NIL.
(defstruct (pitch-token (:constructor make-pitch-token (step alter octave-mark duration)))
  step
  alter
  octave-mark
  duration)

(defun lilylink-error-at (line col fmt &rest args)
  (error 'lilylink-parse-error
         :message (apply #'format nil fmt args)
         :line line :col col))

(defun duration-log-from-num (num)
  "Log2 of NUM, which must be a positive power of two no longer than
+MAX-DURATION-LOG+ (i.e. at most a 1024th note)."
  (unless (and (integerp num) (plusp num))
    (lilylink-error-at 0 0 "Invalid duration number ~S" num))
  (let ((log (integer-length (1- num))))
    (unless (= num (expt 2 log))
      (lilylink-error-at 0 0 "Invalid duration number ~S" num))
    (when (> log +max-duration-log+)
      (lilylink-error-at 0 0 "Unsupported duration ~S (longest supported note is 2^~D)"
                         num +max-duration-log+))
    log))

(defun tokenize (string)
  "Tokenize a LilyPond source string into a list of TOKEN structs."
  (let ((len (length string))
        (pos 0)
        (line 1)
        (col 1)
        (tok-line 1)
        (tok-col 1)
        (tokens nil))
    (labels ((peekc ()
               (when (< pos len) (char string pos)))
             (peekc2 ()
               (when (< (1+ pos) len) (char string (1+ pos))))
             (eofp ()
               (>= pos len))
             (consume ()
               (let ((c (char string pos)))
                 (incf pos)
                 (if (char= c #\Newline)
                     (progn (incf line) (setf col 1))
                     (incf col))
                 c))
             ;; Tokens always start at the beginning of a loop iteration, so
             ;; push-tok reports the position captured there, not where the
             ;; token finished being scanned.
             (push-tok (type value)
               (push (make-token :type type :value value
                                 :line tok-line :col tok-col)
                     tokens))
             (err (fmt &rest args)
               (lilylink-error-at tok-line tok-col fmt args))
             (skip-comment ()
               (consume)  ; the %
               (if (and (peekc) (char= (peekc) #\{))
                   (progn
                     (consume)
                     (loop until (or (eofp)
                                     (and (peekc) (char= (peekc) #\%)
                                          (peekc2) (char= (peekc2) #\})))
                           do (consume))
                     (unless (eofp) (consume) (consume)))
                   (loop until (or (eofp) (char= (peekc) #\Newline))
                         do (consume))))
             (scan-digits-and-dots ()
               ;; Returns (values num dots) if a digit follows, else NIL.
               (when (and (peekc) (digit-char-p (peekc)))
                 (let ((digits nil)
                       (dots 0))
                   (loop while (and (peekc) (digit-char-p (peekc)))
                         do (push (consume) digits))
                   (loop while (and (peekc) (char= (peekc) #\.))
                         do (consume) (incf dots))
                   (values (parse-integer (coerce (nreverse digits) 'string))
                           dots))))
             (scan-number ()
               (multiple-value-bind (num dots) (scan-digits-and-dots)
                 (push-tok :number (cons num dots))))
             (note-parts (run)
               ;; Returns (values step alter fullp) for a letter run, or nil.
               (let ((first (char run 0)))
                 (if (and (char<= #\a first #\g))
                     (let ((step (position first "cdefgab"))
                           (alter 0)
                           (p 1)
                           (n (length run)))
                       (loop while (< p n)
                             do (cond
                                  ((and (< (1+ p) n)
                                        (char= (char run p) #\i)
                                        (char= (char run (1+ p)) #\s))
                                   (incf alter) (incf p 2))
                                  ((and (< (1+ p) n)
                                        (char= (char run p) #\e)
                                        (char= (char run (1+ p)) #\s))
                                   (decf alter) (incf p 2))
                                  (t (return))))
                       (values step alter (= p n)))
                     (values nil nil nil))))
             (scan-duration-parts ()
               ;; Called when a digit follows a pitch/rest with no whitespace.
               ;; Returns (LOG . DOTS) or NIL without emitting a token.
               (multiple-value-bind (num dots) (scan-digits-and-dots)
                 (when num
                   (cons (duration-log-from-num num) dots))))
             (scan-note-or-rest (run)
               ;; After the letter run, octave marks, !/?, and adjacent duration.
               (let ((c (char run 0)))
                 (cond
                   ((and (member c '(#\r #\s)) (= (length run) 1))
                    (push-tok :rest (scan-duration-parts)))
                   (t
                    (multiple-value-bind (step alter fullp) (note-parts run)
                      (if (and step fullp)
                          (let ((mark 0))
                            (loop while (and (peekc) (member (peekc) '(#\' #\,)))
                                  do (if (char= (consume) #\')
                                         (incf mark)
                                         (decf mark)))
                            ;; consume reminder/cautionary accidentals, ignore
                            (loop while (and (peekc) (member (peekc) '(#\! #\?)))
                                  do (consume))
                            (push-tok :pitch
                                      (make-pitch-token step alter mark
                                                        (scan-duration-parts))))
                          (push-tok :word run)))))))
             (scan-word (run)
               (push-tok :word run))
             (scan-word-or-note ()
               (let ((run (with-output-to-string (s)
                            (loop while (and (peekc) (alpha-char-p (peekc)))
                                  do (write-char (consume) s)))))
                 (cond
                   ((or (and (char<= #\a (char run 0) #\g))
                        (member (char run 0) '(#\r #\s)))
                    (scan-note-or-rest run))
                   (t (scan-word run)))))
              (scan-command ()
               (consume)  ; the backslash
               (let ((c (peekc)))
                 (if (and c (alpha-char-p c))
                     (let ((name (with-output-to-string (s)
                                   (loop while (and (peekc) (alpha-char-p (peekc)))
                                         do (write-char (consume) s)))))
                       (push-tok :command (intern (string-upcase name) "KEYWORD")))
                     (case c
                       (#\( (consume) (push-tok :phrase-open nil))
                       (#\) (consume) (push-tok :phrase-close nil))
                       (#\< (consume) (push-tok :wedge-start :crescendo))
                       (#\> (consume) (push-tok :wedge-start :diminuendo))
                       (#\! (consume) (push-tok :wedge-stop nil))
                       (#\\ (consume) (push-tok :voice-separator nil))
                       (t (err "Expected a command name after \\"))))))
             (scan-string ()
               (consume)  ; opening quote
               (let ((str (with-output-to-string (s)
                            (loop while (and (peekc) (not (char= (peekc) #\")))
                                  do (let ((c (consume)))
                                       (if (char= c #\\)
                                           (write-char (consume) s)
                                           (write-char c s)))))))
                 (when (eofp)
                   (err "Unterminated string"))
                 (consume)  ; closing quote
                 (push-tok :string str)))
             (scan-dash ()
               ;; Articulation shorthands (->, -., -^, -!, -+, --, -_), or a
               ;; general dash attachment prefix (- followed by a command).
               (consume)  ; the -
               (case (peekc)
                 (#\> (consume) (push-tok :articulation :accent))
                 (#\. (consume) (push-tok :articulation :staccato))
                 (#\^ (consume) (push-tok :articulation :marcato))
                 (#\! (consume) (push-tok :articulation :staccatissimo))
                 (#\+ (consume) (push-tok :articulation :stopped))
                 (#\- (consume) (push-tok :articulation :tenuto))
                 (#\_ (consume) (push-tok :articulation :portato))
                 (t (push-tok :attach-dash nil)))))
      (loop
        (when (eofp) (return))
        (let ((c (peekc)))
          (setf tok-line line tok-col col)
          (cond
            ((or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return))
             (consume))
            ((char= c #\%) (skip-comment))
            ((char= c #\{) (consume) (push-tok :brace-open nil))
            ((char= c #\}) (consume) (push-tok :brace-close nil))
            ((char= c #\<)
             (if (and (peekc2) (char= (peekc2) #\<))
                 (progn (consume) (consume) (push-tok :simult-open nil))
                 (progn (consume) (push-tok :chord-open nil))))
            ((char= c #\>)
             (if (and (peekc2) (char= (peekc2) #\>))
                 (progn (consume) (consume) (push-tok :simult-close nil))
                 (progn (consume) (push-tok :chord-close nil))))
            ((char= c #\() (consume) (push-tok :slur-open nil))
            ((char= c #\)) (consume) (push-tok :slur-close nil))
            ((char= c #\|) (consume) (push-tok :barline nil))
            ((char= c #\/) (consume) (push-tok :slash nil))
            ((char= c #\~) (consume) (push-tok :tie nil))
            ((char= c #\-) (scan-dash))
            ((char= c #\^) (consume) (push-tok :attach-up nil))
            ((char= c #\_) (consume) (push-tok :attach-down nil))
            ((char= c #\\) (scan-command))
            ((char= c #\") (scan-string))
            ((digit-char-p c) (scan-number))
            ((alpha-char-p c) (scan-word-or-note))
            (t (consume) (push-tok :other (string c))))))
      (nreverse tokens))))
