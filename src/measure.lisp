(in-package #:lilylink)

;;; Convert a flat list of events (notes, rests, chords, and command
;;; objects) into a SCORE object with a single staff, splitting events into
;;; measures according to the time signature and bar checks.  Notes, rests,
;;; and chords that would overflow a measure are split at the barline into
;;; tied dyadic-dotted pieces.

(defun step-semitone (step)
  (aref #(0 2 4 5 7 9 11) step))

(defun key-fifths (step alter mode)
  "Circle-of-fifths position (number of sharps/flats) for a key signature.
For minor keys, compute via the relative major."
  (let* ((semi (mod (+ (step-semitone step) alter) 12))
         (semi (if (eq mode :minor) (mod (+ semi 3) 12) semi))
         (f (mod (* 7 semi) 12)))
    (if (<= f 6) f (- f 12))))

(defun beat-type-log (beat-type)
  (integer-length (1- beat-type)))

(defun scan-divisions-log (events)
  "Largest (log + dots) over all durations, raised to also make every
measure length integral in division units (i.e. 4*2^log divisible by each
\\time beat-type)."
  (let ((log 0))
    (dolist (ev events log)
      (etypecase ev
        (time-change
         (setf log (max log (- (beat-type-log (time-change-beat-type ev)) 2))))
        (note
         (setf log (max log (+ (duration-log (note-duration ev))
                               (duration-dots (note-duration ev))))))
        (rest-event
         (setf log (max log (+ (duration-log (rest-duration ev))
                               (duration-dots (rest-duration ev))))))
        (chord
         (setf log (max log (+ (duration-log (chord-duration ev))
                               (duration-dots (chord-duration ev))))))
        (key-change nil)
        (clef-change nil)
        (barline nil)))))

(defun build-score (events)
  (let* ((divisions (let ((log (scan-divisions-log events)))
                      (if (zerop log) 4 (expt 2 log))))
         (staff (make-instance 'staff))
         (measures nil)
         (measure-num 1)
         (current nil)
         (accumulated 0)                    ; division units
         (measure-cap (measure-units 4 4 divisions))
         (attrs-dirty t))
    (labels
        ((open-measure ()
           (setf current (make-instance 'measure :number measure-num
                                        :attributes attrs-dirty))
           (when attrs-dirty (snapshot-attrs))
           (setf attrs-dirty nil))

         (snapshot-attrs ()
           (setf (measure-attr-data current)
                 (list :clef (staff-clef staff)
                       :clef-octave-shift (staff-clef-octave-shift staff)
                       :key-fifths (staff-key-fifths staff)
                       :key-mode (staff-key-mode staff)
                       :time-beats (staff-time-beats staff)
                       :time-beat-type (staff-time-beat-type staff))))

         (close-measure ()
           (when (and current (measure-events current))
             (setf (measure-events current) (nreverse (measure-events current)))
             (push current measures))
           (setf current nil)
           (incf measure-num))

         (mark-attrs ()
           (setf attrs-dirty t)
           (when (and current (null (measure-events current)))
             (setf (measure-attributes current) t)
             (snapshot-attrs)
             (setf attrs-dirty nil)))

         (push-event (ev duration)
           (push ev (measure-events current))
           (incf accumulated (duration-units duration divisions)))

         (next-measure ()
           (close-measure)
           (open-measure)
           (setf accumulated 0))

         ;; Place a run of dyadic-dotted pieces of one pitch as tied notes.
         (place-note-pieces (pitch pieces first-p last-p src-start src-stop)
           (let ((np (length pieces)))
             (loop for piece in pieces
                   for j from 0
                   do (destructuring-bind (log . dots) piece
                        (let* ((has-prev (not (and first-p (zerop j))))
                               (has-next (not (and last-p (= (1+ j) np))))
                               (dur (make-duration log :dots dots))
                               (note (make-instance 'note :pitch pitch :duration dur)))
                          (when (or has-prev (and first-p (zerop j) src-stop))
                            (setf (note-tie-stop-p note) t))
                          (when (or has-next (and last-p (= (1+ j) np) src-start))
                            (setf (note-tie-start-p note) t))
                          (push-event note dur))))))

         (place-note (note)
           (let* ((value (duration-units (note-duration note) divisions))
                  (remaining (- measure-cap accumulated))
                  (chunks (measure-chunks value remaining measure-cap)))
             (if (null (rest chunks))
                 (push-event note (note-duration note))
                 (let ((pitch (note-pitch note))
                       (src-start (note-tie-start-p note))
                       (src-stop (note-tie-stop-p note))
                       (nchunks (length chunks)))
                   (loop for chunk in chunks
                         for i from 0
                         do (place-note-pieces pitch
                                               (decompose-units chunk divisions)
                                               (zerop i)
                                               (= (1+ i) nchunks)
                                               src-start src-stop)
                            (unless (= (1+ i) nchunks)
                              (next-measure)))))))

         (place-rest (rest-event)
           (let* ((value (duration-units (rest-duration rest-event) divisions))
                  (remaining (- measure-cap accumulated))
                  (chunks (measure-chunks value remaining measure-cap)))
             (if (null (rest chunks))
                 (push-event rest-event (rest-duration rest-event))
                 (loop for chunk in chunks
                       for i from 0
                       do (dolist (piece (decompose-units chunk divisions))
                            (destructuring-bind (log . dots) piece
                              (let ((dur (make-duration log :dots dots)))
                                (push-event (make-rest dur) dur))))
                          (unless (= (1+ i) (length chunks))
                            (next-measure))))))

         (place-chord (chord)
           (let* ((value (duration-units (chord-duration chord) divisions))
                  (remaining (- measure-cap accumulated))
                  (chunks (measure-chunks value remaining measure-cap)))
             (if (null (rest chunks))
                 (push-event chord (chord-duration chord))
                 (let ((notes (chord-notes chord))
                       (nchunks (length chunks)))
                   (loop for chunk in chunks
                         for i from 0
                         do (let ((pieces (decompose-units chunk divisions)))
                              (loop for piece in pieces
                                    for j from 0
                                    do (destructuring-bind (log . dots) piece
                                         (let* ((has-prev (not (and (zerop i) (zerop j))))
                                                (has-next (not (and (= (1+ i) nchunks)
                                                                    (= (1+ j) (length pieces)))))
                                                (dur (make-duration log :dots dots))
                                                (sub-notes
                                                  (mapcar (lambda (n)
                                                            (let ((sub (make-instance 'note
                                                                                      :pitch (note-pitch n)
                                                                                      :duration dur)))
                                                              (when (or has-prev
                                                                        (and (zerop i) (note-tie-stop-p n)))
                                                                (setf (note-tie-stop-p sub) t))
                                                              (when (or has-next
                                                                        (and (= (1+ i) nchunks)
                                                                             (note-tie-start-p n)))
                                                                (setf (note-tie-start-p sub) t))
                                                              sub))
                                                          notes)))
                                           (push-event (make-chord sub-notes dur) dur))))
                              (unless (= (1+ i) nchunks)
                                (next-measure)))))))))
      (open-measure)
      (dolist (ev events)
        (etypecase ev
          (time-change
           (setf (staff-time-beats staff) (time-change-beats ev))
           (setf (staff-time-beat-type staff) (time-change-beat-type ev))
           (setf measure-cap (measure-units (time-change-beats ev)
                                            (time-change-beat-type ev)
                                            divisions))
           (mark-attrs))
          (key-change
           (let ((p (key-change-pitch ev)))
             (setf (staff-key-fifths staff)
                   (key-fifths (pitch-step p) (pitch-alter p) (key-change-mode ev)))
             (setf (staff-key-mode staff) (key-change-mode ev)))
           (mark-attrs))
          (clef-change
           (setf (staff-clef staff) (clef-change-clef ev))
           (setf (staff-clef-octave-shift staff) (clef-change-octave-shift ev))
           (mark-attrs))
          (barline
           (when (measure-events current)
             (next-measure)))
          (note (place-note ev))
          (rest-event (place-rest ev))
          (chord (place-chord ev)))
        (when (and current (>= accumulated measure-cap))
          (next-measure)))
      (close-measure)
      (setf (staff-measures staff) (nreverse measures))
      (setf (staff-divisions staff) divisions)
      (make-instance 'score :staves (list staff)))))
