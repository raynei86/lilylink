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
         (measure-hash (make-hash-table))
         (measures nil)                    ; ordered, in creation order
         (measure-cap (measure-units 4 4 divisions))
         (attrs-dirty t)
         (cursors (make-hash-table)))      ; voice -> (measure-number . accumulated)
    (labels
        ((voice-cursor (voice)
           (or (gethash voice cursors)
               (setf (gethash voice cursors) (cons 1 0))))

         (ensure-measure (n)
           "Create or reuse measure N, snapshoting attributes on first use."
           (or (gethash n measure-hash)
               (let ((m (make-instance 'measure :number n :attributes attrs-dirty)))
                 (when attrs-dirty
                   (setf (measure-attr-data m)
                         (list :clef (staff-clef staff)
                               :clef-octave-shift (staff-clef-octave-shift staff)
                               :key-fifths (staff-key-fifths staff)
                               :key-mode (staff-key-mode staff)
                               :time-beats (staff-time-beats staff)
                               :time-beat-type (staff-time-beat-type staff)))
                   (setf attrs-dirty nil))
                 (setf (gethash n measure-hash) m)
                 (setf measures (append measures (list m)))
                 m)))

         (mark-attrs ()
           (setf attrs-dirty t))

         (push-event (ev duration voice)
           (let* ((cursor (voice-cursor voice))
                  (m (ensure-measure (car cursor))))
             (push ev (measure-events m))
             (incf (cdr cursor) (duration-units duration divisions))))

         (advance-voice (voice)
           (let ((cursor (voice-cursor voice)))
             (incf (car cursor))
             (setf (cdr cursor) 0)))

         (flush-voice (voice)
           (when (>= (cdr (voice-cursor voice)) measure-cap)
             (advance-voice voice)))

         ;; Place a run of dyadic-dotted pieces of one pitch as tied notes.
         ;; FIRST-ATTS go on the first piece, LAST-ATTS (spanner stops) on the
         ;; last.
         (place-note-pieces (pitch pieces first-p last-p src-start src-stop
                                  first-atts last-atts voice)
           (let ((np (length pieces)))
             (loop for piece in pieces
                   for j from 0
                   do (destructuring-bind (log . dots) piece
                        (let* ((has-prev (not (and first-p (zerop j))))
                               (has-next (not (and last-p (= (1+ j) np))))
                               (dur (make-duration log :dots dots))
                               (note (make-instance 'note :pitch pitch :duration dur
                                                    :voice voice)))
                          (when (or has-prev (and first-p (zerop j) src-stop))
                            (setf (note-tie-stop-p note) t))
                          (when (or has-next (and last-p (= (1+ j) np) src-start))
                            (setf (note-tie-start-p note) t))
                          (when (and first-p (zerop j) first-atts)
                            (setf (event-attachments note) first-atts))
                          (when (and last-p (= (1+ j) np) last-atts)
                            (setf (event-attachments note) last-atts))
                          (push-event note dur voice))))))

         (place-note (note voice)
           (let* ((value (duration-units (note-duration note) divisions))
                  (remaining (- measure-cap (cdr (voice-cursor voice))))
                  (chunks (measure-chunks value remaining measure-cap)))
             (if (null (rest chunks))
                 (push-event note (note-duration note) voice)
                 (let ((pitch (note-pitch note))
                       (src-start (note-tie-start-p note))
                       (src-stop (note-tie-stop-p note))
                       (atts (event-attachments note))
                       (nchunks (length chunks)))
                   (loop for chunk in chunks
                         for i from 0
                         do (place-note-pieces pitch
                                               (decompose-units chunk divisions)
                                               (zerop i)
                                               (= (1+ i) nchunks)
                                               src-start src-stop
                                               (remove-if #'attachment-stop-p atts)
                                               (remove-if-not #'attachment-stop-p atts)
                                               voice)
                            (unless (= (1+ i) nchunks)
                              (advance-voice voice)))))))

         (place-rest (rest-event voice)
           (let* ((value (duration-units (rest-duration rest-event) divisions))
                  (remaining (- measure-cap (cdr (voice-cursor voice))))
                  (chunks (measure-chunks value remaining measure-cap)))
             (if (null (rest chunks))
                 (push-event rest-event (rest-duration rest-event) voice)
                 (loop for chunk in chunks
                       for i from 0
                       do (dolist (piece (decompose-units chunk divisions))
                            (destructuring-bind (log . dots) piece
                              (let* ((dur (make-duration log :dots dots))
                                     (r (make-rest dur)))
                                (setf (rest-voice r) voice)
                                (push-event r dur voice))))
                          (unless (= (1+ i) (length chunks))
                            (advance-voice voice))))))

         (place-chord (chord voice)
           (let* ((value (duration-units (chord-duration chord) divisions))
                  (remaining (- measure-cap (cdr (voice-cursor voice))))
                  (chunks (measure-chunks value remaining measure-cap)))
             (if (null (rest chunks))
                 (push-event chord (chord-duration chord) voice)
                 (let ((notes (chord-notes chord))
                       (atts (event-attachments chord))
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
                                                                                      :duration dur
                                                                                      :voice voice)))
                                                              (when (or has-prev
                                                                        (and (zerop i) (note-tie-stop-p n)))
                                                                (setf (note-tie-stop-p sub) t))
                                                              (when (or has-next
                                                                        (and (= (1+ i) nchunks)
                                                                             (note-tie-start-p n)))
                                                                (setf (note-tie-start-p sub) t))
                                                              sub))
                                                          notes)))
                                           (let ((sub-chord (make-chord sub-notes dur)))
                                             (setf (chord-voice sub-chord) voice)
                                             ;; Marks go on the first split chord; spanner
                                             ;; stops on the last.
                                             (cond
                                               ((and (zerop i) (zerop j))
                                                (setf (event-attachments sub-chord)
                                                      (remove-if #'attachment-stop-p atts)))
                                               ((and (= (1+ i) nchunks)
                                                     (= (1+ j) (length pieces)))
                                                (setf (event-attachments sub-chord)
                                                      (remove-if-not #'attachment-stop-p atts))))
                                             (push-event sub-chord dur voice)))))
                              (unless (= (1+ i) nchunks)
                                (advance-voice voice)))))))))
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
           (when (plusp (cdr (voice-cursor (barline-voice ev))))
             (advance-voice (barline-voice ev))))
          (note (place-note ev (note-voice ev)))
          (rest-event (place-rest ev (rest-voice ev)))
          (chord (place-chord ev (chord-voice ev))))
        (typecase ev
          (note (flush-voice (note-voice ev)))
          (rest-event (flush-voice (rest-voice ev)))
          (chord (flush-voice (chord-voice ev)))
          (t nil)))
      (dolist (m measures)
        (setf (measure-events m) (nreverse (measure-events m))))
      (setf (staff-measures staff) measures)
      (setf (staff-divisions staff) divisions)
      (make-instance 'score :staves (list staff)))))
