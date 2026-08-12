(in-package #:lilylink)

;;; Convert a flat list of events (notes, rests, chords, and command
;;; objects) into a SCORE object with a single staff, splitting events into
;;; measures according to the time signature and bar checks.

(defun step-semitone (step)
  (aref #(0 2 4 5 7 9 11) step))

(defun key-fifths (step alter mode)
  "Circle-of-fifths position (number of sharps/flats) for a key signature.
For minor keys, compute via the relative major."
  (let* ((semi (mod (+ (step-semitone step) alter) 12))
         (semi (if (eq mode :minor) (mod (+ semi 3) 12) semi))
         (f (mod (* 7 semi) 12)))
    (if (<= f 6) f (- f 12))))

(defun measure-length-value (beats beat-type)
  "Length of a measure in whole-note units."
  (/ beats beat-type))

(defun build-score (events)
  (let* ((staff (make-instance 'staff))
         (measures nil)
         (measure-num 1)
         (current nil)
         (accumulated 0)
         (measure-length 1)
         (max-div 0)
         (attrs-dirty t))
    (labels ((open-measure ()
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
             (add-event (ev duration)
               (push ev (measure-events current))
               (incf accumulated (duration-value duration))
               (setf max-div (max max-div (+ (duration-log duration)
                                             (duration-dots duration))))))
      (open-measure)
      (dolist (ev events)
        (etypecase ev
          (time-change
           (setf (staff-time-beats staff) (time-change-beats ev))
           (setf (staff-time-beat-type staff) (time-change-beat-type ev))
           (setf measure-length (measure-length-value (time-change-beats ev)
                                                      (time-change-beat-type ev)))
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
             (close-measure)
             (open-measure)
             (setf accumulated 0)))
          (note
           (add-event ev (note-duration ev)))
          (rest-event
           (add-event ev (rest-duration ev)))
          (chord
           (add-event ev (chord-duration ev))))
        (when (and current (>= accumulated measure-length))
          (close-measure)
          (open-measure)
          (setf accumulated 0)))
      (close-measure)
      (setf (staff-measures staff) (nreverse measures))
      (setf (staff-divisions staff) (if (zerop max-div) 4 (expt 2 max-div)))
      (make-instance 'score :staves (list staff)))))
