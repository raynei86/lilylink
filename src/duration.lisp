(in-package #:lilylink)

;;; Arithmetic on note values in integer "division units": one quarter note
;;; equals DIVISIONS units and one whole note equals 4*DIVISIONS units.  This
;;; is used to split notes that cross a measure boundary into tied dyadic-dotted
;;; pieces.

(defun duration-units (duration divisions)
  "Duration in division units (integer)."
  (round (* 4 divisions (duration-value duration))))

(defun measure-units (beats beat-type divisions)
  "Length of a measure in division units (integer)."
  (round (* 4 divisions (/ beats beat-type))))

(defun measure-chunks (units remaining cap)
  "Split UNITS (division units) into a list of chunk sizes that fit across
measures: the first chunk is limited to REMAINING units in the current measure,
every later chunk to CAP units (one full measure)."
  (iter (with chunks = nil)
        (with u = units)
        (with c = remaining)
        (while (> u 0))
        (let ((piece (min u c)))
          (push piece chunks)
          (decf u piece)
          (setf c cap))
        (finally (return (nreverse chunks)))))

(defun largest-dyadic-dotted-piece (units divisions)
  "Largest dyadic-dotted note value <= UNITS (division units).
Returns (values log dots piece-units)."
  (let ((whole (* 4 divisions))
        (max-k (1- (integer-length (* 4 divisions))))
        (best-log 0)
        (best-dots 0)
        (best-units 0))
    (iter (for k from 0 to max-k)
          (iter (for d from 0 to (min 4 k))
                (let* ((numer (1- (expt 2 (1+ d))))
                       (val (floor (* whole numer) (expt 2 k))))
                  (when (and (<= val units) (> val best-units))
                    (setf best-units val
                          best-log (- k d)
                          best-dots d)))))
    (when (zerop best-units)
      (emit-error "Cannot represent ~D division units as a note value" units))
    (values best-log best-dots best-units)))

(defun decompose-units (units divisions)
  "Decompose UNITS (division units) into a list of (LOG . DOTS) dyadic-dotted
notes summing to UNITS, greedy largest-first."
  (let ((pieces nil)
        (u units))
    (iter (while (> u 0))
          (multiple-value-bind (log dots piece)
              (largest-dyadic-dotted-piece u divisions)
            (push (cons log dots) pieces)
            (decf u piece))
          (finally (return (nreverse pieces))))))
