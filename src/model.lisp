(in-package #:lilylink)

(defclass pitch ()
  ((step :initarg :step :accessor pitch-step)      ; 0..6, c=0 d=1 ... b=6
   (alter :initarg :alter :initform 0 :accessor pitch-alter)
   (octave :initarg :octave :accessor pitch-octave)))

(defclass duration ()
  ((log :initarg :log :accessor duration-log)      ; log2 of reciprocal, 0=whole
   (dots :initarg :dots :initform 0 :accessor duration-dots)))

(defclass note ()
  ((pitch :initarg :pitch :accessor note-pitch)
   (duration :initarg :duration :accessor note-duration)
   (tie-start :initarg :tie-start :initform nil :accessor note-tie-start-p)
   (tie-stop :initarg :tie-stop :initform nil :accessor note-tie-stop-p)))

(defclass rest-event ()
  ((duration :initarg :duration :accessor rest-duration)))

(defclass chord ()
  ((notes :initarg :notes :accessor chord-notes)
   (duration :initarg :duration :accessor chord-duration)))

(defclass time-change ()
  ((beats :initarg :beats :accessor time-change-beats)
   (beat-type :initarg :beat-type :accessor time-change-beat-type)))

(defclass key-change ()
  ((pitch :initarg :pitch :accessor key-change-pitch)
   (mode :initarg :mode :accessor key-change-mode)))

(defclass clef-change ()
  ((clef :initarg :clef :accessor clef-change-clef)
   (octave-shift :initarg :octave-shift :initform 0 :accessor clef-change-octave-shift)))

(defclass barline () ())

(defclass measure ()
  ((number :initarg :number :accessor measure-number)
   (events :initform nil :accessor measure-events)
   (attributes :initarg :attributes :initform nil :accessor measure-attributes)
   (attr-data :initform nil :accessor measure-attr-data)))

(defclass staff ()
  ((clef :initarg :clef :initform :treble :accessor staff-clef)
   (clef-octave-shift :initarg :clef-octave-shift :initform 0 :accessor staff-clef-octave-shift)
   (key-fifths :initarg :key-fifths :initform 0 :accessor staff-key-fifths)
   (key-mode :initarg :key-mode :initform :major :accessor staff-key-mode)
   (time-beats :initarg :time-beats :initform 4 :accessor staff-time-beats)
   (time-beat-type :initarg :time-beat-type :initform 4 :accessor staff-time-beat-type)
   (divisions :initarg :divisions :initform 4 :accessor staff-divisions)
   (measures :initarg :measures :initform nil :accessor staff-measures)))

(defclass score ()
  ((staves :initarg :staves :accessor score-staves)))

(defun make-pitch (step &key (alter 0) (octave 0))
  (make-instance 'pitch :step step :alter alter :octave octave))

(defun make-duration (log &key (dots 0))
  (make-instance 'duration :log log :dots dots))

(defun make-rest (duration)
  (make-instance 'rest-event :duration duration))

(defun make-chord (notes duration)
  (make-instance 'chord :notes notes :duration duration))

(defun make-time-change (beats beat-type)
  (make-instance 'time-change :beats beats :beat-type beat-type))

(defun make-key-change (pitch mode)
  (make-instance 'key-change :pitch pitch :mode mode))

(defun make-clef-change (clef octave-shift)
  (make-instance 'clef-change :clef clef :octave-shift octave-shift))

(defun make-barline ()
  (make-instance 'barline))

(defparameter +pitch-step-letters+ "cdefgab")

(defun pitch-step-letter (step)
  (aref +pitch-step-letters+ step))

(defun pitch-num (pitch)
  (+ (* 7 (pitch-octave pitch)) (pitch-step pitch)))

;;; Duration value in whole-note units (exact rationals).
(defun duration-value (duration)
  (let ((base (expt 2 (- (duration-log duration)))))
    (loop for i from 1 to (duration-dots duration)
          sum (/ base (expt 2 i)) into extra
          finally (return (+ base extra)))))
