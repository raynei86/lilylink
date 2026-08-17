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
   (tie-stop :initarg :tie-stop :initform nil :accessor note-tie-stop-p)
   (attachments :initform nil :accessor note-attachments)
   (voice :initarg :voice :initform 1 :accessor note-voice)
   (staff :initarg :staff :initform 1 :accessor note-staff)))

(defclass rest-event ()
  ((duration :initarg :duration :accessor rest-duration)
   (attachments :initform nil :accessor rest-attachments)
   (voice :initarg :voice :initform 1 :accessor rest-voice)
   (staff :initarg :staff :initform 1 :accessor rest-staff)))

(defclass chord ()
  ((notes :initarg :notes :accessor chord-notes)
   (duration :initarg :duration :accessor chord-duration)
   (attachments :initform nil :accessor chord-attachments)
   (voice :initarg :voice :initform 1 :accessor chord-voice)
   (staff :initarg :staff :initform 1 :accessor chord-staff)))

(defclass time-change ()
  ((beats :initarg :beats :accessor time-change-beats)
   (beat-type :initarg :beat-type :accessor time-change-beat-type)
   (staff :initarg :staff :initform 1 :accessor time-change-staff)))

(defclass key-change ()
  ((pitch :initarg :pitch :accessor key-change-pitch)
   (mode :initarg :mode :accessor key-change-mode)
   (staff :initarg :staff :initform 1 :accessor key-change-staff)))

(defclass clef-change ()
  ((clef :initarg :clef :accessor clef-change-clef)
   (octave-shift :initarg :octave-shift :initform 0 :accessor clef-change-octave-shift)
   (staff :initarg :staff :initform 1 :accessor clef-change-staff)))

;;; A tempo marking (\tempo 4 = 120, \tempo "Allegro" 4 = 120, or text only).
;;; BEAT-UNIT is the note-value numerator (4 for quarter), PER-MINUTE the BPM;
;;; either can be NIL for text-only marks.
(defclass tempo-change ()
  ((text :initarg :text :initform nil :accessor tempo-text)
   (beat-unit :initarg :beat-unit :initform nil :accessor tempo-beat-unit)
   (per-minute :initarg :per-minute :initform nil :accessor tempo-per-minute)
   (staff :initarg :staff :initform 1 :accessor tempo-staff)))

(defclass barline ()
  ((voice :initarg :voice :initform 1 :accessor barline-voice)
   (staff :initarg :staff :initform 1 :accessor barline-staff)))

;;; An attached expressive mark (articulation, ornament, dynamic, technical,
;;; or fermata).  KIND selects the MusicXML container, TAG is the XML element
;;; name, ATTRS an attribute plist, TEXT optional content for other-* marks.
(defclass mark ()
  ((kind :initarg :kind :accessor mark-kind)
   (tag :initarg :tag :accessor mark-tag)
   (text :initarg :text :initform nil :accessor mark-text)
   (attrs :initarg :attrs :initform nil :accessor mark-attrs)))

;;; A slur or phrasing slur spanner attached to a note: ACTION is :start or
;;; :stop; NUMBER identifies the (possibly nested) slur; PHRASE-P marks a
;;; phrasing slur, which MusicXML has no element for and so is emitted as a
;;; <slur> with an offset number.
(defclass slur ()
  ((number :initarg :number :accessor slur-number)
   (action :initarg :action :accessor slur-action)
   (phrase-p :initarg :phrase-p :initform nil :accessor slur-phrase-p)))

;;; A hairpin (crescendo/diminuendo) spanner.  On a start note TYPE is
;;; :crescendo or :diminuendo; on a stop note it is :stop.
(defclass wedge ()
  ((number :initarg :number :accessor wedge-number)
   (type :initarg :type :accessor wedge-type)))

;;; A glissando spanner from a note to the immediately following note.
(defclass glissando ()
  ((number :initarg :number :accessor glissando-number)
   (action :initarg :action :accessor glissando-action)))  ; :start :stop

;;; A trill span (\startTrillSpan ... \stopTrillSpan).
(defclass trill ()
  ((number :initarg :number :accessor trill-number)
   (action :initarg :action :accessor trill-action)))  ; :start :stop

;;; An arpeggio on a chord; DIRECTION is NIL, :up, or :down.
(defclass arpeggio ()
  ((direction :initarg :direction :initform nil :accessor arpeggio-direction)))

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

(defun make-rest (duration &optional (staff 1))
  (make-instance 'rest-event :duration duration :staff staff))

(defun make-chord (notes duration &optional (staff 1))
  (make-instance 'chord :notes notes :duration duration :staff staff))

(defun make-mark (spec)
  "Build a MARK from a descriptor (KIND XML-TAG . PROPS); a :text prop is
moved into the mark's text slot."
  (destructuring-bind (kind tag &rest props) spec
    (let ((text (getf props :text)))
      (remf props :text)
      (make-instance 'mark :kind kind :tag tag :text text :attrs props))))

(defun make-slur (number action &optional phrase-p)
  (make-instance 'slur :number number :action action :phrase-p phrase-p))

(defun make-wedge (number type)
  (make-instance 'wedge :number number :type type))

(defun make-glissando (number action)
  (make-instance 'glissando :number number :action action))

(defun make-trill (number action)
  (make-instance 'trill :number number :action action))

(defun make-arpeggio (&optional (direction nil))
  (make-instance 'arpeggio :direction direction))

(defun attachment-stop-p (attachment)
  "Whether an attachment terminates a spanner (goes on the last split piece)."
  (or (and (typep attachment 'slur) (eq (slur-action attachment) :stop))
      (and (typep attachment 'wedge) (eq (wedge-type attachment) :stop))
      (and (typep attachment 'glissando) (eq (glissando-action attachment) :stop))
      (and (typep attachment 'trill) (eq (trill-action attachment) :stop))))

;;; Attached marks can be added to notes, rests, and chords uniformly.
(defgeneric event-attachments (event))
(defgeneric (setf event-attachments) (value event))
(defmethod event-attachments ((n note)) (note-attachments n))
(defmethod (setf event-attachments) (v (n note)) (setf (note-attachments n) v))
(defmethod event-attachments ((r rest-event)) (rest-attachments r))
(defmethod (setf event-attachments) (v (r rest-event)) (setf (rest-attachments r) v))
(defmethod event-attachments ((c chord)) (chord-attachments c))
(defmethod (setf event-attachments) (v (c chord)) (setf (chord-attachments c) v))

(defun event-staff-of (ev)
  "The staff index an event belongs to (default 1)."
  (typecase ev
    (note (note-staff ev))
    (rest-event (rest-staff ev))
    (chord (chord-staff ev))
    (barline (barline-staff ev))
    (time-change (time-change-staff ev))
    (key-change (key-change-staff ev))
    (clef-change (clef-change-staff ev))
    (tempo-change (tempo-staff ev))
    (t 1)))

(defun make-time-change (beats beat-type &optional (staff 1))
  (make-instance 'time-change :beats beats :beat-type beat-type :staff staff))

(defun make-key-change (pitch mode &optional (staff 1))
  (make-instance 'key-change :pitch pitch :mode mode :staff staff))

(defun make-clef-change (clef octave-shift &optional (staff 1))
  (make-instance 'clef-change :clef clef :octave-shift octave-shift :staff staff))

(defun make-barline (&optional (voice 1) (staff 1))
  (make-instance 'barline :voice voice :staff staff))

(defun make-tempo-change (&key text beat-unit per-minute (staff 1))
  (make-instance 'tempo-change :text text :beat-unit beat-unit
                 :per-minute per-minute :staff staff))

;;; The union of all events that flow through the parse/build/emit
;;; pipeline.  Used for exhaustive dispatch (etypecase-of).
(deftype event ()
  '(or note rest-event chord barline
    time-change key-change clef-change tempo-change))

;;; The events that survive into a measure's event list and are emitted
;;; as <note> elements (build-score turns time/key/clef changes into
;;; attributes and consumes barlines, so neither reaches emit-event).
(deftype musical-event ()
  '(or note rest-event chord))

(defconst +pitch-step-letters+ "cdefgab")

(defconst +max-duration-log+ 10
  "Largest supported duration log2 (whole=0 .. 1024th=10).")

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
