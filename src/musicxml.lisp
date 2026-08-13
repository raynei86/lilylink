(in-package #:lilylink)

;;; Emit the intermediate representation as MusicXML (score-partwise).

(defparameter +duration-type-names+
  #("whole" "half" "quarter" "eighth" "16th" "32nd" "64th"
    "128th" "256th" "512th" "1024th" "2048th" "4096th"))

(defparameter +clef-signs+
  '((:treble "G" 2) (:alto "C" 3) (:tenor "C" 4) (:bass "F" 4)))

;;; When non-NIL, every note/rest carries a <voice> element (polyphonic
;;; scores).  Bound in emit-score.
(defvar *emit-voice* nil)

(defun event-voice-of (ev)
  (typecase ev
    (note (note-voice ev))
    (rest-event (rest-voice ev))
    (chord (chord-voice ev))
    (t 1)))

(defun score-polyphonic-p (score)
  (loop for staff in (score-staves score)
        thereis (loop for m in (staff-measures staff)
                      thereis (loop for ev in (measure-events m)
                                    thereis (> (event-voice-of ev) 1)))))

(defun group-by-voice (events)
  "Group EVENTS by voice number, returning ((voice . events) ...) in order."
  (let ((table (make-hash-table)))
    (dolist (ev events)
      (push ev (gethash (event-voice-of ev) table)))
    (sort (loop for voice being the hash-keys of table
                collect (cons voice (nreverse (gethash voice table))))
          #'< :key #'car)))

(defun voice-total (events divisions)
  "Total duration of EVENTS in division units (a chord counts once)."
  (loop for ev in events
        sum (typecase ev
              (note (duration-units (note-duration ev) divisions))
              (rest-event (duration-units (rest-duration ev) divisions))
              (chord (duration-units (chord-duration ev) divisions))
              (t 0))))

(defun duration-type-name (log)
  (unless (< log (length +duration-type-names+))
    (emit-error "Cannot emit a note of duration log2 ~D" log))
  (aref +duration-type-names+ log))

(defun duration-in-divisions (duration divisions)
  (round (* divisions 4 (duration-value duration))))

(defparameter +mark-container+
  '((:articulation . "articulations")
    (:other-articulation . "articulations")
    (:ornament . "ornaments")
    (:other-ornament . "ornaments")
    (:technical . "technical")
    (:other-technical . "technical")
    (:dynamic . "dynamics")
    (:other-dynamics . "dynamics")))

(defun emit-mark-attrs (s mark)
  (loop for (key value) on (mark-attrs mark) by #'cddr
        do (format s " ~A=\"~A\"" (string-downcase key) value)))

(defun emit-mark (s mark)
  (format s "<~A" (mark-tag mark))
  (emit-mark-attrs s mark)
  (if (mark-text mark)
      (format s ">~A</~A>" (mark-text mark) (mark-tag mark))
      (write-string "/>" s)))

(defun emit-mark-group (s container marks)
  (write-string (format nil "<~A>" container) s)
  (dolist (mark marks)
    (emit-mark s mark))
  (write-string (format nil "</~A>" container) s))

;;; Write the children of <notations> (slur, tied, then mark containers) for
;;; EVENT, plus any EXTRA attachments (e.g. chord-level marks merged into the
;;; first note).  Returns whether anything was written.
(defun emit-notations-content (s event &optional extra)
  (let ((written nil)
        (marks (append extra (event-attachments event))))
    (dolist (attachment marks)
      (when (typep attachment 'slur)
        (let ((number (if (slur-phrase-p attachment)
                          (+ 100 (slur-number attachment))
                          (slur-number attachment))))
          (format s "<slur type=\"~A\" number=\"~D\"/>"
                  (string-downcase (slur-action attachment)) number))
        (setf written t)))
    (when (typep event 'note)
      (when (note-tie-stop-p event)
        (write-string "<tied type=\"stop\"/>" s)
        (setf written t))
      (when (note-tie-start-p event)
        (write-string "<tied type=\"start\"/>" s)
        (setf written t)))
    (dolist (attachment marks)
      (when (typep attachment 'glissando)
        (format s "<glissando type=\"~A\" number=\"~D\"/>"
                (string-downcase (glissando-action attachment))
                (glissando-number attachment))
        (setf written t)))
    (dolist (attachment marks)
      (when (typep attachment 'trill)
        (write-string "<ornaments>" s)
        (when (eq (trill-action attachment) :start)
          (write-string "<trill-mark/>" s))
        (format s "<wavy-line type=\"~A\" number=\"~D\"/>" 
                (string-downcase (trill-action attachment))
                (trill-number attachment))
        (write-string "</ornaments>" s)
        (setf written t)))
    (dolist (container '("ornaments" "technical" "articulations" "dynamics"))
      (let ((group (remove-if-not
                    (lambda (mark)
                      (and (typep mark 'mark)
                           (string= (cdr (assoc (mark-kind mark) +mark-container+))
                                    container)))
                    marks)))
        (when group
          (emit-mark-group s container group)
          (setf written t))))
    (dolist (mark marks)
      (when (and (typep mark 'mark) (eq (mark-kind mark) :fermata))
        (write-string "<fermata" s)
        (emit-mark-attrs s mark)
        (write-string "/>" s)
        (setf written t)))
    (dolist (attachment marks)
      (when (typep attachment 'arpeggio)
        (if (arpeggio-direction attachment)
            (format s "<arpeggiate direction=\"~A\"/>"
                    (string-downcase (arpeggio-direction attachment)))
            (write-string "<arpeggiate/>" s))
        (setf written t)))
    written))

(defun emit-notations (s event &optional extra)
  (let ((content (with-output-to-string (cs)
                   (emit-notations-content cs event extra))))
    (unless (string= content "")
      (write-string "<notations>" s)
      (write-string content s)
      (write-string "</notations>" s))))

(defun emit-pitch (s pitch)
  (format s "<pitch><step>~C</step>"
          (char-upcase (pitch-step-letter (pitch-step pitch))))
  (unless (zerop (pitch-alter pitch))
    (format s "<alter>~A</alter>" (pitch-alter pitch)))
  (format s "<octave>~D</octave></pitch>" (pitch-octave pitch)))

(defun emit-dots (s dots)
  (loop repeat dots
        do (write-string "<dot/>" s)))

(defun emit-duration (s duration divisions)
  (format s "<duration>~D</duration><type>~A</type>"
          (duration-in-divisions duration divisions)
          (duration-type-name (duration-log duration)))
  (emit-dots s (duration-dots duration)))

(defun emit-note (s note divisions &optional chord-p extra)
  (write-string "<note>" s)
  (when chord-p (write-string "<chord/>" s))
  (emit-pitch s (note-pitch note))
  (format s "<duration>~D</duration>"
          (duration-in-divisions (note-duration note) divisions))
  ;; <tie> (the sound tie) precedes <type>/<dot>; the notated marks live
  ;; inside a trailing <notations> block.
  (when (note-tie-stop-p note)
    (write-string "<tie type=\"stop\"/>" s))
  (when (note-tie-start-p note)
    (write-string "<tie type=\"start\"/>" s))
  (when *emit-voice*
    (format s "<voice>~D</voice>" (note-voice note)))
  (format s "<type>~A</type>"
          (duration-type-name (duration-log (note-duration note))))
  (emit-dots s (duration-dots (note-duration note)))
  (emit-notations s note extra)
  (write-string "</note>" s))

(defun emit-rest (s rest divisions)
  (write-string "<note><rest/>" s)
  (format s "<duration>~D</duration>"
          (duration-in-divisions (rest-duration rest) divisions))
  (when *emit-voice*
    (format s "<voice>~D</voice>" (rest-voice rest)))
  (format s "<type>~A</type>"
          (duration-type-name (duration-log (rest-duration rest))))
  (emit-dots s (duration-dots (rest-duration rest)))
  (emit-notations s rest)
  (write-string "</note>" s))

(defun emit-chord (s chord divisions)
  (let ((notes (chord-notes chord)))
    (emit-note s (first notes) divisions nil (chord-attachments chord))
    (dolist (n (rest notes))
      (emit-note s n divisions t))))

(defun emit-event (s ev divisions)
  (typecase ev
    (note
     (emit-note s ev divisions)
     (emit-wedges s ev))
    (rest-event
     (emit-rest s ev divisions)
     (emit-wedges s ev))
    (chord
     (emit-chord s ev divisions)
     (emit-wedges s ev))
    (t (emit-error "Cannot emit event ~S" ev))))

;;; Hairpins are <direction> elements, siblings of <note>, so they are emitted
;;; after the note (or chord) they attach to.
(defun emit-wedges (s event)
  (dolist (attachment (event-attachments event))
    (when (typep attachment 'wedge)
      (format s "<direction placement=\"above\"><direction-type><wedge type=\"~A\" number=\"~D\"/></direction-type></direction>"
              (string-downcase (wedge-type attachment))
              (wedge-number attachment)))))

(defun clef-sign-line (clef)
  (let ((entry (assoc clef +clef-signs+)))
    (when (null entry)
      (emit-error "Unknown clef ~S" clef))
    (destructuring-bind (sign line) (cdr entry)
      (values sign line))))

(defun emit-attributes (s data divisions)
  "Emit <attributes> from DATA, a plist snapshot of staff attributes."
  (let ((clef (getf data :clef))
        (octave-shift (getf data :clef-octave-shift)))
    (multiple-value-bind (sign line) (clef-sign-line clef)
      (format s "<attributes><divisions>~D</divisions>" divisions)
      (format s "<key><fifths>~D</fifths><mode>~A</mode></key>"
              (getf data :key-fifths)
              (if (eq (getf data :key-mode) :minor) "minor" "major"))
      (format s "<time><beats>~D</beats><beat-type>~D</beat-type></time>"
              (getf data :time-beats) (getf data :time-beat-type))
      (format s "<clef><sign>~A</sign><line>~D</line>" sign line)
      (unless (zerop octave-shift)
        (format s "<octave-change>~D</octave-change>" octave-shift))
      (write-string "</clef></attributes>" s))))

(defun emit-measure (s measure divisions)
  (format s "<measure number=\"~D\">" (measure-number measure))
  (when (measure-attributes measure)
    (emit-attributes s (measure-attr-data measure) divisions))
  (let* ((groups (group-by-voice (measure-events measure)))
         (totals (mapcar (lambda (group) (voice-total (cdr group) divisions))
                         groups)))
    (loop for group in groups
          for total in totals
          for i from 0
          do (when (plusp i)
               (format s "<backup><duration>~D</duration></backup>"
                       (nth (1- i) totals)))
             (dolist (ev (cdr group))
               (emit-event s ev divisions))))
  (write-string "</measure>" s))

(defun emit-part (s staff)
  (write-string "<part id=\"P1\">" s)
  (dolist (m (staff-measures staff))
    (emit-measure s m (staff-divisions staff)))
  (write-string "</part>" s))

(defun emit-score (s score)
  (let ((*emit-voice* (score-polyphonic-p score)))
    (write-string "<score-partwise version=\"3.1\">" s)
    (write-string "<part-list><score-part id=\"P1\"><part-name>Music</part-name>"
                  s)
    (write-string "</score-part></part-list>" s)
    (let ((staff (first (score-staves score))))
      (emit-part s staff))
    (write-string "</score-partwise>" s)))
