(in-package #:lilylink)

;;; Emit the intermediate representation as MusicXML (score-partwise), built
;;; declaratively through the tiny EL / XML-ESCAPE helpers below rather than
;;; by hand-concatenating format strings.

;;; ---------------------------------------------------------------------------
;;; A minimal XML writer
;;; ---------------------------------------------------------------------------

(defun xml-escape (string)
  "Escape &, <, >, \\\", and ' in STRING for XML text/attribute content."
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&apos;" out))
               (t (write-char ch out))))))

(defun build-el (tag attrs children &optional (force-open-close nil))
  "Build an XML element as a string.  TAG names the element (lowercased);
ATTRS is a list of (KEY VALUE) pairs; CHILDREN is a list of strings, numbers,
keywords, or other element strings (a single list-valued child is flattened
in, so callers may pass (append ...) or (mapcar ...) results).  NIL children
are dropped; an element with no children is self-closing unless
FORCE-OPEN-CLOSE is true (used for containers like <part> that must always
have a closing tag).  Text and attribute values are escaped, and attribute
values that are symbols (keywords) are lowercased."
  (let ((name (string-downcase (symbol-name tag)))
        (attr-strs nil)
        (kids (flatten-children children)))
    (dolist (pair attrs)
      (destructuring-bind (key value) pair
        (push (format nil " ~A=\"~A\""
                      (string-downcase (symbol-name key))
                      (xml-escape (attr-value-string value)))
              attr-strs)))
    (let ((attr-strs (nreverse attr-strs))
          (kids (remove nil kids)))
      (if (null kids)
          (if force-open-close
              (format nil "<~A~{~A~}></~A>" name attr-strs name)
              (format nil "<~A~{~A~}/>" name attr-strs))
          (format nil "<~A~{~A~}>~{~A~}</~A>"
                  name attr-strs
                  (mapcar (lambda (c)
                            (etypecase c
                              (string (xml-escape-text c))
                              (integer (princ-to-string c))
                              (symbol (xml-escape (string-downcase (symbol-name c))))
                              (t (princ-to-string c))))
                          kids)
                  name)))))

(defun xml-escape-text (string)
  "Escape STRING as XML text content, unless it is already an element (i.e.
begins with '<'), in which case it is emitted verbatim."
  (if (and (plusp (length string)) (char= (char string 0) #\<))
      string
      (xml-escape string)))

(defun attr-value-string (value)
  "A string for an attribute VALUE; keywords are lowercased."
  (if (keywordp value)
      (string-downcase (symbol-name value))
      (princ-to-string value)))

(defun flatten-children (children)
  "Flatten one level of CHILDREN so a single list child (e.g. from append or
mapcar) contributes its elements directly."
  (loop for c in children
        append (if (and (consp c) (not (stringp c)) (not (keywordp c)))
                   c
                   (list c))))

(defmacro el (tag attrs &rest children)
  "Build an XML element.  TAG is a keyword naming the element; ATTRS is an
attribute plist whose keys are literal keywords and whose values are
expressions (evaluated at runtime); CHILDREN are forms whose values become
child content.  A trailing :open-close keyword argument forces the element
to have a closing tag even when it has no children."
  (let ((open-close (and (member :open-close children) t))
        (body (remove :open-close children))
        (attr-forms nil))
    (loop for (key value) on attrs by #'cddr
          do (push `(list (quote ,key) ,value) attr-forms))
    `(build-el ,tag (list ,@(nreverse attr-forms)) (list ,@body)
               ,open-close)))

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

;;; ---------------------------------------------------------------------------
;;; Marks and notations
;;; ---------------------------------------------------------------------------

;;; Each (KIND . CONTAINER) pair says which <notations> container a mark of a
;;; given kind is emitted into.  Standalone dynamics are handled separately as
;;; <direction> elements (see EMIT-DYNAMICS).
(defparameter +mark-container+
  '((:articulation . "articulations")
    (:other-articulation . "articulations")
    (:ornament . "ornaments")
    (:other-ornament . "ornaments")
    (:technical . "technical")
    (:other-technical . "technical")))

(defun mark-attr-pairs (mark)
  "MARK's attributes as a list of (KEY VALUE) pairs for BUILD-EL."
  (loop for (key value) on (mark-attrs mark) by #'cddr
        collect (list key value)))

(defun emit-mark (mark)
  "A single MARK as an element string, with text content or self-closing."
  (let ((tag (intern (string-upcase (mark-tag mark)) "KEYWORD")))
    (if (mark-text mark)
        (build-el tag (mark-attr-pairs mark) (list (mark-text mark)))
        (build-el tag (mark-attr-pairs mark) nil))))

(defun emit-mark-group (container marks)
  (el (intern (string-upcase container) "KEYWORD") nil
      (mapcar #'emit-mark marks)))

;;; The children of <notations> for EVENT plus any EXTRA attachments (e.g.
;;; chord-level marks merged into the first note), in MusicXML's required
;;; order: slurs, tied, glissando, trill ornaments, mark containers, fermata,
;;; arpeggio.  Returns a list of element strings (possibly empty).
(defun slur-p (a) (typep a 'slur))
(defun glissando-p (a) (typep a 'glissando))
(defun trill-p (a) (typep a 'trill))
(defun mark-p (a) (typep a 'mark))
(defun fermata-p (a) (and (typep a 'mark) (eq (mark-kind a) :fermata)))
(defun arpeggio-p (a) (typep a 'arpeggio))

(defun emit-slurs (marks)
  (filter-map (lambda (a)
                (when (slur-p a)
                  (let ((number (if (slur-phrase-p a)
                                    (+ 100 (slur-number a))
                                    (slur-number a))))
                    (el :slur (:type (slur-action a) :number number)))))
              marks))

(defun emit-glissandos (marks)
  (filter-map (lambda (a)
                (when (glissando-p a)
                  (el :glissando (:type (glissando-action a)
                                        :number (glissando-number a)))))
              marks))

(defun emit-trills (marks)
  (filter-map (lambda (a)
                (when (trill-p a)
                  (el :ornaments nil
                      (when (eq (trill-action a) :start) (el :trill-mark nil))
                      (el :wavy-line (:type (trill-action a)
                                            :number (trill-number a))))))
              marks))

(defun emit-arpeggios (marks)
  (filter-map (lambda (a)
                (when (arpeggio-p a)
                  (if (arpeggio-direction a)
                      (el :arpeggiate (:direction (arpeggio-direction a)))
                      (el :arpeggiate nil))))
              marks))

(defun emit-fermatas (marks)
  (filter-map (lambda (a)
                (when (fermata-p a)
                  (build-el :fermata (mark-attr-pairs a) nil)))
              marks))

;;; Marks are grouped into their containers in a fixed order.
(defparameter +mark-container-order+
  '("ornaments" "technical" "articulations"))

(defun emit-mark-containers (marks)
  (let ((marks (remove-if-not #'mark-p marks)))
     (loop for container in +mark-container-order+
           for group = (remove-if-not
                        (lambda (mark)
                          (string= (assocdr (mark-kind mark)
                                               +mark-container+)
                                   container))
                       marks)
          when group
          collect (emit-mark-group container group))))

;;; +notations-order+ is a list of (TEST . EMITTER) applied in order: TEST
;;; selects the attachments an EMITTER handles (receiving the whole list and
;;; returning its element strings).  Notated ties are emitted right after the
;;; slurs (MusicXML's order), since they come from the note itself rather than
;;; an attachment.
(defparameter +notations-order+
  (list (cons #'glissando-p #'emit-glissandos)
        (cons #'trill-p #'emit-trills)
        (cons #'mark-p #'emit-mark-containers)
        (cons #'fermata-p #'emit-fermatas)
        (cons #'arpeggio-p #'emit-arpeggios)))

(defun notations-children (event &optional extra)
  (let ((marks (append extra (event-attachments event))))
    (append
     ;; Slurs.
     (when (some #'slur-p marks) (emit-slurs marks))
     ;; Notated ties.
     (when (typep event 'note)
       (append (when (note-tie-stop-p event)
                 (list (el :tied (:type "stop"))))
               (when (note-tie-start-p event)
                 (list (el :tied (:type "start"))))))
     ;; Everything else in registry order.
     (loop for (test . emitter) in +notations-order+
           when (some test marks)
           append (funcall emitter marks)))))

(defun emit-notations (event &optional extra)
  (let ((children (notations-children event extra)))
    (when children
      (el :notations nil children))))

;;; ---------------------------------------------------------------------------
;;; Notes, rests, and chords
;;; ---------------------------------------------------------------------------

(defun emit-pitch (pitch)
  (el :pitch nil
      (el :step nil (char-upcase (pitch-step-letter (pitch-step pitch))))
      (unless (zerop (pitch-alter pitch))
        (el :alter nil (pitch-alter pitch)))
      (el :octave nil (pitch-octave pitch))))

(defun emit-dots (dots)
  (loop repeat dots collect (el :dot nil)))

(defun emit-duration (duration divisions)
  (append
   (list (el :duration nil (duration-in-divisions duration divisions))
         (el :type nil (duration-type-name (duration-log duration))))
   (emit-dots (duration-dots duration))))

(defun emit-note (note divisions &optional chord-p extra)
  (el :note nil
      (when chord-p (el :chord nil))
      (emit-pitch (note-pitch note))
      (el :duration nil (duration-in-divisions (note-duration note) divisions))
      (when (note-tie-stop-p note) (el :tie (:type "stop")))
      (when (note-tie-start-p note) (el :tie (:type "start")))
      (when *emit-voice* (el :voice nil (note-voice note)))
      (el :type nil (duration-type-name (duration-log (note-duration note))))
      (emit-dots (duration-dots (note-duration note)))
      (emit-notations note extra)))

(defun emit-rest (rest divisions)
  (el :note nil
      (el :rest nil)
      (el :duration nil (duration-in-divisions (rest-duration rest) divisions))
      (when *emit-voice* (el :voice nil (rest-voice rest)))
      (el :type nil (duration-type-name (duration-log (rest-duration rest))))
      (emit-dots (duration-dots (rest-duration rest)))
      (emit-notations rest)))

(defun emit-chord (chord divisions)
  (let ((notes (chord-notes chord)))
    (cons (emit-note (first notes) divisions nil (chord-attachments chord))
          (mapcar (lambda (n) (emit-note n divisions t)) (rest notes)))))

;;; ---------------------------------------------------------------------------
;;; Directions: hairpins, dynamics, and tempo
;;; ---------------------------------------------------------------------------

(defun emit-wedges (event)
  "Hairpin <direction> elements attached to EVENT."
  (filter-map (lambda (a)
                (when (typep a 'wedge)
                  (el :direction (:placement "above")
                      (el :direction-type nil
                          (el :wedge (:type (wedge-type a)
                                           :number (wedge-number a)))))))
              (event-attachments event)))

;;; MusicXML places standalone dynamics inside a <dynamics> container within
;;; <direction-type>, as <direction> siblings of the note.
(defparameter +direction-container+
  '((:dynamic . "dynamics")
    (:other-dynamics . "dynamics")))

(defun emit-dynamics (event)
  "Standalone dynamic marks (\\f, \\mf, \\sff, ...) attached to EVENT as
<direction> elements (standard placement, siblings of the note)."
  (filter-map (lambda (mark)
(when (and (typep mark 'mark)
                            (in (mark-kind mark) :dynamic :other-dynamics))
                   (el :direction (:placement "above")
                      (el :direction-type nil
                          (el (intern (string-upcase
                                       (assocdr (mark-kind mark)
                                                +direction-container+))
                                      "KEYWORD")
                              nil
                              (emit-mark mark))))))
              (event-attachments event)))

;;; A tempo marking is a <direction> with an optional <words>, <metronome>,
;;; and <sound tempo>.
(defun emit-tempo (ev)
  (el :direction (:placement "above")
      (el :direction-type nil
          (when (tempo-text ev) (el :words nil (tempo-text ev)))
          (when (tempo-beat-unit ev)
            (el :metronome nil
                (el :beat-unit nil
                    (duration-type-name
                     (duration-log-from-num (tempo-beat-unit ev))))
                (when (tempo-per-minute ev)
                  (el :per-minute nil (tempo-per-minute ev))))))
      (when (tempo-per-minute ev)
        (el :sound (:tempo (tempo-per-minute ev))))))

;;; ---------------------------------------------------------------------------
;;; Attributes and measures
;;; ---------------------------------------------------------------------------

(defun clef-sign-line (clef)
  (let ((entry (assoc clef +clef-signs+)))
    (when (null entry)
      (emit-error "Unknown clef ~S" clef))
    (destructuring-bind (sign line) (cdr entry)
      (values sign line))))

(defun emit-attributes (data divisions)
  "Emit <attributes> from DATA, a plist snapshot of staff attributes."
  (let ((clef (getf data :clef))
        (octave-shift (getf data :clef-octave-shift)))
    (multiple-value-bind (sign line) (clef-sign-line clef)
      (el :attributes nil
          (el :divisions nil divisions)
          (el :key nil
              (el :fifths nil (getf data :key-fifths))
              (el :mode nil (if (eq (getf data :key-mode) :minor)
                                "minor" "major")))
          (el :time nil
              (el :beats nil (getf data :time-beats))
              (el :beat-type nil (getf data :time-beat-type)))
          (el :clef nil
              (el :sign nil sign)
              (el :line nil line)
              (unless (zerop octave-shift)
                (el :octave-change nil octave-shift)))))))

(defun emit-measure (measure divisions)
  "A whole <measure> element string."
  (el :measure (:number (measure-number measure))
      (when (measure-attributes measure)
        (emit-attributes (measure-attr-data measure) divisions))
      (let* ((events (measure-events measure))
             ;; Directions (tempo) are emitted before the voice-grouped notes.
             (directions (remove-if-not (lambda (ev) (typep ev 'tempo-change))
                                        events))
             (musical (remove-if (lambda (ev) (typep ev 'tempo-change)) events))
             (groups (group-by-voice musical))
             (totals (mapcar (lambda (group) (voice-total (cdr group) divisions))
                             groups)))
        (append
         (mapcar #'emit-tempo directions)
         (loop for group in groups
               for total in totals
               for i from 0
               append (append (when (plusp i)
                                (list (el :backup nil
                                          (el :duration nil (nth (1- i) totals)))))
                              (loop for ev in (cdr group)
                                    append (emit-event ev divisions))))))))

(defun emit-event (ev divisions)
  "EVENT as a list of element strings (a chord expands to several notes, and
note-attached directions follow their note)."
  (etypecase-of musical-event ev
    (note (append (list (emit-note ev divisions))
               (emit-wedges ev)
               (emit-dynamics ev)))
    (rest-event (append (list (emit-rest ev divisions))
                   (emit-wedges ev)
                   (emit-dynamics ev)))
    (chord (append (emit-chord ev divisions)
               (emit-wedges ev)
               (emit-dynamics ev)))))

(defun emit-part (staff id)
  (el :part (:id (format nil "P~D" id)) :open-close
      (mapcar (lambda (m) (emit-measure m (staff-divisions staff)))
              (staff-measures staff))))

(defun emit-score (s score)
  (let ((*emit-voice* (score-polyphonic-p score))
        (staves (score-staves score)))
    (let ((xml (el :score-partwise (:version "3.1")
                   (el :part-list nil
                       (loop for staff in staves
                             for id from 1
                             collect (el :score-part (:id (format nil "P~D" id))
                                         (el :part-name nil "Music"))))
                   (loop for staff in staves
                         for id from 1
                         collect (emit-part staff id)))))
      (write-string xml s))))
