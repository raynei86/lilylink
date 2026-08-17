(in-package #:lilylink)

;;; A mutable relative-mode context. REF is either a PITCH object (the
;;; current reference for octave placement) or :UNSET (used by the
;;; `\relative { ... }` form, where the first note is written in absolute
;;; pitch and becomes the reference).
(defstruct (rel-ctx (:constructor make-rel-ctx (&optional (ref :unset))))
  ref)

;;; The state that must be isolated per voice and reset by rests: open
;;; spanners (ties, slurs, hairpins, glissandos, trills) and the current
;;; arpeggio direction.  Grouping it here keeps save/restore and reset to a
;;; single call (see parse-simultaneous, parse-voice-expression, parse-rest-event).
(defstruct (spanner-state (:constructor make-spanner-state (&key ties slurs wedge glissando trill arpeggio)))
  (ties nil)
  (slurs nil)
  (wedge nil)
  (glissando nil)
  (trill nil)
  (arpeggio nil))

(defstruct (parser (:constructor make-parser (tokens)))
  tokens
  (pos 0)
  (last-pitch nil)
  (last-duration nil)
  (last-event nil)
  (voice 1)
  (staff 1)
  (max-staff 0)
  (spanners (make-spanner-state)))

(defun reset-spanner-state (p)
  "Drop all open spanners and the arpeggio direction."
  (setf (parser-spanners p) (make-spanner-state)))

;;; Short accessors for one spanner slot of the parser: (spanner p :ties) etc.
;;; Handed out to the post-event dispatch table as lambdas so handlers read
;;; (ties p) / (setf (ties p) ...).
(defun spanner (p name)
  (let ((s (parser-spanners p)))
    (ecase name
      (:ties (spanner-state-ties s))
      (:slurs (spanner-state-slurs s))
      (:wedge (spanner-state-wedge s))
      (:glissando (spanner-state-glissando s))
      (:trill (spanner-state-trill s))
      (:arpeggio (spanner-state-arpeggio s)))))

(defun (setf spanner) (value p name)
  (let ((s (parser-spanners p)))
    (ecase name
      (:ties (setf (spanner-state-ties s) value))
      (:slurs (setf (spanner-state-slurs s) value))
      (:wedge (setf (spanner-state-wedge s) value))
      (:glissando (setf (spanner-state-glissando s) value))
      (:trill (setf (spanner-state-trill s) value))
      (:arpeggio (setf (spanner-state-arpeggio s) value)))))

(defun parse-music (string)
  "Parse a LilyPond source string into a list of events."
  (parse-file-events (make-parser (coerce (tokenize string) 'vector))))

(defun peek-token (p)
  (when (< (parser-pos p) (length (parser-tokens p)))
    (aref (parser-tokens p) (parser-pos p))))

(defun advance-token (p)
  (prog1 (peek-token p)
    (incf (parser-pos p))))

(defun token-line-or-0 (tok)
  (if tok (token-line tok) 0))

(defun token-col-or-0 (tok)
  (if tok (token-col tok) 0))

(defun expect-token (p type)
  (let ((tok (advance-token p)))
    (unless (and tok (eq (token-type tok) type))
      (signal-parse-error (token-line-or-0 tok) (token-col-or-0 tok) tok
                          "Expected ~S but found ~S" type
                          (if tok (token-type tok) 'eof)))
    tok))

(defun parser-error (p fmt &rest args)
  (let ((tok (peek-token p)))
    (apply #'signal-parse-error (token-line-or-0 tok) (token-col-or-0 tok)
           tok fmt args)))

;;; Token types that can start a fresh event/form, used by the recovery
;;; restarts to resynchronize after skipping past a bad construct.
(defconst +event-start-types+
  '(:pitch :rest :chord-open :number :barline :brace-close
    :simult-close :voice-separator :command))

(defun resync-to-event-start (p &optional (advance-one nil))
  "Best-effort recovery: advance past tokens that cannot start an event,
stopping at the next +EVENT-START-TYPES+ token or EOF.  When ADVANCE-ONE
is true, first advance past the current token (used by SKIP-EVENT, where
the offending token was not yet consumed); SKIP-COMMAND leaves the already
consumed command token behind and passes NIL."
  (when advance-one
    (advance-token p))
  (loop while (and (peek-token p)
                   (not (member (token-type (peek-token p))
                                +event-start-types+)))
        do (advance-token p)))

(defun recover (p restart fmt &rest args)
  "Handle a recoverable problem at the current token.  In strict mode
(*strict-mode*) signal a parse error; otherwise warn and invoke RESTART
(one of the SKIP-* recovery restarts), falling back to a plain resync.
The warning carries the current token's location."
  (let ((tok (peek-token p)))
    (if *strict-mode*
        (apply #'signal-parse-error (token-line-or-0 tok) (token-col-or-0 tok)
               tok fmt args)
        (progn
          (apply #'signal-warning (token-line-or-0 tok) (token-col-or-0 tok)
                 fmt args)
          (if (find-restart restart)
              (invoke-restart restart)
              (resync-to-event-start p))))))

(defun resync-command-args (p)
  "Best-effort recovery for a consumed top-level \\command: skip bare
note/word/argument tokens and one optional trailing braced block, stopping
at the next top-level construct."
  (loop while (and (peek-token p)
                   (member (token-type (peek-token p))
                           '(:pitch :rest :number :word :slash :string)))
        do (advance-token p))
  (when (and (peek-token p) (eq (token-type (peek-token p)) :brace-open))
    (skip-braced-block p)))

;;; Push EVENT onto the accumulating list, record it as the parser's last event
;;; (used by \breathe), and return the updated list so callers can setf it.

(defun track-last-event (p event)
  (setf (parser-last-event p) event)
  event)

;;; Relative octave placement: choose the octave that minimizes the
;;; diatonic interval (ignoring accidentals) between the target step and
;;; the reference pitch.
(defun relative-octave (ref-num target-step)
  (round (/ (- ref-num target-step) 7)))

(defun resolve-pitch (p ctx step alter net)
  "Resolve STEP/ALTER/NET (net octave marks) against CTX into a PITCH object.
CTX is a REL-CTX or NIL (absolute mode)."
  (let* ((base (make-pitch step :alter alter :octave (+ 3 net)))
         (pitch (if (and ctx (not (eq (rel-ctx-ref ctx) :unset)))
                    (make-pitch step :alter alter
                                :octave (+ (relative-octave (pitch-num (rel-ctx-ref ctx))
                                                            step)
                                           net))
                    base)))
    (when ctx (setf (rel-ctx-ref ctx) pitch))
    (setf (parser-last-pitch p) pitch)
    pitch))

(defun resolve-pitch-token (p ctx pt)
  "Resolve the PITCH-TOKEN PT against CTX into a PITCH object."
  (resolve-pitch p ctx (pitch-token-step pt) (pitch-token-alter pt)
                 (pitch-token-octave-mark pt)))

(defun effective-duration (p tok-dur)
  "Return the effective duration for a note/rest given an explicit token
duration (LOG . DOTS) or NIL, updating the parser's last-duration."
  (if tok-dur
      (setf (parser-last-duration p)
            (make-duration (car tok-dur) :dots (cdr tok-dur)))
      (or (parser-last-duration p) (make-duration 2))))

;;; Ties: a tie (the ~ token) connects a note to the next note of the same
;;; written pitch (step, alteration, and octave).  Pending ties are matched
;;; against the pitches of the very next note/chord; any that do not match are
;;; dropped, mirroring LilyPond.
(defun pitch-key (pitch)
  (list (pitch-step pitch) (pitch-alter pitch) (pitch-octave pitch)))

(defun pitch-in-pending (p pitch)
  (member (pitch-key pitch) (spanner-state-ties (parser-spanners p)) :test #'equal))

(defun consume-tie (p)
  (let ((tok (peek-token p)))
    (when (and tok (eq (token-type tok) :tie))
      (advance-token p)
      t)))

(defun finish-note (p pitch duration tie-stop-p)
  "Build a NOTE, marking a pending-tie stop and consuming a trailing ~ as start."
  (let ((note (make-instance 'note :pitch pitch :duration duration
                             :voice (parser-voice p) :staff (parser-staff p))))
    (when tie-stop-p
      (setf (note-tie-stop-p note) t))
    (when (consume-tie p)
      (setf (note-tie-start-p note) t)
      (push (pitch-key pitch) (spanner-state-ties (parser-spanners p))))
    note))

;;; Expressive marks attached to a note, rest, or chord via \name commands,
;;; -X shorthand articulations, and ^ / _ (direction, parsed and ignored) or
;;; - prefixes.  The loop stops at the first token that is not an attachment.
(defun parse-attached-mark (p event)
  (let ((tok (peek-token p)))
    (unless (and tok (eq (token-type tok) :command)
                 (lookup-mark (token-value tok)))
      (recover p 'skip-event "Expected an expressive mark after the attachment prefix"))
    (let* ((cmd (token-value tok))
           (spec (lookup-mark cmd)))
      (advance-token p)
      (when (serapeum:in (car spec) :dynamic :other-dynamics)
        (close-pending-wedge p event))
      (push (make-mark spec) (event-attachments event)))))

(defun handle-hairpin (p event cmd)
  (advance-token p)
  (close-pending-wedge p event)
  (let ((type (if (eq cmd :cr) :crescendo :diminuendo)))
    (setf (spanner p :wedge) (cons type 1))
    (push (make-wedge 1 type) (event-attachments event))))

(defun handle-hairpin-stop (p event cmd)
  (declare (ignore cmd))
  (advance-token p)
  (close-pending-wedge p event))

(defun handle-glissando (p event cmd)
  (declare (ignore cmd))
  (advance-token p)
  (setf (spanner p :glissando) 1)
  (push (make-glissando 1 :start) (event-attachments event)))

(defun handle-trill-span (p event cmd)
  (advance-token p)
  (when-let ((open (spanner p :trill)))
    (push (make-trill open :stop) (event-attachments event)))
  (ecase cmd
    (:starttrillspan
     (setf (spanner p :trill) 1)
     (push (make-trill 1 :start) (event-attachments event)))
    (:stoptrillspan
     (setf (spanner p :trill) nil))))

(defun handle-arpeggio (p event cmd)
  (declare (ignore cmd))
  (advance-token p)
  (push (make-arpeggio (spanner p :arpeggio)) (event-attachments event))
  (setf (spanner p :arpeggio) nil))

(defun handle-bend (p event cmd)
  (declare (ignore cmd))
  (advance-token p)
  (parse-bend-args p event))

;;; Special post-event commands (those that do more than attach a plain mark),
;;; dispatched by keyword from parse-post-events.  Each handler consumes its
;;; own command token and mutates the parser/event.
(defconst +post-event-commands+
  '((:cr . handle-hairpin)
    (:decr . handle-hairpin)
    (:endcr . handle-hairpin-stop)
    (:enddecr . handle-hairpin-stop)
    (:glissando . handle-glissando)
    (:starttrillspan . handle-trill-span)
    (:stoptrillspan . handle-trill-span)
    (:arpeggio . handle-arpeggio)
    (:bendafter . handle-bend)))

(defun parse-post-events (p event)
  (loop
    (let ((tok (peek-token p)))
      (when (null tok) (return))
      (case (token-type tok)
        (:articulation
         (let* ((tok (advance-token p))
                (spec (lookup-mark (token-value tok))))
           (unless spec
             (recover p 'skip-event "Unknown articulation ~S" (token-value tok)))
           (push (make-mark spec) (event-attachments event))))
        (:command
         (let* ((cmd (token-value tok))
                (spec (lookup-mark cmd))
                (entry (assoc cmd +post-event-commands+)))
           (cond (spec
                  (advance-token p)
                  ;; An absolute dynamic terminates an open hairpin.
                  (when (serapeum:in (car spec) :dynamic :other-dynamics)
                    (close-pending-wedge p event))
                  (push (make-mark spec) (event-attachments event)))
                 (entry
                  (funcall (cdr entry) p event cmd))
                 (t (return)))))
        ((:attach-dash :attach-up :attach-down)
         (advance-token p)
         (parse-attached-mark p event))
        (:slur-open
         (advance-token p)
         (let ((number (1+ (length (spanner p :slurs)))))
           (push (cons nil number) (spanner p :slurs))
           (push (make-slur number :start) (event-attachments event))))
        (:slur-close
         (advance-token p)
         (let ((spec (pop (spanner p :slurs))))
           (when spec
             (push (make-slur (cdr spec) :stop) (event-attachments event)))))
        (:phrase-open
         (advance-token p)
         (let ((number (1+ (length (spanner p :slurs)))))
           (push (cons t number) (spanner p :slurs))
           (push (make-slur number :start t) (event-attachments event))))
        (:phrase-close
         (advance-token p)
         (let ((spec (pop (spanner p :slurs))))
           (when spec
             (push (make-slur (cdr spec) :stop t) (event-attachments event)))))
        (:wedge-start
         (advance-token p)
         (close-pending-wedge p event)
         (setf (spanner p :wedge) (cons (token-value tok) 1))
         (push (make-wedge 1 (token-value tok)) (event-attachments event)))
        (:wedge-stop
         (advance-token p)
         (close-pending-wedge p event))
        (t (return)))))
  event)

(defun close-pending-wedge (p event)
  "Close the open hairpin, if any, marking EVENT as its stop."
  (when-let ((wedge (spanner-state-wedge (parser-spanners p))))
    (push (make-wedge (cdr wedge) :stop) (event-attachments event))
    (setf (spanner-state-wedge (parser-spanners p)) nil)))

;;; A glissando connects to the immediately following note or chord, so the
;;; next event stops any open glissando.  Trill spans are ended explicitly.
(defun add-implicit-spanner-stops (p event)
  (when-let ((glissando (spanner-state-glissando (parser-spanners p))))
    (push (make-glissando glissando :stop) (event-attachments event))
    (setf (spanner-state-glissando (parser-spanners p)) nil)))

(defun parse-bend-args (p event)
  ;; \bendAfter has been consumed.  Read the interval; only its sign matters.
  (let ((sign 1))
    (when (and (peek-token p)
               (eq (token-type (peek-token p)) :attach-dash))
      (advance-token p)
      (setf sign -1))
    (when (and (peek-token p) (eq (token-type (peek-token p)) :number))
      (advance-token p))
    (when (and (peek-token p) (eq (token-type (peek-token p)) :number))
      (advance-token p))
    (push (make-mark (if (minusp sign)
                         '(:articulation "falloff")
                         '(:articulation "doit")))
          (event-attachments event))))

(defun parse-note-event (p ctx)
  (let* ((tok (advance-token p))
         (pt (token-value tok))
         (pitch (resolve-pitch-token p ctx pt))
         (tie-stop-p (pitch-in-pending p pitch)))
    (setf (spanner-state-ties (parser-spanners p)) nil)
    (let ((note (finish-note p pitch (effective-duration p (pitch-token-duration pt))
                             tie-stop-p)))
      (add-implicit-spanner-stops p note)
      (parse-post-events p note))))

(defun parse-rest-event (p ctx)
  (declare (ignore ctx))
  ;; A rest (or spacer) breaks pending ties and spanners.
  (reset-spanner-state p)
  (let* ((tok (advance-token p))
         (duration (effective-duration p (token-value tok))))
    (let ((rest (make-rest duration (parser-staff p))))
      (setf (rest-voice rest) (parser-voice p))
      (parse-post-events p rest))))

(defun parse-duration-event (p ctx)
  (declare (ignore ctx))
  (unless (parser-last-pitch p)
    (recover p 'skip-event "Duration with no preceding note"))
  (let* ((tok (advance-token p))
         (val (token-value tok))
         (duration (setf (parser-last-duration p)
                         (make-duration (duration-log-from-num (car val))
                                        :dots (cdr val))))
         (pitch (parser-last-pitch p))
         (tie-stop-p (pitch-in-pending p pitch)))
    (setf (spanner-state-ties (parser-spanners p)) nil)
    (let ((note (finish-note p pitch duration tie-stop-p)))
      (add-implicit-spanner-stops p note)
      (parse-post-events p note))))

(defun parse-chord (p ctx)
  (advance-token p)  ; consume <
  (let ((entries nil)
        (first-pitch nil))
    (loop
      (let ((tok (peek-token p)))
        (when (or (null tok) (eq (token-type tok) :chord-close)) (return))
        (unless (eq (token-type tok) :pitch)
          (recover p 'skip-event "Expected a pitch inside a chord"))
        (let* ((tok (advance-token p))
               (pitch (resolve-pitch-token p ctx (token-value tok)))
               (tie-stop-p (pitch-in-pending p pitch))
               (tie-start-p (consume-tie p)))
          (unless first-pitch (setf first-pitch pitch))
          (push (list pitch tie-stop-p tie-start-p) entries))))
    (expect-token p :chord-close)
    ;; The chord is fully matched: drop any pending ties it did not consume.
    (setf (spanner-state-ties (parser-spanners p)) nil)
    ;; The reference for anything following the chord is its first note.
    (when (and ctx first-pitch)
      (setf (rel-ctx-ref ctx) first-pitch))
    (when first-pitch
      (setf (parser-last-pitch p) first-pitch))
    ;; Optional adjacent duration after `>`.
    (when-let ((tok (peek-token p)))
      (when (eq (token-type tok) :number)
        (let* ((val (token-value (advance-token p)))
               (duration (make-duration (duration-log-from-num (car val))
                                        :dots (cdr val))))
          (setf (parser-last-duration p) duration))))
    ;; A trailing ~ after `>` ties every note in the chord.
    (let* ((chord-tie-p (consume-tie p))
           (duration (or (parser-last-duration p) (make-duration 2)))
           (notes (mapcar (lambda (entry)
                             (destructuring-bind (pitch tie-stop-p tie-start-p) entry
                               (let ((note (make-instance 'note :pitch pitch
                                                           :duration duration
                                                           :staff (parser-staff p))))
                                 (when tie-stop-p
                                   (setf (note-tie-stop-p note) t))
                                 (when (or tie-start-p chord-tie-p)
                                   (setf (note-tie-start-p note) t)
                                   (push (pitch-key pitch) (spanner-state-ties (parser-spanners p))))
                                 note)))
                            (nreverse entries))))
      (let ((chord (make-chord notes duration (parser-staff p))))
        (setf (chord-voice chord) (parser-voice p))
        (dolist (n (chord-notes chord))
          (setf (note-voice n) (parser-voice p)))
        (add-implicit-spanner-stops p chord)
        (parse-post-events p chord)))))

(defun parse-time-args (p)
  (let ((a (expect-token p :number))
        (s (expect-token p :slash))
        (b (expect-token p :number)))
    (declare (ignore s))
    (make-time-change (car (token-value a)) (car (token-value b))
                      (parser-staff p))))

;;; \tempo has been consumed.  Accept "\tempo 4 = 120", "\tempo "Allegro" 4 =
;;; 120", or "\tempo "Allegro"" (text only).
(defun parse-tempo-args (p)
  (let ((text nil)
        (beat-unit nil)
        (per-minute nil))
    (when (and (peek-token p) (eq (token-type (peek-token p)) :string))
      (setf text (token-value (advance-token p))))
    (when (and (peek-token p) (eq (token-type (peek-token p)) :number))
      (setf beat-unit (car (token-value (advance-token p))))
      (when (and (peek-token p) (eq (token-type (peek-token p)) :equals))
        (advance-token p)
        (setf per-minute (car (token-value (expect-token p :number))))))
    (make-tempo-change :text text :beat-unit beat-unit :per-minute per-minute
                       :staff (parser-staff p))))

(defun parse-key-args (p)
  (let* ((pitch-tok (expect-token p :pitch))
         (mode-tok (expect-token p :command))
         (mode (token-value mode-tok)))
    (unless (serapeum:in mode :major :minor)
      (recover p 'skip-command "Unsupported key mode \\~A" mode))
    (let ((pt (token-value pitch-tok)))
      (make-key-change (make-pitch (pitch-token-step pt)
                                   :alter (pitch-token-alter pt))
                       mode (parser-staff p)))))

(defun parse-clef-octave-shift (p suffix)
  ;; LilyPond clef octave marks: _8 -> -1, ^8 -> +1, _15 -> -2, etc.
  (when (and (plusp (length suffix)) (char= (char suffix 0) #\0))
    (recover p 'skip-command "Invalid clef octave mark"))
  (let* ((n (parse-integer suffix :junk-allowed t))
         (k (when n (/ (1- n) 7))))
    (when (or (null k) (not (integerp k)))
      (recover p 'skip-command "Unsupported clef octave mark"))
    k))

(defun parse-clef-args (p)
  (let* ((tok (advance-token p))
         (name-str (cond
                     ((eq (token-type tok) :word) (token-value tok))
                     ((eq (token-type tok) :string) (token-value tok))
                     (t (recover p 'skip-command "Expected a clef name"))))
         (shift 0)
         (sep-pos (position-if (lambda (c) (member c '(#\_ #\^))) name-str)))
    (when sep-pos
      (let ((sign (char name-str sep-pos))
            (suffix (subseq name-str (1+ sep-pos))))
        (setf shift (parse-clef-octave-shift p suffix))
        (when (char= sign #\_)
          (setf shift (- shift)))
        (setf name-str (subseq name-str 0 sep-pos))))
    (make-clef-change (intern (string-upcase name-str) "KEYWORD") shift
                      (parser-staff p))))

(defun skip-braced-block (p)
  (expect-token p :brace-open)
  (let ((depth 1))
    (loop
      (let ((tok (advance-token p)))
        (when (null tok)
          (parser-error p "Unterminated braced block"))
        (cond ((eq (token-type tok) :brace-open) (incf depth))
              ((eq (token-type tok) :brace-close) (decf depth))))
      (when (zerop depth) (return)))))

(defun parse-relative-block (p ctx)
  (expect-token p :brace-open)
  (let ((events (parse-events p ctx)))
    (expect-token p :brace-close)
    events))

(defun parse-relative (p)
  ;; \relative has been consumed
  (let ((tok (peek-token p)))
    (if (and tok (eq (token-type tok) :pitch))
        (let ((pt (token-value (advance-token p))))
          (parse-relative-block p
                                (make-rel-ctx (make-pitch (pitch-token-step pt)
                                                          :alter (pitch-token-alter pt)
                                                          :octave (+ 3 (pitch-token-octave-mark pt))))))
        (parse-relative-block p (make-rel-ctx :unset)))))

(defun parse-score (p)
  (expect-token p :brace-open)
  (let ((events (parse-events p nil)))
    (expect-token p :brace-close)
    events))

(defconst +voice-style-commands+
  '(:voiceone :voicetwo :voicethree :voicefour :onevoice
    :voiceonestyle :voicetwostyle :voicethreestyle :voicefourstyle
    :voiceneutralstyle))

(defun consume-voice-style-commands (p)
  "Consume and ignore \\voiceOne..\\oneVoice and style commands."
  (loop while (and (peek-token p)
                   (eq (token-type (peek-token p)) :command)
                   (member (token-value (peek-token p)) +voice-style-commands+))
        do (advance-token p)))

;;; \new has been consumed.  Parse a context: Staff, PianoStaff, or Voice
;;; (each a bare capitalized word), with an optional `= "id"`.  Returns the
;;; parsed events.
(defun parse-new-command (p ctx)
  (let ((type-tok (peek-token p)))
    (unless (and type-tok (eq (token-type type-tok) :word))
      (parser-error p "Expected a context name after \\new"))
    (let ((type (intern (string-upcase (token-value type-tok)) "KEYWORD")))
      (advance-token p)
      ;; Skip an optional `= "id"`.
      (when (and (peek-token p) (eq (token-type (peek-token p)) :equals))
        (advance-token p)
        (when (and (peek-token p) (eq (token-type (peek-token p)) :string))
          (advance-token p)))
      (case type
        (:staff
         (let ((old-staff (parser-staff p)))
           (incf (parser-max-staff p))
           (setf (parser-staff p) (parser-max-staff p))
           (unwind-protect
                (parse-context-body p ctx)
             (setf (parser-staff p) old-staff))))
        (:pianostaff
         (expect-token p :simult-open)
         (parse-simultaneous p ctx))
        (:voice
         (parse-context-body p ctx))
        (t (recover p 'skip-command "Unsupported \\new context \\~A" type))))))

;;; Parse the music body of a \new Staff / \new Voice context: either a braced
;;; block, or a \relative block.
(defun parse-context-body (p ctx)
  (let ((tok (peek-token p)))
    (case (token-type tok)
      (:brace-open
       (advance-token p)
       (prog1 (parse-events p ctx)
         (expect-token p :brace-close)))
      (:command
       (case (token-value tok)
         (:relative (advance-token p) (parse-relative p))
         (t (recover p 'skip-command "Unsupported command \\~A in a context"
                     (token-value tok)))))
      (t (recover p 'skip-event "Expected music after \\new")))))

(defun parse-voice-expression (p ctx voice-number)
  "Parse one voice inside << >>, isolating spanners from other voices."
  (setf (parser-voice p) voice-number)
  (reset-spanner-state p)
  (setf (parser-last-pitch p) nil)
  (setf (parser-last-duration p) nil)
  (consume-voice-style-commands p)
  (let ((tok (peek-token p)))
    (case (token-type tok)
      (:brace-open
       (advance-token p)
       (prog1 (parse-events p ctx)
         (expect-token p :brace-close)))
      (:command
       (case (token-value tok)
         (:relative (advance-token p) (parse-relative p))
         (t (recover p 'skip-command "Unsupported command \\~A in a voice"
                     (token-value tok)))))
      (:pitch (list (parse-note-event p ctx)))
      (:rest (list (parse-rest-event p ctx)))
      (:chord-open (list (parse-chord p ctx)))
      (:number (list (parse-duration-event p ctx)))
      (t (recover p 'skip-event "Expected a music expression in a voice")))))

(defun parse-simultaneous (p ctx)
  "Parse << expr1 \\\\ expr2 ... >> into a flat list of voice/staff-tagged
events.  << has been consumed.  Entries are either voice expressions
separated by \\\\, or \\new Staff / \\new PianoStaff contexts (which each
carry their own staff index)."
  (let ((saved-spanners (copy-spanner-state (parser-spanners p)))
        (saved-pitch (parser-last-pitch p))
        (saved-duration (parser-last-duration p))
        (saved-voice (parser-voice p)))
    (unwind-protect
         (serapeum:collecting
           (let ((voice-number 1)
                 (separator-seen nil)
                 (new-seen nil))
             (loop
               (let* ((entry (parse-simultaneous-entry p ctx voice-number)))
                 (when (eq (car entry) :new)
                   (setf new-seen t))
                 (dolist (e (cdr entry))
                   (collect e)))
               (let ((tok (peek-token p)))
                 (when (null tok)
                   (parser-error p "Unterminated simultaneous music"))
                 (case (token-type tok)
                   (:voice-separator
                    (advance-token p)
                    (setf separator-seen t)
                    (incf voice-number))
                   (:simult-close
                    (advance-token p)
                    (return))
                   (t (unless (and (eq (token-type tok) :command)
                                   (eq (token-value tok) :new))
                        (recover p 'skip-event
                                 "Expected \\\\ or >> in simultaneous music"))))))
             ;; Chord-forming << {a} {b} >> (no \\, no \new) is unsupported.
             (unless (or separator-seen new-seen)
               (recover p 'skip-command
                        "Simultaneous music without \\\\ (chord-forming << >>) is not supported"))))
      (setf (parser-voice p) saved-voice)
      (setf (parser-spanners p) saved-spanners)
      (setf (parser-last-pitch p) saved-pitch)
      (setf (parser-last-duration p) saved-duration))))

;;; Parse one entry inside << >>: either a \\new context (returned as
;;; (:new . events)) or a voice expression on the current staff (returned as
;;; (:voice . events)), so the caller can tell staves apart.
(defun parse-simultaneous-entry (p ctx voice-number)
  (let ((tok (peek-token p)))
    (cond ((and tok (eq (token-type tok) :command)
                (eq (token-value tok) :new))
           (advance-token p)
           (cons :new (parse-new-command p ctx)))
          (t (cons :voice (parse-voice-expression p ctx voice-number))))))

(defun parse-events (p ctx)
  (serapeum:collecting
    (loop do
      (restart-case
          (if-let ((tok (peek-token p)))
            (case (token-type tok)
              (:brace-close (return))
              (:command
               (advance-token p)
               (case (token-value tok)
                 (:relative (dolist (e (parse-relative p)) (collect e)))
                 (:time (collect (parse-time-args p)))
                 (:key (collect (parse-key-args p)))
                 (:clef (collect (parse-clef-args p)))
                 (:tempo (collect (parse-tempo-args p)))
                 (:new (dolist (e (parse-new-command p ctx)) (collect e)))
                 (:header (skip-braced-block p))
                 (:layout (skip-braced-block p))
                 (:paper (skip-braced-block p))
                 (:midi (skip-braced-block p))
                 (:breathe
                  (when (parser-last-event p)
                    (push (make-mark '(:articulation "breath-mark"))
                          (event-attachments (parser-last-event p)))))
                 (:arpeggioarrowup (setf (spanner-state-arpeggio (parser-spanners p)) :up))
                 (:arpeggioarrowdown (setf (spanner-state-arpeggio (parser-spanners p)) :down))
                 (:arpeggionormal (setf (spanner-state-arpeggio (parser-spanners p)) nil))
                 (t (unless (member (token-value tok) +voice-style-commands+)
                      (recover p 'skip-command "Unsupported command \\~A"
                               (token-value tok))))))
              (:simult-open
               (advance-token p)
               (dolist (e (parse-simultaneous p ctx)) (collect e)))
              (:brace-open
               (advance-token p)
               (dolist (e (parse-events p ctx)) (collect e))
               (expect-token p :brace-close))
              (:barline (advance-token p)
                        (collect (make-barline (parser-voice p) (parser-staff p))))
              (:pitch (collect (track-last-event p (parse-note-event p ctx))))
              (:rest (collect (track-last-event p (parse-rest-event p ctx))))
              (:number (collect (track-last-event p (parse-duration-event p ctx))))
              (:chord-open (collect (track-last-event p (parse-chord p ctx))))
              (t (recover p 'skip-event "Unexpected ~S" (token-type tok))))
            (return))
        ;; Recovery: skip the offending construct and keep going.  SKIP-EVENT
        ;; must advance past the unconsumed bad token; SKIP-COMMAND leaves the
        ;; already-consumed command token behind.
        (skip-event () (resync-to-event-start p t))
        (skip-command () (resync-to-event-start p))
        ;; Recovery: stop this events sequence and keep what was parsed so far.
        (abort-parse () (throw 'abort-parse (collect)))))))

(defun parse-top-level-form (p tok)
  (case (token-type tok)
    (:command
     (advance-token p)
     (case (token-value tok)
       (:version (when (eq (token-type (peek-token p)) :string)
                   (advance-token p))
                 nil)
       (:header (skip-braced-block p) nil)
       (:layout (skip-braced-block p) nil)
       (:paper (skip-braced-block p) nil)
       (:midi (skip-braced-block p) nil)
       (:relative (parse-relative p))
       (:score (parse-score p))
       (:new (parse-new-command p nil))
       (t (recover p 'skip-command "Unsupported top-level command \\~A"
                   (token-value tok)))))
    (:brace-open
     (advance-token p)
     (prog1 (parse-events p nil)
       (expect-token p :brace-close)))
    (t (recover p 'skip-event "Unexpected ~S at top level" (token-type tok)))))

(defun parse-file-events (p)
  (catch 'abort-parse
    (serapeum:collecting
      (loop do
        (restart-case
            (if-let ((tok (peek-token p)))
              (dolist (e (parse-top-level-form p tok)) (collect e))
              (return))
          ;; Recovery: skip an unsupported top-level command and its args.
          (skip-command () (resync-command-args p))
          ;; Recovery: skip one unconsumed unexpected token.
          (skip-event () (resync-to-event-start p t))
          ;; Recovery: stop parsing and hand back whatever was parsed so far.
          (abort-parse () (throw 'abort-parse (collect))))))))
