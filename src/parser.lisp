(in-package #:lilylink)

;;; A mutable relative-mode context. REF is either a PITCH object (the
;;; current reference for octave placement) or :UNSET (used by the
;;; `\relative { ... }` form, where the first note is written in absolute
;;; pitch and becomes the reference).
(defstruct (rel-ctx (:constructor make-rel-ctx (&optional (ref :unset))))
  ref)

(defstruct (parser (:constructor make-parser (tokens)))
  tokens
  (pos 0)
  (last-pitch nil)
  (last-duration nil)
  (last-event nil)
  (voice 1)
  (pending-ties nil)
  (pending-slurs nil)
  (pending-wedge nil)
  (pending-glissando nil)
  (pending-trill nil)
  (arpeggio-direction nil))

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
      (lilylink-error-at (token-line-or-0 tok) (token-col-or-0 tok)
                         "Expected ~S but found ~S" type
                         (if tok (token-type tok) 'eof)))
    tok))

(defun parser-error (p fmt &rest args)
  (let ((tok (peek-token p)))
    (lilylink-error-at (token-line-or-0 tok) (token-col-or-0 tok) fmt args)))

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
  (member (pitch-key pitch) (parser-pending-ties p) :test #'equal))

(defun consume-tie (p)
  (let ((tok (peek-token p)))
    (when (and tok (eq (token-type tok) :tie))
      (advance-token p)
      t)))

(defun finish-note (p pitch duration tie-stop-p)
  "Build a NOTE, marking a pending-tie stop and consuming a trailing ~ as start."
  (let ((note (make-instance 'note :pitch pitch :duration duration
                             :voice (parser-voice p))))
    (when tie-stop-p
      (setf (note-tie-stop-p note) t))
    (when (consume-tie p)
      (setf (note-tie-start-p note) t)
      (push (pitch-key pitch) (parser-pending-ties p)))
    note))

;;; Expressive marks attached to a note, rest, or chord via \name commands,
;;; -X shorthand articulations, and ^ / _ (direction, parsed and ignored) or
;;; - prefixes.  The loop stops at the first token that is not an attachment.
(defun parse-attached-mark (p event)
  (let ((tok (peek-token p)))
    (unless (and tok (eq (token-type tok) :command)
                 (lookup-mark (token-value tok)))
      (parser-error p "Expected an expressive mark after the attachment prefix"))
    (let* ((cmd (token-value tok))
           (spec (lookup-mark cmd)))
      (advance-token p)
      (when (member (car spec) '(:dynamic :other-dynamics))
        (close-pending-wedge p event))
      (push (make-mark spec) (event-attachments event)))))

(defun parse-post-events (p event)
  (loop
    (let ((tok (peek-token p)))
      (when (null tok) (return))
      (case (token-type tok)
        (:articulation
         (let* ((tok (advance-token p))
                (spec (lookup-mark (token-value tok))))
           (unless spec
             (parser-error p "Unknown articulation ~S" (token-value tok)))
           (push (make-mark spec) (event-attachments event))))
        (:command
         (let* ((cmd (token-value tok))
                (spec (lookup-mark cmd)))
           (cond (spec
                  (advance-token p)
                  ;; An absolute dynamic terminates an open hairpin.
                  (when (member (car spec) '(:dynamic :other-dynamics))
                    (close-pending-wedge p event))
                  (push (make-mark spec) (event-attachments event)))
                 ((member cmd '(:cr :decr))
                  (advance-token p)
                  (close-pending-wedge p event)
                  (let ((type (if (eq cmd :cr) :crescendo :diminuendo)))
                    (setf (parser-pending-wedge p) (cons type 1))
                    (push (make-wedge 1 type) (event-attachments event))))
                 ((member cmd '(:endcr :enddecr))
                  (advance-token p)
                  (close-pending-wedge p event))
                 ((eq cmd :glissando)
                  (advance-token p)
                  (setf (parser-pending-glissando p) 1)
                  (push (make-glissando 1 :start) (event-attachments event)))
                 ((eq cmd :starttrillspan)
                  (advance-token p)
                  (when-let ((trill (parser-pending-trill p)))
                    (push (make-trill trill :stop) (event-attachments event)))
                  (setf (parser-pending-trill p) 1)
                  (push (make-trill 1 :start) (event-attachments event)))
                 ((eq cmd :stoptrillspan)
                  (advance-token p)
                  (when-let ((trill (parser-pending-trill p)))
                    (push (make-trill trill :stop) (event-attachments event))
                    (setf (parser-pending-trill p) nil)))
                 ((eq cmd :arpeggio)
                  (advance-token p)
                  (push (make-arpeggio (parser-arpeggio-direction p))
                        (event-attachments event))
                  (setf (parser-arpeggio-direction p) nil))
                 ((eq cmd :bendafter)
                  (advance-token p)
                  (parse-bend-args p event))
                 (t (return)))))
        ((:attach-dash :attach-up :attach-down)
         (advance-token p)
         (parse-attached-mark p event))
        (:slur-open
         (advance-token p)
         (let ((number (1+ (length (parser-pending-slurs p)))))
           (push (cons nil number) (parser-pending-slurs p))
           (push (make-slur number :start) (event-attachments event))))
        (:slur-close
         (advance-token p)
         (let ((spec (pop (parser-pending-slurs p))))
           (when spec
             (push (make-slur (cdr spec) :stop) (event-attachments event)))))
        (:phrase-open
         (advance-token p)
         (let ((number (1+ (length (parser-pending-slurs p)))))
           (push (cons t number) (parser-pending-slurs p))
           (push (make-slur number :start t) (event-attachments event))))
        (:phrase-close
         (advance-token p)
         (let ((spec (pop (parser-pending-slurs p))))
           (when spec
             (push (make-slur (cdr spec) :stop t) (event-attachments event)))))
        (:wedge-start
         (advance-token p)
         (close-pending-wedge p event)
         (setf (parser-pending-wedge p) (cons (token-value tok) 1))
         (push (make-wedge 1 (token-value tok)) (event-attachments event)))
        (:wedge-stop
         (advance-token p)
         (close-pending-wedge p event))
        (t (return)))))
  event)

(defun close-pending-wedge (p event)
  "Close the open hairpin, if any, marking EVENT as its stop."
  (when-let ((wedge (parser-pending-wedge p)))
    (push (make-wedge (cdr wedge) :stop) (event-attachments event))
    (setf (parser-pending-wedge p) nil)))

;;; A glissando connects to the immediately following note or chord, so the
;;; next event stops any open glissando.  Trill spans are ended explicitly.
(defun add-implicit-spanner-stops (p event)
  (when-let ((glissando (parser-pending-glissando p)))
    (push (make-glissando glissando :stop) (event-attachments event))
    (setf (parser-pending-glissando p) nil)))

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
    (setf (parser-pending-ties p) nil)
    (let ((note (finish-note p pitch (effective-duration p (pitch-token-duration pt))
                             tie-stop-p)))
      (add-implicit-spanner-stops p note)
      (parse-post-events p note))))

(defun parse-rest-event (p ctx)
  (declare (ignore ctx))
  ;; A rest (or spacer) breaks pending ties and spanners.
  (setf (parser-pending-ties p) nil)
  (setf (parser-pending-glissando p) nil)
  (setf (parser-pending-trill p) nil)
  (let* ((tok (advance-token p))
         (duration (effective-duration p (token-value tok))))
    (let ((rest (make-rest duration)))
      (setf (rest-voice rest) (parser-voice p))
      (parse-post-events p rest))))

(defun parse-duration-event (p ctx)
  (declare (ignore ctx))
  (unless (parser-last-pitch p)
    (parser-error p "Duration with no preceding note"))
  (let* ((tok (advance-token p))
         (val (token-value tok))
         (duration (setf (parser-last-duration p)
                         (make-duration (duration-log-from-num (car val))
                                        :dots (cdr val))))
         (pitch (parser-last-pitch p))
         (tie-stop-p (pitch-in-pending p pitch)))
    (setf (parser-pending-ties p) nil)
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
          (parser-error p "Expected a pitch inside a chord"))
        (let* ((tok (advance-token p))
               (pitch (resolve-pitch-token p ctx (token-value tok)))
               (tie-stop-p (pitch-in-pending p pitch))
               (tie-start-p (consume-tie p)))
          (unless first-pitch (setf first-pitch pitch))
          (push (list pitch tie-stop-p tie-start-p) entries))))
    (expect-token p :chord-close)
    ;; The chord is fully matched: drop any pending ties it did not consume.
    (setf (parser-pending-ties p) nil)
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
                                                          :duration duration)))
                                (when tie-stop-p
                                  (setf (note-tie-stop-p note) t))
                                (when (or tie-start-p chord-tie-p)
                                  (setf (note-tie-start-p note) t)
                                  (push (pitch-key pitch) (parser-pending-ties p)))
                                note)))
                           (nreverse entries))))
      (let ((chord (make-chord notes duration)))
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
    (make-time-change (car (token-value a)) (car (token-value b)))))

(defun parse-key-args (p)
  (let* ((pitch-tok (expect-token p :pitch))
         (mode-tok (expect-token p :command))
         (mode (token-value mode-tok)))
    (unless (member mode '(:major :minor))
      (lilylink-error-at (token-line mode-tok) (token-col mode-tok)
                         "Unsupported key mode \\~A" mode))
    (let ((pt (token-value pitch-tok)))
      (make-key-change (make-pitch (pitch-token-step pt)
                                   :alter (pitch-token-alter pt))
                       mode))))

(defun parse-clef-octave-shift (suffix tok)
  ;; LilyPond clef octave marks: _8 -> -1, ^8 -> +1, _15 -> -2, etc.
  (when (and (plusp (length suffix)) (char= (char suffix 0) #\0))
    (lilylink-error-at (token-line tok) (token-col tok)
                       "Invalid clef octave mark"))
  (let* ((n (parse-integer suffix :junk-allowed t))
         (k (when n (/ (1- n) 7))))
    (when (or (null k) (not (integerp k)))
      (lilylink-error-at (token-line tok) (token-col tok)
                         "Unsupported clef octave mark"))
    k))

(defun parse-clef-args (p)
  (let* ((tok (advance-token p))
         (name-str (cond
                     ((eq (token-type tok) :word) (token-value tok))
                     ((eq (token-type tok) :string) (token-value tok))
                     (t (parser-error p "Expected a clef name"))))
         (shift 0)
         (sep-pos (position-if (lambda (c) (member c '(#\_ #\^))) name-str)))
    (when sep-pos
      (let ((sign (char name-str sep-pos))
            (suffix (subseq name-str (1+ sep-pos))))
        (setf shift (parse-clef-octave-shift suffix tok))
        (when (char= sign #\_)
          (setf shift (- shift)))
        (setf name-str (subseq name-str 0 sep-pos))))
    (make-clef-change (intern (string-upcase name-str) "KEYWORD") shift)))

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

(defparameter +voice-style-commands+
  '(:voiceone :voicetwo :voicethree :voicefour :onevoice
    :voiceonestyle :voicetwostyle :voicethreestyle :voicefourstyle
    :voiceneutralstyle))

(defun consume-voice-style-commands (p)
  "Consume and ignore \\voiceOne..\\oneVoice and style commands."
  (loop while (and (peek-token p)
                   (eq (token-type (peek-token p)) :command)
                   (member (token-value (peek-token p)) +voice-style-commands+))
        do (advance-token p)))

(defun parse-voice-expression (p ctx voice-number)
  "Parse one voice inside << >>, isolating spanners from other voices."
  (setf (parser-voice p) voice-number)
  (setf (parser-pending-ties p) nil)
  (setf (parser-pending-slurs p) nil)
  (setf (parser-pending-wedge p) nil)
  (setf (parser-pending-glissando p) nil)
  (setf (parser-pending-trill p) nil)
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
         (t (parser-error p "Unsupported command \\~A in a voice" (token-value tok)))))
      (:pitch (list (parse-note-event p ctx)))
      (:rest (list (parse-rest-event p ctx)))
      (:chord-open (list (parse-chord p ctx)))
      (:number (list (parse-duration-event p ctx)))
      (t (parser-error p "Expected a music expression in a voice")))))

(defun parse-simultaneous (p ctx)
  "Parse << expr1 \\\\ expr2 ... >> into a flat list of voice-tagged events.
<< has been consumed."
  (let ((saved-ties (parser-pending-ties p))
        (saved-slurs (parser-pending-slurs p))
        (saved-wedge (parser-pending-wedge p))
        (saved-glissando (parser-pending-glissando p))
        (saved-trill (parser-pending-trill p))
        (saved-pitch (parser-last-pitch p))
        (saved-duration (parser-last-duration p)))
    (unwind-protect
         (let ((voices nil)
               (voice-number 1)
               (separator-seen nil))
           (loop
             (push (parse-voice-expression p ctx voice-number) voices)
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
                 (t (parser-error p "Expected \\\\ or >> in simultaneous music")))))
           (unless separator-seen
             (parser-error p "Simultaneous music without \\\\ (chord-forming << >>) is not supported"))
           (apply #'nconc (nreverse voices)))
      (setf (parser-voice p) 1)
      (setf (parser-pending-ties p) saved-ties)
      (setf (parser-pending-slurs p) saved-slurs)
      (setf (parser-pending-wedge p) saved-wedge)
      (setf (parser-pending-glissando p) saved-glissando)
      (setf (parser-pending-trill p) saved-trill)
      (setf (parser-last-pitch p) saved-pitch)
      (setf (parser-last-duration p) saved-duration))))

(defun parse-events (p ctx)
  (let ((events nil))
    (loop
      (if-let ((tok (peek-token p)))
        (case (token-type tok)
          (:brace-close (return))
          (:command
           (advance-token p)
           (case (token-value tok)
             (:relative (setf events (nreconc (parse-relative p) events)))
             (:time (push (parse-time-args p) events))
             (:key (push (parse-key-args p) events))
             (:clef (push (parse-clef-args p) events))
             (:header (skip-braced-block p))
             (:layout (skip-braced-block p))
             (:paper (skip-braced-block p))
             (:midi (skip-braced-block p))
             (:breathe
              (when (parser-last-event p)
                (push (make-mark '(:articulation "breath-mark"))
                      (event-attachments (parser-last-event p)))))
             (:arpeggioarrowup (setf (parser-arpeggio-direction p) :up))
             (:arpeggioarrowdown (setf (parser-arpeggio-direction p) :down))
             (:arpeggionormal (setf (parser-arpeggio-direction p) nil))
             (t (unless (member (token-value tok) +voice-style-commands+)
                  (parser-error p "Unsupported command \\~A" (token-value tok))))))
          (:simult-open
           (advance-token p)
           (setf events (nreconc (parse-simultaneous p ctx) events)))
          (:brace-open
           (advance-token p)
           (prog1 (setf events (nreconc (parse-events p ctx) events))
             (expect-token p :brace-close)))
          (:barline (advance-token p) (push (make-barline (parser-voice p)) events))
          (:pitch (let ((note (parse-note-event p ctx)))
                    (setf (parser-last-event p) note)
                    (push note events)))
          (:rest (let ((rest (parse-rest-event p ctx)))
                   (setf (parser-last-event p) rest)
                   (push rest events)))
          (:number (let ((note (parse-duration-event p ctx)))
                     (setf (parser-last-event p) note)
                     (push note events)))
          (:chord-open (let ((chord (parse-chord p ctx)))
                         (setf (parser-last-event p) chord)
                         (push chord events)))
          (t (parser-error p "Unexpected ~S" (token-type tok))))
        (return)))
    (nreverse events)))

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
       (t (parser-error p "Unsupported top-level command \\~A" (token-value tok)))))
    (:brace-open
     (advance-token p)
     (prog1 (parse-events p nil)
       (expect-token p :brace-close)))
    (t (parser-error p "Unexpected ~S at top level" (token-type tok)))))

(defun parse-file-events (p)
  (let ((events nil))
    (loop
      (if-let ((tok (peek-token p)))
        (setf events (nreconc (parse-top-level-form p tok) events))
        (return (nreverse events))))))
