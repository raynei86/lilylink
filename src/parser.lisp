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
  (pending-ties nil))

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
  (let ((note (make-instance 'note :pitch pitch :duration duration)))
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
           (if spec
               (progn (advance-token p)
                      (push (make-mark spec) (event-attachments event)))
               (return))))
        ((:attach-dash :attach-up :attach-down)
         (advance-token p)
         (parse-attached-mark p event))
        (t (return)))))
  event)

(defun parse-note-event (p ctx)
  (let* ((tok (advance-token p)))
    (destructuring-bind (step alter mark duration) (token-value tok)
      (let* ((pitch (resolve-pitch p ctx step alter mark))
             (tie-stop-p (pitch-in-pending p pitch)))
        (setf (parser-pending-ties p) nil)
        (parse-post-events
         p (finish-note p pitch (effective-duration p duration) tie-stop-p))))))

(defun parse-rest-event (p ctx)
  (declare (ignore ctx))
  ;; A rest (or spacer) breaks any pending tie: the next note-head cannot
  ;; connect to a tie across a rest, so drop it (mirroring LilyPond).
  (setf (parser-pending-ties p) nil)
  (let* ((tok (advance-token p))
         (duration (effective-duration p (token-value tok))))
    (parse-post-events p (make-rest duration))))

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
    (parse-post-events p (finish-note p pitch duration tie-stop-p))))

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
               (pitch (destructuring-bind (step alter mark duration) (token-value tok)
                        (declare (ignore duration))
                        (resolve-pitch p ctx step alter mark)))
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
      (parse-post-events p (make-chord notes duration)))))

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
    (destructuring-bind (step alter mark duration) (token-value pitch-tok)
      (declare (ignore mark duration))
      (make-key-change (make-pitch step :alter alter) mode))))

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
        (destructuring-bind (step alter mark duration) (token-value (advance-token p))
          (declare (ignore duration))
          (parse-relative-block p
                                (make-rel-ctx (make-pitch step :alter alter
                                                          :octave (+ 3 mark)))))
        (parse-relative-block p (make-rel-ctx :unset)))))

(defun parse-score (p)
  (expect-token p :brace-open)
  (let ((events (parse-events p nil)))
    (expect-token p :brace-close)
    events))

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
             (t (parser-error p "Unsupported command \\~A" (token-value tok)))))
          (:brace-open
           (advance-token p)
           (prog1 (setf events (nreconc (parse-events p ctx) events))
             (expect-token p :brace-close)))
          (:barline (advance-token p) (push (make-barline) events))
          (:pitch (push (parse-note-event p ctx) events))
          (:rest (push (parse-rest-event p ctx) events))
          (:number (push (parse-duration-event p ctx) events))
          (:chord-open (push (parse-chord p ctx) events))
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
