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
  (last-duration nil))

(defun parse-music (string)
  "Parse a LilyPond source string into a list of events."
  (parse-file-events (make-parser (coerce (tokenize string) 'vector))))

(defun peek-token (p)
  (when (< (parser-pos p) (length (parser-tokens p)))
    (aref (parser-tokens p) (parser-pos p))))

(defun advance-token (p)
  (prog1 (peek-token p)
    (incf (parser-pos p))))

(defun expect-token (p type)
  (let ((tok (advance-token p)))
    (unless (and tok (eq (token-type tok) type))
      (lilylink-error-at (if tok (token-line tok) 0) (if tok (token-col tok) 0)
                         "Expected ~S but found ~S" type
                         (if tok (token-type tok) 'eof)))
    tok))

(defun parser-error (p fmt &rest args)
  (let ((tok (peek-token p)))
    (lilylink-error-at (if tok (token-line tok) 0) (if tok (token-col tok) 0)
                       fmt args)))

;;; Relative octave placement: choose the octave that minimizes the
;;; diatonic interval (ignoring accidentals) between the target step and
;;; the reference pitch.
(defun relative-octave (ref-num target-step)
  (round (/ (- ref-num target-step) 7)))

(defun resolve-pitch (p ctx step alter net)
  "Resolve STEP/ALTER/NET (net octave marks) against CTX into a PITCH object.
CTX is a REL-CTX or NIL (absolute mode)."
  (let ((pitch
          (if (null ctx)
              (make-pitch step :alter alter :octave (+ 3 net))
              (let ((ref (rel-ctx-ref ctx)))
                (if (eq ref :unset)
                    (make-pitch step :alter alter :octave (+ 3 net))
                    (make-pitch step :alter alter
                                :octave (+ (relative-octave (pitch-num ref) step)
                                           net)))))))
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

(defun parse-note-event (p ctx)
  (let* ((tok (advance-token p))
         (parts (token-value tok))
         (pitch (resolve-pitch p ctx (nth 0 parts) (nth 1 parts) (nth 2 parts)))
         (duration (effective-duration p (nth 3 parts))))
    (make-instance 'note :pitch pitch :duration duration)))

(defun parse-rest-event (p ctx)
  (declare (ignore ctx))
  (let* ((tok (advance-token p))
         (duration (effective-duration p (token-value tok))))
    (make-rest duration)))

(defun parse-duration-event (p ctx)
  (declare (ignore ctx))
  (unless (parser-last-pitch p)
    (parser-error p "Duration with no preceding note"))
  (let* ((tok (advance-token p))
         (val (token-value tok))
         (duration (setf (parser-last-duration p)
                         (make-duration (duration-log-from-num (car val))
                                        :dots (cdr val)))))
    (make-instance 'note :pitch (parser-last-pitch p) :duration duration)))

(defun parse-chord (p ctx)
  (advance-token p)  ; consume <
  (let ((notes nil)
        (first-pitch nil))
    (loop
      (let ((tok (peek-token p)))
        (when (or (null tok) (eq (token-type tok) :chord-close)) (return))
        (unless (eq (token-type tok) :pitch)
          (parser-error p "Expected a pitch inside a chord"))
        (let* ((tok (advance-token p))
               (parts (token-value tok))
               (pitch (resolve-pitch p ctx (nth 0 parts) (nth 1 parts) (nth 2 parts))))
          (unless first-pitch (setf first-pitch pitch))
          (push pitch notes))))
    (expect-token p :chord-close)
    ;; The reference for anything following the chord is its first note.
    (when (and ctx first-pitch)
      (setf (rel-ctx-ref ctx) first-pitch))
    (when first-pitch
      (setf (parser-last-pitch p) first-pitch))
    ;; Optional adjacent duration after `>`.
    (let ((tok (peek-token p)))
      (when (and tok (eq (token-type tok) :number))
        (let* ((val (token-value (advance-token p)))
               (duration (make-duration (duration-log-from-num (car val))
                                        :dots (cdr val))))
          (setf (parser-last-duration p) duration))))
    (let ((duration (or (parser-last-duration p) (make-duration 2))))
      (make-chord (mapcar (lambda (pt) (make-instance 'note :pitch pt :duration duration))
                          (nreverse notes))
                  duration))))

(defun parse-time-args (p)
  (let ((a (expect-token p :number))
        (s (expect-token p :slash))
        (b (expect-token p :number)))
    (declare (ignore s))
    (list :time (car (token-value a)) (car (token-value b)))))

(defun parse-key-args (p)
  (let* ((pitch-tok (expect-token p :pitch))
         (mode-tok (expect-token p :command))
         (parts (token-value pitch-tok))
         (mode (token-value mode-tok)))
    (unless (member mode '(:major :minor))
      (lilylink-error-at (token-line mode-tok) (token-col mode-tok)
                         "Unsupported key mode \\~A" mode))
    (list :key (make-pitch (nth 0 parts) :alter (nth 1 parts))
          mode)))

(defun parse-clef-octave-shift (suffix)
  ;; LilyPond clef octave marks: _8 -> -1, ^8 -> +1, _15 -> -2, etc.
  (when (and (plusp (length suffix)) (char= (char suffix 0) #\0))
    (lilylink-error-at 0 0 "Invalid clef octave mark"))
  (let* ((n (parse-integer suffix :junk-allowed t))
         (k (when n (/ (1- n) 7))))
    (when (or (null k) (not (integerp k)))
      (lilylink-error-at 0 0 "Unsupported clef octave mark"))
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
        (setf shift (parse-clef-octave-shift suffix))
        (when (char= sign #\_)
          (setf shift (- shift)))
        (setf name-str (subseq name-str 0 sep-pos))))
    (list :clef (intern (string-upcase name-str) "KEYWORD") shift)))

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

(defun parse-relative-block (p outer-ctx ctx)
  (declare (ignore outer-ctx))
  (expect-token p :brace-open)
  (let ((events (parse-events p ctx)))
    (expect-token p :brace-close)
    events))

(defun parse-relative (p outer-ctx)
  ;; \relative has been consumed
  (let ((tok (peek-token p)))
    (if (and tok (eq (token-type tok) :pitch))
        (let* ((parts (token-value (advance-token p)))
               (start (make-pitch (nth 0 parts) :alter (nth 1 parts)
                                  :octave (+ 3 (nth 2 parts)))))
          (parse-relative-block p outer-ctx (make-rel-ctx start)))
        (parse-relative-block p outer-ctx (make-rel-ctx :unset)))))

(defun parse-score (p)
  (expect-token p :brace-open)
  (let ((events (parse-events p nil)))
    (expect-token p :brace-close)
    events))

(defun parse-events (p ctx)
  (let ((events nil))
    (loop
      (let ((tok (peek-token p)))
        (when (null tok) (return))
        (case (token-type tok)
          (:brace-close (return))
          (:command
           (advance-token p)
           (case (token-value tok)
             (:relative (setf events (append (reverse (parse-relative p ctx)) events)))
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
           (prog1 (setf events (append (reverse (parse-events p ctx)) events))
             (expect-token p :brace-close)))
          (:barline (advance-token p) (push (list :barline) events))
          (:pitch (push (parse-note-event p ctx) events))
          (:rest (push (parse-rest-event p ctx) events))
          (:number (push (parse-duration-event p ctx) events))
          (:chord-open (push (parse-chord p ctx) events))
          (t (parser-error p "Unexpected ~S" (token-type tok))))))
    (nreverse events)))

(defun parse-file-events (p)
  (let ((events nil))
    (loop
      (let ((tok (peek-token p)))
        (when (null tok) (return events))
        (setf events (append events (parse-top-level-form p tok)))))))

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
       (:relative (parse-relative p nil))
       (:score (parse-score p))
       (t (parser-error p "Unsupported top-level command \\~A" (token-value tok)))))
    (:brace-open
     (advance-token p)
     (prog1 (parse-events p nil)
       (expect-token p :brace-close)))
    (t (parser-error p "Unexpected ~S at top level" (token-type tok)))))
