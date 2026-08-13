(in-package :lilylink/tests/main)

(defun pitch-string (e)
  (cond ((typep e 'lilylink::note)
         (format nil "~A~D" (lilylink::pitch-step-letter
                             (lilylink::pitch-step (lilylink::note-pitch e)))
                 (lilylink::pitch-octave (lilylink::note-pitch e))))
        ((typep e 'lilylink::chord)
         (format nil "[~{~A~^ ~}]"
                 (mapcar (lambda (n)
                           (format nil "~A~D"
                                   (lilylink::pitch-step-letter
                                    (lilylink::pitch-step (lilylink::note-pitch n)))
                                   (lilylink::pitch-octave (lilylink::note-pitch n))))
                         (lilylink::chord-notes e))))
        ((typep e 'lilylink::rest-event) "r")
        ((typep e 'lilylink::barline) "|")
        (t "cmd")))

(defun pitch-sequence (src)
  (mapcar #'pitch-string (lilylink:parse-music src)))

(deftest parse-relative-octaves
  (testing "ascending scale stays in octave, wraps correctly"
    (ok (equal (pitch-sequence "\\relative c' { c d e f g a b c | g c | c, g'' }")
               '("c4" "d4" "e4" "f4" "g4" "a4" "b4" "c5" "|" "g4" "c5" "|" "c4" "g5"))))
  (testing "the closer interval wins (a fourth below beats a fifth above)"
    (ok (equal (pitch-sequence "\\relative c' { c g }") '("c4" "g3"))))
  (testing "accidentals do not affect octave placement"
    (ok (equal (pitch-sequence "\\relative c'' { c2 ges }") '("c5" "g4")))))

(deftest parse-relative-chords
  (testing "chord notes are relative to each other; ref resets to first note"
    (ok (equal (pitch-sequence "\\relative { c' <c e g> <c' e g'> <c, e, g''> }")
               '("c4" "[c4 e4 g4]" "[c5 e5 g6]" "[c4 e3 g5]")))))

(deftest parse-relative-no-start-pitch
  (testing "first note is absolute without a start pitch"
    (ok (equal (pitch-sequence "\\relative { c g }") '("c3" "g2"))))
  (testing "with start pitch the first note is relative"
    (ok (equal (pitch-sequence "\\relative c' { g }") '("g3")))))

(deftest parse-absolute-mode
  (testing "bare braces use absolute octaves"
    (ok (equal (pitch-sequence "{ c'4 d' }") '("c4" "d4")))))

(deftest parse-durations
  (testing "isolated durations reuse the previous pitch"
    (ok (equal (pitch-sequence "\\relative c' { c4 8 8 }") '("c4" "c4" "c4"))))
  (testing "durations carry through to following notes"
    (let ((events (lilylink:parse-music "\\relative c' { c8 d }")))
      (ok (every (lambda (e)
                   (and (typep e 'lilylink::note)
                        (eq (lilylink::duration-log (lilylink::note-duration e)) 3)))
                 events)))))

(deftest parse-rests
  (testing "rests do not affect relative octaves"
    (ok (equal (pitch-sequence "\\relative c' { c4 r4 d4 }") '("c4" "r" "d4")))))

(deftest parse-relative-nested-blocks
  (testing "nested relative blocks do not affect the enclosing reference"
    (ok (equal (pitch-sequence "\\relative c' { c4 \\relative { d' e f } g4 }")
               '("c4" "d4" "e4" "f4" "g3"))))
  (testing "nested relative blocks resolve their own octaves"
    (ok (equal (pitch-sequence "\\relative c' { c4 \\relative { d' e f } }")
               '("c4" "d4" "e4" "f4")))))

(defun tie-seq (src)
  (mapcar (lambda (e)
            (labels ((one (n)
                       (format nil "~A~A" (pitch-string n)
                               (cond ((and (lilylink::note-tie-start-p n)
                                           (lilylink::note-tie-stop-p n))
                                      "~-")
                                     ((lilylink::note-tie-start-p n) "~")
                                     ((lilylink::note-tie-stop-p n) "-")
                                     (t "")))))
              (cond ((typep e 'lilylink::note) (one e))
                    ((typep e 'lilylink::chord)
                     (format nil "[~{~A~^ ~}]"
                             (mapcar #'one (lilylink::chord-notes e))))
                    (t (pitch-string e)))))
          (lilylink:parse-music src)))

(deftest parse-ties
  (testing "a tilde ties a note to the next same-pitch note"
    (ok (equal (tie-seq "\\relative c' { c4~ c4 }") '("c4~" "c4-"))))
  (testing "tie chains produce stop+start on the middle note"
    (ok (equal (tie-seq "\\relative c' { c4~ c4~ c4 }")
               '("c4~" "c4~-" "c4-"))))
  (testing "ties work with isolated durations"
    (ok (equal (tie-seq "\\relative c' { a2~ 4 }") '("a3~" "a3-"))))
  (testing "whole-chord ties tie matching pitches"
    (ok (equal (tie-seq "\\relative c' { <c e>4~ <c e>4 }")
               '("[c4~ e4~]" "[c4- e4-]"))))
  (testing "chord-internal ties tie only the marked note"
    (ok (equal (tie-seq "\\relative c' { <c~ e>4 <c e>4 }")
               '("[c4~ e4]" "[c4- e4]"))))
  (testing "unmatched ties are dropped without leaking"
    (ok (equal (tie-seq "\\relative c' { c4~ d4 c4 }")
               '("c4~" "d4" "c4"))))
  (testing "a rest breaks a pending tie"
    (ok (equal (tie-seq "\\relative c' { c4~ r4 c4 }")
               '("c4~" "r" "c4"))))
  (testing "a spacer rest breaks a pending tie"
    (ok (equal (tie-seq "\\relative c' { c4~ s4 c4 }")
               '("c4~" "r" "c4")))))

(defun event-marks (e)
  (mapcar #'lilylink::mark-tag (lilylink::event-attachments e)))

(defun attach-string (a)
  (cond ((typep a 'lilylink::slur)
         (format nil "~A~A~D"
                 (if (lilylink::slur-phrase-p a) "p" "s")
                 (string-downcase (lilylink::slur-action a))
                 (lilylink::slur-number a)))
        ((typep a 'lilylink::wedge)
         (format nil "w~A" (string-downcase (lilylink::wedge-type a))))
        ((typep a 'lilylink::glissando)
         (format nil "g~A" (string-downcase (lilylink::glissando-action a))))
        ((typep a 'lilylink::trill)
         (format nil "t~A" (string-downcase (lilylink::trill-action a))))
        ((typep a 'lilylink::arpeggio)
         (if (lilylink::arpeggio-direction a)
             (format nil "arp~A" (string-downcase (lilylink::arpeggio-direction a)))
             "arp"))
        (t (lilylink::mark-tag a))))

(defun attach-seq (e)
  (mapcar #'attach-string (lilylink::event-attachments e)))

(deftest parse-spanners
  (testing "slur start and stop attach to notes"
    (let ((events (lilylink:parse-music "\\relative c' { c4( d e) }")))
      (ok (equal (mapcar #'attach-seq events)
                 '(("sstart1") nil ("sstop1"))))))
  (testing "nested and phrasing slurs get distinct numbers"
    (let ((events (lilylink:parse-music "\\relative c' { c4\\( d4( e4) f4\\) }")))
      (ok (equal (mapcar #'attach-seq events)
                 '(("pstart1") ("sstart2") ("sstop2") ("pstop1"))))))
  (testing "hairpins start and stop"
    (let ((events (lilylink:parse-music "\\relative c' { c4\\< d\\! }")))
      (ok (equal (mapcar #'attach-seq events) '(("wcrescendo") ("wstop"))))))
  (testing "an absolute dynamic closes an open hairpin"
    (let ((events (lilylink:parse-music "\\relative c' { c4\\< d\\f }")))
      (ok (equal (mapcar #'attach-seq events) '(("wcrescendo") ("f" "wstop")))))))

(deftest parse-lines
  (testing "glissando connects to the next note"
    (let ((events (lilylink:parse-music "\\relative c' { g2\\glissando g'4 }")))
      (ok (equal (mapcar #'attach-seq events) '(("gstart") ("gstop"))))))
  (testing "trill spans start and stop"
    (let ((events (lilylink:parse-music "\\relative c' { d1\\startTrillSpan d1 c2\\stopTrillSpan }")))
      (ok (equal (mapcar #'attach-seq events) '(("tstart") nil ("tstop"))))))
  (testing "arpeggio attaches to a chord"
    (let ((events (lilylink:parse-music "\\relative c' { <c e g>1\\arpeggio }")))
      (ok (equal (attach-seq (first events)) '("arp")))))
  (testing "breathe attaches a breath mark to the preceding note"
    (let ((events (lilylink:parse-music "\\relative c' { c2 \\breathe d4 }")))
      (ok (equal (mapcar #'attach-seq events) '(("breath-mark") nil)))))
  (testing "bendAfter maps sign to doit/falloff"
    (let ((events (lilylink:parse-music "\\relative c' { c2\\bendAfter 4 d2\\bendAfter -4 }")))
      (ok (equal (mapcar #'attach-seq events) '(("doit") ("falloff")))))))

(deftest parse-marks
  (testing "articulation and dynamic commands attach marks"
    (let* ((events (lilylink:parse-music "\\relative c' { c4\\staccato d\\mf }"))
           (marks (mapcar #'event-marks events)))
      (ok (equal marks '(("staccato") ("mf"))))))
  (testing "shorthand articulations"
    (let* ((events (lilylink:parse-music "\\relative c' { c4-. d-> e-^ }"))
           (marks (mapcar #'event-marks events)))
      (ok (equal marks '(("staccato") ("accent") ("strong-accent"))))))
  (testing "direction prefixes are parsed but ignored"
    (let* ((events (lilylink:parse-music "\\relative c' { c4^\\f d_\\p }"))
           (marks (mapcar #'event-marks events)))
      (ok (equal marks '(("f") ("p"))))))
  (testing "rests and chords carry marks"
    (let* ((events (lilylink:parse-music "\\relative c' { r4\\fermata <c e>4\\mf }"))
           (marks (mapcar #'event-marks events)))
      (ok (equal marks '(("fermata") ("mf"))))))
  (testing "ornament and technical names map by lookup"
    (let* ((events (lilylink:parse-music "\\relative c' { c4\\mordent d\\prall e\\upbow }"))
           (marks (mapcar #'event-marks events)))
      (ok (equal marks '(("inverted-mordent") ("mordent") ("up-bow")))))))

(defun voice-seq (src)
  (mapcar (lambda (e)
            (cond ((typep e 'lilylink::note)
                   (format nil "~A/~D"
                           (lilylink::pitch-step-letter
                            (lilylink::pitch-step (lilylink::note-pitch e)))
                           (lilylink::note-voice e)))
                  ((typep e 'lilylink::rest-event) "r")
                  (t (format nil "~S" (type-of e)))))
          (lilylink:parse-music src)))

(deftest parse-voices
  (testing "two voices are tagged in order"
    (ok (equal (voice-seq "\\relative c' { << { c4 d } \\\\ { e4 f } >> }")
               '("c/1" "d/1" "e/2" "f/2"))))
  (testing "three voices"
    (ok (equal (voice-seq "\\relative c' { << { c4 } \\\\ { d4 } \\\\ { e4 } >> }")
               '("c/1" "d/2" "e/3"))))
  (testing "voice style commands are consumed and ignored"
    (ok (equal (voice-seq "\\relative c' { << { \\voiceOne c4 } \\\\ { \\voiceTwo d4 } >> }")
               '("c/1" "d/2"))))
  (testing "spanners do not cross voices"
    (let ((events (lilylink:parse-music "\\relative c' { << { c4~ } \\\\ { c4 } >> }")))
      (ok (lilylink::note-tie-start-p (first events)))
      (ok (not (lilylink::note-tie-stop-p (second events))))))
  (testing "simultaneous music without \\\\ is rejected in strict mode"
    (ok (handler-case (progn (let ((lilylink:*strict-mode* t))
                               (lilylink:parse-music "\\relative c' { << { c4 d } >> }"))
                              nil)
          (lilylink:lilylink-parse-error () t)))))

(defun measure-seq (src)
  (let* ((score (lilylink:build-score (lilylink:parse-music src)))
         (staff (first (lilylink::score-staves score))))
    (mapcar (lambda (m)
              (mapcar (lambda (e)
                        (cond ((typep e 'lilylink::note)
                               (format nil "~A~D/~D"
                                       (lilylink::pitch-step-letter
                                        (lilylink::pitch-step (lilylink::note-pitch e)))
                                       (lilylink::pitch-octave (lilylink::note-pitch e))
                                       (lilylink::note-voice e)))
                              ((typep e 'lilylink::rest-event)
                               (format nil "r/~D" (lilylink::rest-voice e)))
                              (t "?")))
                      (lilylink::measure-events m)))
            (lilylink::staff-measures staff))))

(deftest build-voices-measures
  (testing "two voices share a measure, grouped by voice"
    (ok (equal (measure-seq "\\relative c' { << { c4 d e f } \\\\ { g2 a2 } >> }")
               '(("c4/1" "d4/1" "e4/1" "f4/1" "g4/2" "a4/2")))))
  (testing "voices continue after the simultaneous block"
    (ok (equal (measure-seq "\\relative c' { << { c1 } \\\\ { e1 } >> c1 }")
               '(("c4/1" "e4/2") ("c4/1")))))
  (testing "cross-barline split happens per voice"
    (ok (equal (measure-seq "\\relative c' { \\time 3/4 << { c2 c4 } \\\\ { e2. } >> }")
               '(("c4/1" "c4/1" "e4/2"))))))

(deftest decompose-durations
  (testing "decompose-units produces dyadic-dotted pieces"
    (ok (equal (lilylink::decompose-units 6 4) '((2 . 1))))
    (ok (equal (lilylink::decompose-units 14 4) '((1 . 2))))
    (ok (equal (lilylink::decompose-units 5 4) '((2 . 0) (4 . 0)))))
  (testing "measure-chunks fits values across measures"
    (ok (equal (lilylink::measure-chunks 6 4 6) '(4 2)))
    (ok (equal (lilylink::measure-chunks 4 6 6) '(4)))))

(deftest parse-measure-auto-split
  (testing "events are split into measures by the time signature"
    (let* ((score (lilylink:build-score (lilylink:parse-music "\\relative c' { \\time 4/4 c1 c1 }")))
           (staff (first (lilylink::score-staves score))))
      (ok (= 2 (length (lilylink::staff-measures staff)))))))

(deftest parse-errors
  (testing "unsupported commands are skipped by default"
    (ok (handler-case (progn (lilylink:parse-music "\\transpose c d { c }") t)
          (lilylink:lilylink-parse-error () nil))))
  (testing "unsupported commands signal errors in strict mode"
    (ok (handler-case (progn (let ((lilylink:*strict-mode* t))
                               (lilylink:parse-music "\\transpose c d { c }"))
                              nil)
          (lilylink:lilylink-parse-error () t))))
  (testing "isolated duration without a preceding note is skipped by default"
    (ok (handler-case (progn (lilylink:parse-music "{ 4 }") t)
          (lilylink:lilylink-parse-error () nil))))
  (testing "isolated duration signals an error in strict mode"
    (ok (handler-case (progn (let ((lilylink:*strict-mode* t))
                               (lilylink:parse-music "{ 4 }"))
                              nil)
          (lilylink:lilylink-parse-error () t))))
  (testing "durations beyond the supported maximum are rejected cleanly"
    (ok (handler-case (progn (lilylink:parse-music "{ c2048 }") nil)
          (lilylink:lilylink-parse-error () t))))
  (testing "format arguments are not double-wrapped in error messages"
    (let ((msg (handler-case (let ((lilylink:*strict-mode* t))
                               (lilylink:parse-music "{ \\transpose }"))
                 (lilylink:lilylink-parse-error (c)
                   (lilylink:parse-error-message c)))))
      (ok (equal msg "Unsupported command \\TRANSPOSE"))))
  (testing "recoverable problems warn by default"
    (let ((warnings nil))
      (handler-bind
          ((lilylink:lilylink-warning
             (lambda (c) (push (lilylink:warning-message c) warnings)
               (muffle-warning c))))
        (lilylink:parse-music "{ c4 \\transpose d4 }"))
      (ok (= 1 (length warnings)))
      (ok (search "Unsupported command" (first warnings)))))
  (testing "parse errors are caught as lilylink-error"
    (ok (handler-case (progn (let ((lilylink:*strict-mode* t))
                               (lilylink:parse-music "{ \\transpose }"))
                              nil)
          (lilylink:lilylink-error () t))))
  (testing "emit errors are caught as lilylink-error"
    (ok (handler-case (lilylink::emit-error "Cannot emit event ~S" 'foo)
          (lilylink:lilylink-error () t))))
  (testing "emit errors are not parse errors"
    (ok (eq (handler-case (lilylink::emit-error "boom")
              (lilylink:lilylink-parse-error () :parse)
              (lilylink:lilylink-error () :generic))
            :generic))))

(defun skip-restart-test (restart src)
  (handler-bind
      ((lilylink:lilylink-parse-error
         (lambda (c)
           (declare (ignore c))
           (when (find-restart restart)
             (invoke-restart restart)))))
    (lilylink:parse-music src)))

(defun events->pitch-strings (events)
  (mapcar #'pitch-string events))

(deftest recovery-restarts
  (testing "skip-command recovers past an unsupported command"
    (ok (equal (events->pitch-strings
                (skip-restart-test
                 'lilylink:skip-command
                 "{ c4 \\transpose d4 e4 f4 }"))
               '("c3" "d3" "e3" "f3"))))
  (testing "skip-event skips an isolated duration with no preceding note"
    (ok (equal (events->pitch-strings
                (skip-restart-test
                 'lilylink:skip-event
                 "{ r4 4 }"))
               '("r"))))
  (testing "abort-parse returns the events parsed so far"
    (ok (= (length (skip-restart-test
                    'lilylink:abort-parse
                    "{ c4 d4 e4 f4 g4 \\transpose }"))
           5)))
  (testing "without a handler the error still propagates"
    (ok (handler-case (progn (let ((lilylink:*strict-mode* t))
                               (lilylink:parse-music "{ \\transpose }"))
                              nil)
          (lilylink:lilylink-parse-error () t)))))
