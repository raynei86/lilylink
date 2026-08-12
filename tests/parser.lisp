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
  (testing "unsupported commands signal parse errors"
    (ok (handler-case (progn (lilylink:parse-music "\\transpose c d { c }") nil)
          (lilylink:lilylink-parse-error () t))))
  (testing "isolated duration without a preceding note is an error"
    (ok (handler-case (progn (lilylink:parse-music "{ 4 }") nil)
          (lilylink:lilylink-parse-error () t))))
  (testing "durations beyond the supported maximum are rejected cleanly"
    (ok (handler-case (progn (lilylink:parse-music "{ c2048 }") nil)
          (lilylink:lilylink-parse-error () t)))))
