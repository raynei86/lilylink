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
        ((and (listp e) (eq (car e) :barline)) "|")
        (t "cmd")))

(defun pitch-sequence (src)
  (mapcar #'pitch-string (lilylink:parse-music src)))

(deftest parse-relative-octaves
  (testing "ascending scale stays in octave, wraps correctly"
    (ok (equal (pitch-sequence "\\relative c' { c d e f g a b c | g c | c, g'' }")
               '("c4" "d4" "e4" "f4" "g4" "a4" "b4" "c5" "|" "g4" "c5" "|" "c4" "g5"))))
  (testing "intervals are minimized (fifth up beats fourth down on ties)"
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
          (lilylink:lilylink-parse-error () t)))))
