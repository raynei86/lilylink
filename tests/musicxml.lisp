(in-package :lilylink/tests/main)

(defun xml-tag-text (tag xml)
  "Return the text content of the first <TAG>...</TAG> in XML, or NIL."
  (let ((start (search (format nil "<~A>" tag) xml)))
    (when start
      (let* ((begin (+ start (length (format nil "<~A>" tag))))
             (end (search (format nil "</~A>" tag) xml :start2 begin)))
        (when end
          (subseq xml begin end))))))

(defun key-xml (key mode)
  "Convert a one-note piece in KEY \\MODE and return its <fifths>/<mode> text."
  (let ((xml (lilylink:convert-string (format nil "{ \\key ~A \\~A c4 }" key mode))))
    (list (xml-tag-text "fifths" xml)
          (xml-tag-text "mode" xml))))

(deftest convert-key-signatures
  (testing "sharp keys count fifths"
    (ok (equal (key-xml "d" "major") '("2" "major")))
    (ok (equal (key-xml "e" "major") '("4" "major"))))
  (testing "flat keys count negative fifths"
    (ok (equal (key-xml "f" "major") '("-1" "major")))
    (ok (equal (key-xml "bes" "major") '("-2" "major"))))
  (testing "minor keys are relative to the major"
    (ok (equal (key-xml "a" "minor") '("0" "minor")))
    (ok (equal (key-xml "e" "minor") '("1" "minor")))
    (ok (equal (key-xml "d" "minor") '("-1" "minor")))))

(deftest convert-tempo
  (testing "metronome tempo emits beat-unit, per-minute and sound"
    (let ((xml (lilylink:convert-string "\\relative c' { \\tempo 4 = 120 c4 }")))
      (ok (search "<direction placement=\"above\">" xml))
      (ok (search "<metronome><beat-unit>quarter</beat-unit><per-minute>120</per-minute></metronome>" xml))
      (ok (search "<sound tempo=\"120\"/>" xml))))
  (testing "tempo with text adds a words element"
    (let ((xml (lilylink:convert-string "\\relative c' { \\tempo \"Allegro\" 4 = 120 c4 }")))
      (ok (search "<words>Allegro</words>" xml))
      (ok (search "<sound tempo=\"120\"/>" xml))))
  (testing "a text-only tempo has no metronome or sound"
    (let ((xml (lilylink:convert-string "\\relative c' { \\tempo \"Andante\" c4 }")))
      (ok (search "<words>Andante</words>" xml))
      (ok (not (search "<metronome>" xml)))
      (ok (not (search "<sound" xml)))))
  (testing "tempo appears before the notes of its measure"
    (let ((xml (lilylink:convert-string "\\relative c' { \\tempo 4 = 96 c4 d }")))
      (ok (search "<sound tempo=\"96\"/></direction><note>" xml)))))

(deftest convert-basic-melody
  (testing "single staff score structure"
    (let ((xml (lilylink:convert-string "\\relative c' { c4 d e f | g2 a | b1 }")))
      (ok (search "<score-partwise" xml))
      (ok (search "<part id=\"P1\">" xml))
      (ok (search "<measure number=\"1\">" xml))
      (ok (search "<measure number=\"2\">" xml))
      (ok (search "<measure number=\"3\">" xml))))
  (testing "pitches and durations"
    (let ((xml (lilylink:convert-string "\\relative c' { c4 d e f | g2 a | b1 }")))
      (ok (search "<step>C</step><octave>4</octave>" xml))
      (ok (search "<duration>4</duration><type>quarter</type>" xml))
      (ok (search "<duration>8</duration><type>half</type>" xml))
      (ok (search "<duration>16</duration><type>whole</type>" xml)))))

(deftest convert-attributes
  (testing "key, time and clef attributes"
    (let ((xml (lilylink:convert-string "\\relative c' { \\key g \\major \\time 3/4 \\clef treble c4 d e }")))
      (ok (search "<fifths>1</fifths>" xml))
      (ok (search "<mode>major</mode>" xml))
      (ok (search "<beats>3</beats><beat-type>4</beat-type>" xml))
      (ok (search "<clef><sign>G</sign><line>2</line></clef>" xml))))
  (testing "bass clef and octave shift"
    (let ((xml (lilylink:convert-string "{ \\clef bass c4 }")))
      (ok (search "<clef><sign>F</sign><line>4</line></clef>" xml))))
  (testing "clef change emits attributes in the right measure"
    (let ((xml (lilylink:convert-string "\\relative c'' { \\time 3/4 fis4 g a | bes2. | \\clef bass e,4 f g | }")))
      (ok (search "<measure number=\"1\"><attributes>" xml))
      (ok (search "<measure number=\"2\"><note>" xml))
      (ok (search "<measure number=\"3\"><attributes><divisions>4</divisions><key><fifths>0</fifths><mode>major</mode></key><time><beats>3</beats><beat-type>4</beat-type></time><clef><sign>F</sign><line>4</line></clef></attributes>" xml))))
  (testing "accidentals emit an alter element"
    (let ((xml (lilylink:convert-string "\\relative c' { cis4 }")))
      (ok (search "<alter>1</alter>" xml)))
    (let ((xml (lilylink:convert-string "\\relative c' { bes4 }")))
      (ok (search "<alter>-1</alter>" xml))))
  (testing "clef octave shifts emit an octave-change element"
    (let ((xml (lilylink:convert-string "{ \\clef \"treble^8\" c4 }")))
      (ok (search "<octave-change>1</octave-change>" xml)))
    (let ((xml (lilylink:convert-string "{ \\clef \"bass_8\" c4 }")))
      (ok (search "<octave-change>-1</octave-change>" xml)))))

(deftest convert-dotted-and-rests
  (testing "dotted notes and rests"
    (let ((xml (lilylink:convert-string "\\relative c' { \\time 6/8 c4. d8 r4 }")))
      (ok (search "<dot/>" xml))
      (ok (search "<rest/>" xml))
      (ok (search "<duration>12</duration><type>quarter</type><dot/>" xml))
      (ok (search "<divisions>8</divisions>" xml)))))

(deftest convert-chords
  (testing "chord notes carry chord marker"
    (let ((xml (lilylink:convert-string "\\relative c' { <c e g>4 }")))
      (ok (search "<note><pitch><step>C</step><octave>4</octave>" xml))
      (ok (search "<note><chord/><pitch><step>E</step>" xml))
      (ok (search "<note><chord/><pitch><step>G</step>" xml)))))

(deftest convert-score-wrapper
  (testing "version, header and score wrappers are handled"
    (let ((xml (lilylink:convert-string "\\version \"2.26.0\"
\\header { title = \"T\" }
\\score { \\relative c' { c4 } \\layout { } }")))
      (ok (search "<score-partwise" xml))
      (ok (search "<step>C</step>" xml)))))

(deftest convert-ties
  (testing "tied notes emit sound and notated tie markers"
    (let ((xml (lilylink:convert-string "\\relative c' { c4~ c4 }")))
      (ok (search "<tie type=\"start\"/>" xml))
      (ok (search "<tie type=\"stop\"/>" xml))
      (ok (search "<tied type=\"start\"/>" xml))
      (ok (search "<tied type=\"stop\"/>" xml))))
  (testing "tie element order matches the schema"
    (let ((xml (lilylink:convert-string "\\relative c' { c4~ c4 }")))
      (ok (search "<duration>4</duration><tie type=\"start\"/><type>quarter</type>" xml))
      (ok (search "<notations><tied type=\"start\"/></notations>" xml)))))

(deftest convert-auto-split
  (testing "an overflowing note splits into tied notes across barlines"
    (let ((xml (lilylink:convert-string "\\relative c' { \\time 3/4 c2 c2 }")))
      (ok (search "<duration>2</duration><tie type=\"start\"/><type>quarter</type>" xml))
      (ok (search "<measure number=\"2\"><note><pitch><step>C</step><octave>4</octave></pitch><duration>2</duration><tie type=\"stop\"/>" xml))))
  (testing "a whole note in 2/4 splits into tied halves"
    (let ((xml (lilylink:convert-string "\\relative c' { \\time 2/4 c1 }")))
      (ok (search "<duration>8</duration><tie type=\"start\"/><type>half</type>" xml))
      (ok (search "<tie type=\"stop\"/><type>half</type>" xml))))
  (testing "chords split as tied chord pairs"
    (let ((xml (lilylink:convert-string "\\relative c' { \\time 3/4 <c e>2 <c e>2 }")))
      (ok (search "<chord/><pitch><step>E</step><octave>4</octave></pitch><duration>2</duration><tie type=\"start\"/>" xml))
      (ok (search "<chord/><pitch><step>E</step><octave>4</octave></pitch><duration>2</duration><tie type=\"stop\"/>" xml))))
  (testing "overflowing rests split without ties"
    (let ((xml (lilylink:convert-string "\\relative c' { \\time 2/4 r1 }")))
      (ok (search "<rest/><duration>8</duration><type>half</type>" xml))
      (ok (not (search "<tie" xml))))))

(deftest convert-marks
  (testing "articulations, dynamics, ornaments, technical, fermata"
    (let ((xml (lilylink:convert-string "\\relative c' { c4\\staccato d\\mf e\\trill f\\upbow g\\fermata }")))
      (ok (search "<articulations><staccato/></articulations>" xml))
      (ok (search "<dynamics><mf/></dynamics>" xml))
      (ok (search "<ornaments><trill-mark/></ornaments>" xml))
      (ok (search "<technical><up-bow/></technical>" xml))
      (ok (search "<fermata/>" xml))))
  (testing "shorthand articulations map correctly"
    (let ((xml (lilylink:convert-string "\\relative c' { c4-. d-> e-^ f-! g-- a-_ }")))
      (ok (search "<staccato/>" xml))
      (ok (search "<accent/>" xml))
      (ok (search "<strong-accent/>" xml))
      (ok (search "<staccatissimo/>" xml))
      (ok (search "<tenuto/>" xml))
      (ok (search "<detached-legato/>" xml))))
  (testing "a fermata on a rest"
    (let ((xml (lilylink:convert-string "\\relative c' { r4\\fermata }")))
      (ok (search "<rest/><duration>4</duration><type>quarter</type><notations><fermata/>" xml))))
  (testing "marks survive auto-split on the first piece"
    (let ((xml (lilylink:convert-string "\\relative c' { \\time 2/4 c1\\fermata }")))
      (ok (search "<tie type=\"start\"/><type>half</type><notations><tied type=\"start\"/><fermata/></notations>" xml))))
  (testing "marks survive auto-split of a chord"
    (let ((xml (lilylink:convert-string "\\relative c' { \\time 2/4 <c e>1\\fermata }")))
      (ok (search "<chord/><pitch><step>E</step>" xml))
      (ok (search "<tied type=\"start\"/><fermata/></notations>" xml))))
  (testing "mordents map by name"
    (let ((xml (lilylink:convert-string "\\relative c' { c4\\mordent d\\prall }")))
      (ok (search "<inverted-mordent/>" xml))
      (ok (search "<mordent/>" xml))))
  (testing "other-* marks carry their text content"
    (let ((xml (lilylink:convert-string "\\relative c' { c4\\espressivo }")))
      (ok (search "<other-articulation>espressivo</other-articulation>" xml)))
    (let ((xml (lilylink:convert-string "\\relative c' { c4\\sff }")))
      (ok (search "<other-dynamics>sff</other-dynamics>" xml)))))

(deftest convert-slurs-and-hairpins
  (testing "slurs emit as notations"
    (let ((xml (lilylink:convert-string "\\relative c' { c4( d e) }")))
      (ok (search "<slur type=\"start\" number=\"1\"/>" xml))
      (ok (search "<slur type=\"stop\" number=\"1\"/>" xml))))
  (testing "phrasing slurs use an offset number"
    (let ((xml (lilylink:convert-string "\\relative c' { c4\\( d e\\) }")))
      (ok (search "<slur type=\"start\" number=\"101\"/>" xml))
      (ok (search "<slur type=\"stop\" number=\"101\"/>" xml))))
  (testing "hairpins emit as directions"
    (let ((xml (lilylink:convert-string "\\relative c' { c4\\< d\\! }")))
      (ok (search "<direction placement=\"above\"><direction-type><wedge type=\"crescendo\" number=\"1\"/></direction-type></direction>" xml))
      (ok (search "<wedge type=\"stop\" number=\"1\"/>" xml))))
  (testing "a dynamic closes an open hairpin"
    (let ((xml (lilylink:convert-string "\\relative c' { c4\\< d\\f }")))
      (ok (search "<wedge type=\"stop\"" xml))
      (ok (search "<dynamics><f/></dynamics>" xml)))))

(deftest convert-lines
  (testing "glissando emits start and stop"
    (let ((xml (lilylink:convert-string "\\relative c' { g2\\glissando g'4 }")))
      (ok (search "<glissando type=\"start\" number=\"1\"/>" xml))
      (ok (search "<glissando type=\"stop\" number=\"1\"/>" xml))))
  (testing "trill spans emit trill-mark and wavy-line"
    (let ((xml (lilylink:convert-string "\\relative c' { d1\\startTrillSpan c2\\stopTrillSpan }")))
      (ok (search "<trill-mark/><wavy-line type=\"start\" number=\"1\"/>" xml))
      (ok (search "<wavy-line type=\"stop\" number=\"1\"/>" xml))))
  (testing "arpeggio emits on a chord"
    (let ((xml (lilylink:convert-string "\\relative c' { <c e g>1\\arpeggio }")))
      (ok (search "<arpeggiate/>" xml))))
  (testing "breathe emits a breath mark"
    (let ((xml (lilylink:convert-string "\\relative c' { c2 \\breathe d4 }")))
      (ok (search "<articulations><breath-mark/></articulations>" xml))))
  (testing "bends emit doit and falloff"
    (let ((xml (lilylink:convert-string "\\relative c' { c2\\bendAfter 4 d2\\bendAfter -4 }")))
      (ok (search "<doit/>" xml))
      (ok (search "<falloff/>" xml)))))

(deftest convert-voices
  (testing "multiple voices emit voice numbers and backup"
    (let ((xml (lilylink:convert-string "\\relative c' { << { c4 d e f } \\\\ { g2 a2 } >> }")))
      (ok (search "<voice>1</voice>" xml))
      (ok (search "<voice>2</voice>" xml))
      (ok (search "<backup><duration>16</duration></backup>" xml))))
  (testing "voice element comes before type"
    (let ((xml (lilylink:convert-string "\\relative c' { << { c4 } \\\\ { e4 } >> }")))
      (ok (search "<duration>4</duration><voice>1</voice><type>quarter</type>" xml))))
  (testing "a single voice emits no voice element"
    (let ((xml (lilylink:convert-string "\\relative c' { c4 d }")))
      (ok (not (search "<voice>" xml))))))

(deftest convert-error-cases
  (testing "an unknown clef is an emit error, not a parse error"
    (ok (handler-case (progn (lilylink:convert-string "{ \\clef foo c4 }") nil)
          (lilylink:lilylink-emit-error () t)
          (lilylink:lilylink-parse-error () nil))))
  (testing "an unknown clef is caught as lilylink-error"
    (ok (handler-case (progn (lilylink:convert-string "{ \\clef foo c4 }") nil)
          (lilylink:lilylink-error () t))))
  (testing "empty input produces a valid empty score"
    (let ((xml (lilylink:convert-string "")))
      (ok (search "<score-partwise" xml))
      (ok (search "<part id=\"P1\">" xml)))))

(deftest convert-file-roundtrip
  (testing "convert-file reads a .ly file"
    (uiop:with-temporary-file (:pathname p :type "ly")
      (with-open-file (s p :direction :output :if-exists :overwrite)
        (write-string "\\relative c' { c4 d }" s))
      (let ((xml (lilylink:convert-file p)))
        (ok (search "<score-partwise" xml)))))
  (testing "empty music defaults to a valid score"
    (let ((xml (lilylink:convert-string "{ }")))
      (ok (search "<score-partwise" xml))
      (ok (search "<part id=\"P1\">" xml))))
  (testing "top-level version, paper and midi wrappers are handled"
    (let ((xml (lilylink:convert-string "\\version \"2.26.0\" \\paper { } \\midi { } \\relative c' { c4 }")))
      (ok (search "<score-partwise" xml))
      (ok (search "<step>C</step>" xml))))
  (testing "a recoverable problem is skipped end-to-end"
    (let ((xml (lilylink:convert-string "{ c4 \\transpose }")))
      (ok (search "<step>C</step>" xml))
      (ok (not (search "<step>D</step>" xml))))))
