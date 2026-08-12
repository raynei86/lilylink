(in-package :lilylink/tests/main)

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
      (ok (search "<measure number=\"3\"><attributes><divisions>4</divisions><key><fifths>0</fifths><mode>major</mode></key><time><beats>3</beats><beat-type>4</beat-type></time><clef><sign>F</sign><line>4</line></clef></attributes>" xml)))))

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

(deftest convert-file-roundtrip
  (testing "convert-file reads a .ly file"
    (let ((path (uiop:with-temporary-file (:pathname p :keep t :type "ly")
                  (with-open-file (s p :direction :output :if-exists :overwrite)
                    (write-string "\\relative c' { c4 d }" s))
                  p)))
      (let ((xml (lilylink:convert-file path)))
        (ok (search "<score-partwise" xml)))))
  (testing "empty music defaults to a valid score"
    (let ((xml (lilylink:convert-string "{ }")))
      (ok (search "<score-partwise" xml))
      (ok (search "<part id=\"P1\">" xml)))))
