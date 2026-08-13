(in-package :lilylink/tests/main)

(deftest tokenize-notes
  (testing "basic note sequence produces pitch tokens"
    (let ((tokens (lilylink:tokenize "c4 d e")))
      (ok (= (length tokens) 3))
      (ok (every (lambda (tok) (eq (lilylink::token-type tok) :pitch)) tokens))))
  (testing "octave marks and duration are captured"
    (let* ((tokens (lilylink:tokenize "c'4 c, c''8."))
           (vals (mapcar #'lilylink::token-value tokens)))
      (ok (= (nth 2 (first vals)) 1))
      (ok (= (nth 2 (second vals)) -1))
      (ok (= (nth 2 (third vals)) 2))
      (ok (= (car (nth 3 (third vals))) 3))
      (ok (= (cdr (nth 3 (third vals))) 1))))
  (testing "accidentals are captured"
    (let* ((tokens (lilylink:tokenize "cis aes ees cisis?"))
           (alters (mapcar (lambda (tok) (nth 1 (lilylink::token-value tok)))
                           tokens)))
      (ok (= (length alters) 4))
      (ok (equal alters '(1 -1 -1 2))))))

(deftest tokenize-rests-and-chords
  (testing "rests carry durations"
    (let* ((tokens (lilylink:tokenize "r4 r"))
           (vals (mapcar #'lilylink::token-value tokens)))
      (ok (eq (lilylink::token-type (first tokens)) :rest))
      (ok (= (car (first vals)) 2))
      (ok (null (second vals)) "bare r has no duration")))
  (testing "chord delimiters"
    (let ((tokens (lilylink:tokenize "<c e g>")))
      (ok (equal (mapcar #'lilylink::token-type tokens)
                 '(:chord-open :pitch :pitch :pitch :chord-close)))))
  (testing "words vs notes"
    (let ((tokens (lilylink:tokenize "bass treble c")))
      (ok (equal (mapcar #'lilylink::token-type tokens) '(:word :word :pitch))))))

(deftest tokenize-comments-and-commands
  (testing "line comments are skipped"
    (let ((tokens (lilylink:tokenize (concatenate 'string "c % comment here"
                                                  (string #\Newline) " d4"))))
      (ok (= (length tokens) 2))))
  (testing "commands"
    (let ((tokens (lilylink:tokenize "\\relative c' \\time 4/4 \\key g \\major")))
      (ok (equal (mapcar #'lilylink::token-type tokens)
                 '(:command :pitch :command :number :slash :number :command :pitch :command)))))
  (testing "tie tokens"
    (let ((tokens (lilylink:tokenize "c4~ c4")))
      (ok (equal (mapcar #'lilylink::token-type tokens)
                 '(:pitch :tie :pitch)))))
  (testing "quoted strings"
    (let ((tokens (lilylink:tokenize "\\version \"2.26.0\"")))
      (ok (equal (lilylink::token-value (second tokens)) "2.26.0"))))
  (testing "simultaneous music and voice separators"
    (let ((tokens (lilylink:tokenize "<< { c4 } \\\\ { d4 } >>")))
      (ok (equal (mapcar #'lilylink::token-type tokens)
                 '(:simult-open :brace-open :pitch :brace-close
                   :voice-separator :brace-open :pitch :brace-close
                   :simult-close)))))
  (testing "articulation shorthands and direction prefixes"
    (let ((tokens (lilylink:tokenize "c4-. d^\\f e->")))
      (ok (equal (mapcar #'lilylink::token-type tokens)
                 '(:pitch :articulation :pitch :attach-up :command :pitch :articulation))))
    (let* ((tokens (lilylink:tokenize "c4-. d-> e-!"))
           (marks (loop for tok in tokens
                        when (eq (lilylink::token-type tok) :articulation)
                        collect (lilylink::token-value tok))))
      (ok (equal marks '(:staccato :accent :staccatissimo))))
    (let ((tokens (lilylink:tokenize "c4( d\\) e\\< f\\> g\\! a\\( b)")))
      (ok (equal (mapcar #'lilylink::token-type tokens)
                 '(:pitch :slur-open :pitch :phrase-close :pitch :wedge-start
                   :pitch :wedge-start :pitch :wedge-stop :pitch :phrase-open
                   :pitch :slur-close))))))
