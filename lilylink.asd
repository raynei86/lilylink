(defsystem "lilylink"
  :version "0.0.1"
  :author "Lihui Zhang"
  :mailto "zlihui486@gmail.com"
  :license "GPLv3"
  :depends-on ("alexandria")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "model")
                 (:file "duration")
                 (:file "lexer")
                 (:file "parser")
                 (:file "measure")
                 (:file "musicxml"))))
  :description "A tool to convert Lilypond files to MusicXML"
  :in-order-to ((test-op (test-op "lilylink/tests"))))

(defsystem "lilylink/tests"
  :author "Lihui Zhang"
  :license "GPLv3"
  :depends-on ("lilylink"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "lexer")
                 (:file "parser")
                 (:file "musicxml"))))
  :description "Test system for lilylink"
  :perform (test-op (op c) (symbol-call :rove :run c)))
