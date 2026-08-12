(uiop:define-package lilylink
  (:use #:cl #:uiop)
  (:export #:convert-string
           #:convert-file
           #:tokenize
           #:parse-music
           #:build-score
           #:emit-score
           #:lilylink-parse-error
           #:parse-error-message
           #:parse-error-line
           #:parse-error-col))
(in-package #:lilylink)

(define-condition lilylink-parse-error (error)
  ((message :initarg :message :reader parse-error-message)
   (line :initarg :line :initform nil :reader parse-error-line)
   (col :initarg :col :initform nil :reader parse-error-col))
  (:report (lambda (c s)
             (format s "LilyPond parse error at line ~D, column ~D: ~A"
                     (parse-error-line c) (parse-error-col c)
                     (parse-error-message c)))))

(defun convert-string (source)
  "Convert a LilyPond source string to a MusicXML string."
  (let ((score (build-score (parse-music source))))
    (with-output-to-string (s)
      (emit-score s score))))

(defun convert-file (path)
  "Read the LilyPond file at PATH and convert it to a MusicXML string."
  (convert-string (uiop:read-file-string path)))
