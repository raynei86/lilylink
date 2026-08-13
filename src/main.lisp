(uiop:define-package lilylink
  (:use #:cl)
  (:import-from #:alexandria #:when-let #:if-let)
  (:export #:convert-string
           #:convert-file
           #:tokenize
           #:parse-music
           #:build-score
           #:emit-score
           #:lilylink-error
           #:lilylink-parse-error
           #:lilylink-emit-error
           #:parse-error-message
           #:parse-error-line
           #:parse-error-col
           #:parse-error-token))
(in-package #:lilylink)

;;; Root of the library's error hierarchy.  Subtypes: LILYLINK-PARSE-ERROR
;;; (bad input, carries the source location and offending token) and
;;; LILYLINK-EMIT-ERROR (an internal invariant that should not fire on valid
;;; input).
(define-condition lilylink-error (error) ())

(define-condition lilylink-parse-error (lilylink-error)
  ((message :initarg :message :reader parse-error-message)
   (line :initarg :line :initform nil :reader parse-error-line)
   (col :initarg :col :initform nil :reader parse-error-col)
   (token :initarg :token :initform nil :reader parse-error-token))
  (:report (lambda (c s)
             (format s "LilyPond parse error at line ~D, column ~D: ~A"
                     (parse-error-line c) (parse-error-col c)
                     (parse-error-message c)))))

(define-condition lilylink-emit-error (lilylink-error)
  ((message :initarg :message :reader emit-error-message))
  (:report (lambda (c s)
             (format s "Lilylink conversion error: ~A"
                     (emit-error-message c)))))

(defun signal-parse-error (line col token fmt &rest args)
  "Signal a LILYLINK-PARSE-ERROR at LINE/COL carrying TOKEN (or NIL)."
  (error 'lilylink-parse-error
         :message (apply #'format nil fmt args)
         :line line :col col :token token))

(defun emit-error (fmt &rest args)
  "Signal a LILYLINK-EMIT-ERROR (an internal invariant)."
  (error 'lilylink-emit-error
         :message (apply #'format nil fmt args)))

(defun convert-string (source)
  "Convert a LilyPond source string to a MusicXML string."
  (let ((score (build-score (parse-music source))))
    (with-output-to-string (s)
      (emit-score s score))))

(defun convert-file (path)
  "Read the LilyPond file at PATH and convert it to a MusicXML string."
  (convert-string (uiop:read-file-string path)))