(in-package #:common-lisp-user)

(defpackage #:simpbin.test
  (:use :cl #:simpbin)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true #:false)
  (:export #:test-all))

(in-package #:simpbin.test)


;;; integer

(define-test write-integer
  (is equalp
      (flexi-streams:with-output-to-sequence (output)
        (write-integer 42 output))
      #(42 0 0 0)))

(define-test read-integer
  (is = 42
      (flexi-streams:with-input-from-sequence (input #(42 0 0 0))
        (read-integer input))))


;;; varint

(define-test write-varint
  (is equalp
      (flexi-streams:with-output-to-sequence (output)
        (write-varint 42 output))
      #(42))
  (is equalp
      (flexi-streams:with-output-to-sequence (output)
        (write-varint 800 output))
      #(160 6))
  (loop :for i :below 128
        :collect
        (is = `#(,i)
            (flexi-streams:with-output-to-sequence (output)
              (write-varint i output)))))

(define-test read-varint
  (is = 42
      (flexi-streams:with-input-from-sequence (input #(42))
        (read-varint input)))
  (is = 800
      (flexi-streams:with-input-from-sequence (input #(160 6))
        (read-varint input))))

(define-test varint-roundtrip
  (loop :for i :below 100000
        :do (assert
             (= i (flexi-streams:with-input-from-sequence
                      (input
                       (flexi-streams:with-output-to-sequence (output)
                         (write-varint i output)))
                    (read-varint input)))
             (i))))


;;; header

(define-test write-header
  (is equalp
      (flexi-streams:with-output-to-sequence (output)
        (write-header output))
      #(66 73 78 83 0 0 0 0))
  (is =
      (length
       (flexi-streams:with-output-to-sequence (output)
         (write-header output)))
      8))

(define-test read-header
  (true
   (zerop
    (flexi-streams:with-input-from-sequence (input #(66 73 78 83 0 0 0 0))
      (read-header input))))
  (parachute:fail
   ;; This fails because it reaches EOF while reading.
   (flexi-streams:with-input-from-sequence (input #())
     (read-header input))))


;;; octets

(define-test write-octets
  (is equalp
      (flexi-streams:with-output-to-sequence (output)
  (write-octets (make-array
           4
           :initial-contents '(1 2 3 4)
           :element-type 'nibbles:octet)
          output))
      #(4 0 0 0 1 2 3 4)))

(define-test read-octets
  (is equalp
      (flexi-streams:with-input-from-sequence (input #(4 0 0 0 1 2 3 4))
  (read-octets input))
      #(1 2 3 4)))


;;; string

(define-test write-binary-string
  (is equalp
      (flexi-streams:with-output-to-sequence (output)
  (write-binary-string "hi" output))
      #(2 0 0 0 104 105))
  (is equalp
      (flexi-streams:with-output-to-sequence (output)
  (write-binary-string
   "this is a string with şömé unicode"
   output))
      #(37 0 0 0 116 104 105 115 32 105 115 32 97 32 115 116 114 105 110 103 32 119 105 116 104 32 197 159 195 182 109 195 169 32 117 110 105 99 111 100 101)))

(define-test read-binary-string
  (is equalp
      (flexi-streams:with-input-from-sequence (input #(2 0 0 0 104 105))
        (read-binary-string input))
      "hi")
  (is equalp
      (flexi-streams:with-input-from-sequence (input #(37 0 0 0 116 104 105 115 32 105 115 32 97 32 115 116 114 105 110 103 32 119 105 116 104 32 197 159 195 182 109 195 169 32 117 110 105 99 111 100 101))
        (read-binary-string input))
      "this is a string with şömé unicode"))


;;; with-macros

;; These tests are not part of the unit tests because they interact
;; with the file system.
#|
(with-output-to-binary-file (output "/tmp/test.bins"
:if-exists :overwrite
:if-does-not-exist :create)
(write-header output)
(write-binary-string "Hello" output))

(with-input-from-binary-file (input "/tmp/test.bins")
(read-header input)
(read-binary-string input))
;; => "Hello"

(with-input-from-binary-file (input "/tmp/test.bins")
(read-header input)
(read-binary-string input)
(peek-char nil input nil))
|#



(defun test-all ()
  (parachute:test #.*package*))
