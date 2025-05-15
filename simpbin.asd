(defpackage #:simpbin.asd
  (:use :cl :asdf))

(in-package #:simpbin.asd)

(asdf:defsystem #:simpbin
  :description "A common lisp library to store data in a binary format"
  :author "Francis St-Amour"
  :license  "GNU GPLv3"
  :version "0.0.1"
  :depends-on (;; standard utils
               #:alexandria
               ;; facilities to read different encoding
               #:flexi-streams
               ;; fast (buffered) byte vector read/write
               #:fast-io
               ;; a bit like fast-io, but works with "normal" streams
               #:nibbles)
  :in-order-to ((asdf:test-op (asdf:test-op :simpbin/test)))
  :components ((:file "simpbin")))

(asdf:defsystem "simpbin/test"
  :description "Tests for simpbin"
  :version "0.1.0"
  :author "Francis St-Amour"
  :licence "GNU GPLv3"
  :depends-on (#:simpbin #:parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :simpbin.test :test-all))
  :serial t
  :components ((:file "tests")))
