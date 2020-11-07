(defpackage #:simpbin.test.asd
  (:use :cl :asdf))

(in-package #:simpbin.test.asd)

(asdf:defsystem "simpbin.test"
  :description "Tests for simpbin"
  :version "0.1.0"
  :author "Francis St-Amour"
  :licence "GNU GPLv3"
  :depends-on (#:simpbin #:simpbin.cmd #:parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :simpbin.test :test-all t))
  :serial t
  :components ((:file "tests")))
