(defpackage #:simpbin.asd
  (:use :cl :asdf))

(in-package #:simpbin.asd)

(asdf:defsystem #:simpbin
  :description "Describe simpbin here"
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
  :in-order-to ((asdf:test-op (asdf:test-op :simpbin.test)))
  :serial t
  :components ((:file "package")
               (:file "simpbin")))
