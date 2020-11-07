;;;; package.lisp

(defpackage #:simpbin
  (:use #:cl)
  (:export
   ;; integer
   #:write-integer
   #:read-integer
   ;; header
   #:write-header
   #:read-header
   ;; octets
   #:write-octets
   #:read-octets
   ;; string
   #:write-binary-string
   #:read-binary-string
   ;; with-* macros
   #:with-output-to-binary-file
   #:with-input-from-binary-file))
