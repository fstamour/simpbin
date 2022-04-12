(defpackage #:simpbin
  (:use #:cl)
  (:export
   ;; integer
   #:write-integer
   #:read-integer
   ;; varint
   #:write-varint
   #:read-varint
   ;; header
   #:write-header
   #:read-header
   ;; octets
   #:write-octets
   #:read-octets
   #:write-octets*
   #:read-octets*
   ;; string
   #:write-binary-string
   #:read-binary-string
   #:write-binary-string*
   #:read-binary-string*
   ;; with-* macros
   #:with-output-to-binary-file
   #:with-input-from-binary-file))

(in-package #:simpbin)


;;; integer

(defun write-integer (integer stream)
  "Write a 32 bit, little-endian unsigned integer to the stream."
  (declare (type (integer 0 #.(1- (expt 2 32))) ))
  (nibbles:write-ub32/le integer stream))

(defun read-integer (stream)
  "Read a 32 bit, little-endian unsigned integer from the stream."
  (the  (integer 0 #.(1- (expt 2 32)))
        (nibbles:read-ub32/le stream)))


;;; varint

(defun varint-length (integer)
  (if
   (< integer 127)
   1
   (ceiling (log (1+ integer) 2) 7)))

(defun write-varint (integer stream)
  (if (< integer 127)
      (write-byte integer stream)
      (loop
        :with size = (varint-length integer)
        :for i :below size
        :for lastp = (= i (1- size))
        :for msb = (if lastp 0 #x80)
        :for byte = (logior
                     msb
                     (ldb (byte 7 (* 7 i)) integer))
        :do (write-byte byte stream))))

(defun read-varint (stream)
  (loop
    :with result = 0
    :for i :from 0
    :for byte = (read-byte stream)
    :for lastp = (zerop (logand byte #x80))
    :for value = (logand byte #x7f)
    :do
       (setf result (dpb value (byte 7 (* 7 i)) result))
       (when lastp (return result))))


;;; header

(defun write-header (stream)
  "Write a header to the stream"
  ;; signature
  (write-sequence #.(flexi-streams:string-to-octets "BINS") stream)
  ;; variant of the format
  (nibbles:write-ub32/be 0 stream))

(defun read-header (stream)
  "Read a header from the stream, verifies the signature and returns
the variant number."
  (let* ((signature ))
    (when (eq 1397639490 ;; It's the string "BINS" read as an integer
              (read-integer stream))
      ;; Read the variant
      (read-integer stream))))


;;; octets

(defun write-octets (octet-vector stream)
  "Write an octet-vector of octets to the stream.
First the size is written, then the content of the vector.
Use a 32-bits unsigned integer to store the size of the vector."
  (declare (type nibbles:octet-vector octet-vector))
  (write-integer (length octet-vector) stream)
  (write-sequence octet-vector stream))

(defun read-octets (stream)
  "Read an octet-vector of octets to the stream.
Read the length of the vector first, then the content.
Use a 32-bits unsigned integer to store the size of the vector."
  (let* ((length (read-integer stream))
         (octet-vector (nibbles:make-octet-vector length)))
    (read-sequence octet-vector stream)
    octet-vector))

(defun write-octets* (octet-vector stream)
  "Write an octet-vector of octets to the stream.
First the size is written, then the content of the vector.
Use a varint to store the size of the vector."
  (declare (type nibbles:octet-vector octet-vector))
  (write-varint (length octet-vector) stream)
  (write-sequence octet-vector stream))

(defun read-octets* (stream)
  "Read an octet-vector of octets to the stream.
Read the length of the vector first, then the content.
Use a varint to store the size of the vector."
  (let* ((length (read-varint stream))
         (octet-vector (nibbles:make-octet-vector length)))
    (read-sequence octet-vector stream)
    octet-vector))


;;; string

(defun write-binary-string (string stream
                            &key (encoding :utf8))
  "Write a string to stream. Use a 32-bits unsigned integer to store
the size of the string."
  (write-octets
   (flexi-streams:string-to-octets
    string
    :external-format encoding)
   stream))

(defun read-binary-string (stream
                           &key (encoding :utf8))
  "Read a string from stream. Use a 32-bits unsigned integer to store
the size of the string."
  (flexi-streams:octets-to-string
   (read-octets stream)
   :external-format encoding))

(defun write-binary-string* (string stream
                             &key (encoding :utf8))
  "Write a string to stream. Use a varint to store the size of the
string."
  (write-octets*
   (flexi-streams:string-to-octets
    string
    :external-format encoding)
   stream))

(defun read-binary-string* (stream
                            &key (encoding :utf8))
  "Read a string from stream. Use a varint to store the size of the
string."
  (flexi-streams:octets-to-string
   (read-octets* stream)
   :external-format encoding))


;;; multiple octets

(defmacro with-output-to-binary-file ((stream filespec
                                       &rest options
                                       &key
                                         if-exists
                                         if-does-not-exist
                                         external-format)
                                      &body body)
  "Helper macro to open a file with the right element-type for binary
output."
  (declare (ignore if-exists if-does-not-exist external-format))
  `(with-open-file (,stream ,filespec
                            :direction :output
                            :element-type 'nibbles:octet
                            ,@options)
     ,@body))

(defmacro with-input-from-binary-file ((stream filespec
                                        &rest options
                                        &key
                                          if-exists
                                          if-does-not-exist
                                          external-format)
                                       &body body)
  "Helper macro to open a file with the right element-type for binary
input."
  (declare (ignore if-exists if-does-not-exist external-format))
  `(with-open-file (,stream ,filespec
                            :direction :input
                            :element-type 'nibbles:octet
                            ,@options)
     ,@body))
