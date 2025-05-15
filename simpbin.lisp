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
   ;; bit-vector
   #:write-bit-vector
   #:read-bit-vector
   ;; with-* macros
   #:with-output-to-binary-file
   #:with-input-from-binary-file))

(in-package #:simpbin)


;;; integer


(defconstant +integer-length+ 32
  "Number of bits read and writen by `read-integer' and `write-integer'.")

(defun write-integer (integer stream)
  "Write a 32 bit, little-endian unsigned INTEGER to STREAM."
  (declare (type (integer 0 #.(1- (expt 2 32))) ))
  (nibbles:write-ub32/le integer stream))

(defun read-integer (stream)
  "Read a 32 bit, little-endian unsigned INTEGER from STREAM."
  (the  (integer 0 #.(1- (expt 2 32)))
        (nibbles:read-ub32/le stream)))


;;; varint

(defun varint-length (integer)
  "How many octets is needed to encode the unsigned INTEGER as a varint?"
  (if (< integer 127) 1 (ceiling (log (1+ integer) 2) 7)))

(defun write-varint (integer stream)
  "Write the unsigned INTEGER to STREAM as a  varint."
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
  "Read a varint-encoded integer from STREAM."
  (loop :with result = 0
        :for i :from 0
        :for byte = (read-byte stream)
        :for lastp = (zerop (logand byte #x80))
        :for value = (logand byte #x7f)
        :do (setf result (dpb value (byte 7 (* 7 i)) result))
            (when lastp (return result))))


;;; header

(defun write-header (stream &optional (variant 0))
  "Write a header to the stream"
  ;; signature
  (write-sequence #.(flexi-streams:string-to-octets "BINS") stream)
  ;; variant of the format
  (nibbles:write-ub32/be variant stream))

(defun read-header (stream)
  "Read a header from the stream, verifies the signature and returns
the variant number."
  (when (eq 1397639490 ;; It's the string "BINS" read as an integer
            (read-integer stream))
    ;; Read the variant
    (read-integer stream)))


;;; octets

(defun write-octets (octet-vector stream)
  "Write an OCTET-VECTOR to STREAM.
First the size is written, then the content of the vector.
Use a 32-bits unsigned integer to store the size of the vector."
  (declare (type nibbles:octet-vector octet-vector))
  (write-integer (length octet-vector) stream)
  (write-sequence octet-vector stream))

(defun read-octets (stream)
  "Read an OCTET-VECTOR from STREAM.
Read the length of the vector first, then the content.
Use a 32-bits unsigned integer to store the size of the vector."
  (let* ((length (read-integer stream))
         (octet-vector (nibbles:make-octet-vector length)))
    (read-sequence octet-vector stream)
    octet-vector))

(defun write-octets* (octet-vector stream)
  "Write an OCTET-VECTOR to STREAM.
First the size is written, then the content of the vector.
Use a varint to store the size of the vector."
  (declare (type nibbles:octet-vector octet-vector))
  (write-varint (length octet-vector) stream)
  (write-sequence octet-vector stream))

(defun read-octets* (stream)
  "Read an OCTET-VECTOR to STREAM.
Read the length of the vector first, then the content.
Use a varint to store the size of the vector."
  (let* ((length (read-varint stream))
         (octet-vector (nibbles:make-octet-vector length)))
    (read-sequence octet-vector stream)
    octet-vector))


;;; string

(defun write-binary-string (string stream
                            &key (encoding :utf8))
  "Write a STRING to STREAM. Use a 32-bits unsigned integer to store
the size of the string."
  (write-octets
   (flexi-streams:string-to-octets
    string
    :external-format encoding)
   stream))

(defun read-binary-string (stream
                           &key (encoding :utf8))
  "Read a STRING from STREAM. Use a 32-bits unsigned integer to store
the size of the string."
  (flexi-streams:octets-to-string
   (read-octets stream)
   :external-format encoding))

(defun write-binary-string* (string stream
                             &key (encoding :utf8))
  "Write a STRING to STREAM. Use a varint to store the size of the
string."
  (write-octets*
   (flexi-streams:string-to-octets
    string
    :external-format encoding)
   stream))

(defun read-binary-string* (stream
                            &key (encoding :utf8))
  "Read a string from STREAM. Use a varint to store the size of the
string."
  (flexi-streams:octets-to-string
   (read-octets* stream)
   :external-format encoding))


;;; bit-vectors (length is in bits)


(defun write-bit-vector (bit-vector stream
                         &aux (length (length bit-vector)))
  "Write a BIT-VECTOR to STREAM.
The vector is written in 32-bit chunks, padding is added if necessary.
Use varint to store the size, in number of bits, of the bit-vector."
  (write-varint length stream)
  (loop :for i :below (ceiling length +integer-length+)
        :for x := (loop :with x = 0
                        :for j :from (* +integer-length+ i) :below length
                        :for m :below +integer-length+
                        :do (incf x (ash (aref bit-vector j) m))
                        :finally (return x))
        :do (write-integer x stream)))

(defun read-bit-vector (stream)
  "Read a bit-vector from STREAM."
  (let* ((length (read-varint stream))
         (bit-vector (make-array length :element-type 'bit
                                        :initial-element 0)))
    (loop :for i :below (ceiling length +integer-length+)
          :for x := (read-integer stream)
          :do (loop :for m :below +integer-length+
                    :for j :from (* i +integer-length+) :below (min length (* (1+ i) +integer-length+))
                    :do (setf (aref bit-vector j)
                              (ldb (byte 1 m) x))))
    bit-vector))


;;; with-* macros

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
