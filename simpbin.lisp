;;;; simpbin.lisp

(in-package #:simpbin)


;;; integer

(defun write-integer (integer stream)
  "Write a 32 bit, little-endian integer to the stream."
  (declare (type (integer 0 #.(1- (expt 2 32))) ))
  (nibbles:write-ub32/le integer stream))

(defun read-integer (stream)
  "Read a 32 bit, little-endian integer from the stream."
  (the  (integer 0 #.(1- (expt 2 32)))
	(nibbles:read-ub32/le stream)))


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
First the size is written, then the content of the vector."
  (declare (type nibbles:octet-vector octet-vector))
  (write-integer (length octet-vector) stream)
  (write-sequence octet-vector stream))

(defun read-octets (stream)
  "Read an octet-vector of octets to the stream.
Read the length of the vector first, then the content."
  (let* ((length (read-integer stream))
	 (octet-vector (nibbles:make-octet-vector length)))
    (read-sequence octet-vector stream)
    octet-vector))


;;; string

(defun write-binary-string (string stream
			    &key (encoding :utf8))
  "Write a string to stream."
  (write-octets
   (flexi-streams:string-to-octets
    string
    :external-format encoding)
   stream))

(defun read-binary-string (stream
			   &key (encoding :utf8))
  "Read a string from stream."
  (flexi-streams:octets-to-string
   (read-octets stream)
   :external-format encoding))


;;; multiple octets

(defmacro with-output-to-binary-file ((stream filespec
					      &rest options
					      &key 
					      if-exists
					      if-does-not-exist
					      external-format)
				      &body body)
  "Helper macro to open a file for binary output."
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
  "Helper macro to open a file for binary input."
  (declare (ignore if-exists if-does-not-exist external-format))
  `(with-open-file (,stream ,filespec
			    :direction :input
			    :element-type 'nibbles:octet
			    ,@options)
     ,@body))

