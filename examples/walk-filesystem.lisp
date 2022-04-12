(defpackage #:simpbin.walk-filesystem
  (:documentation "An example of using simpbin to cache the result of walking the
filesystem.

We see that it's much faster (3 seconds v.s. 10 minutes) to read a
list of directory from a file that to re-walk the filesystem.
")
  (:use #:cl #:simpbin))

(in-package #:simpbin.walk-filesystem)

(defparameter *examples-dir*
  (asdf:system-relative-pathname "simpbin"
                                 "examples/"))

(defparameter *root* nil)

(unless *root*
  (error "Please give this variable a value.
P.S. This file is meant to be run with a listener (e.g. sly or swank)"))

(unless (uiop:directory-pathname-p *root*)
  (error "*root* must be a directory pathname."))


;;; Walk the filesystem, to get a baseline of how much time it might
;;; take.

(time
 (let ((root )
       (count 0))
   (uiop:collect-sub*directories
    *root*
    ;; collectp
    (constantly t)
    ;; recursep
    (constantly t)
    #'(lambda (path)
        (incf count)))
   (format t "~&There are ~D directories" count)))

#|
There are 11447 directories
Evaluation took:
40.861 seconds of real time
40.828125 seconds of total run time (4.375000 user, 36.453125 system)
99.92% CPU
142,748,085,886 processor cycles
623,136,320 bytes consed
|#


;;; Writing a list of directories as we walk the file system

(time
 (simpbin:with-output-to-binary-file (output
                                      (merge-pathnames "dirs.bin" *examples-dir*)
                                      :if-exists :supersede)
   (write-header output)
   (write-binary-string
    (namestring *root*)
    output)
   (uiop:collect-sub*directories
    *root*
    ;; collectp
    (constantly t)
    ;; recursep
    (constantly t)
    #'(lambda (path)
        (write-binary-string
         (enough-namestring path *root*)
         output)))))

#|
Evaluation took:
40.800 seconds of real time
40.796875 seconds of total run time (4.640625 user, 36.156250 system)
[ Run times consist of 0.125 seconds GC time, and 40.672 seconds non-GC time. ]
99.99% CPU
142,533,572,650 processor cycles
554,408,080 bytes consed

dirs.bin is 622k
|#


;; Ran on a bigger folder:
#|
Evaluation took:
525.813 seconds of real time     (A bit more than 10 minutes)
524.968750 seconds of total run time (57.171875 user, 467.796875 system)
[ Run times consist of 1.359 seconds GC time, and 523.610 seconds non-GC time. ]
99.84% CPU
1,836,906,435,422 processor cycles
7,554,891,856 bytes consed

dirs.bin is 9.2M
|#


;;; Reading the list of directories

(time
 (with-input-from-binary-file (input (merge-pathnames "dirs.bin" *examples-dir*))
   (read-header input)
   (let ((root (read-binary-string input)))
     (loop
       :while (< (file-position input) (file-length input))
       :for path = (merge-pathnames (read-binary-string input)
                                    root)))))

#|
Evaluation took:
0.222 seconds of real time
0.218750 seconds of total run time (0.093750 user, 0.125000 system)
98.65% CPU
775,863,485 processor cycles
46,452,640 bytes consed
|#


;; On the bigger folder:
#|
Evaluation took:
2.799 seconds of real time
2.796875 seconds of total run time (1.687500 user, 1.109375 system)
[ Run times consist of 0.171 seconds GC time, and 2.626 seconds non-GC time. ]
99.93% CPU
9,779,199,535 processor cycles
629,449,888 bytes consed
|#




;;; Writing a list of directories as we walk the file system
;;; Using varint this time!

(time
 (simpbin:with-output-to-binary-file (output
                                      (merge-pathnames "dirs2.bin" *examples-dir*)
                                      :if-exists :supersede)
   (write-header output)
   (write-binary-string*
    (namestring *root*)
    output)
   (uiop:collect-sub*directories
    *root*
    ;; collectp
    (constantly t)
    ;; recursep
    (constantly t)
    #'(lambda (path)
        (write-binary-string*
         (enough-namestring path *root*)
         output)))))

#|
Evaluation took:
41.302 seconds of real time
41.296875 seconds of total run time (4.484375 user, 36.812500 system)
[ Run times consist of 0.296 seconds GC time, and 41.001 seconds non-GC time. ]
99.99% CPU
144,286,847,884 processor cycles
579,077,104 bytes consed

dirs.bin is 589K

|#


;; On the bigger folder:
#|
Evaluation took:
522.929 seconds of real time
522.718750 seconds of total run time (61.296875 user, 461.421875 system)
[ Run times consist of 1.343 seconds GC time, and 521.376 seconds non-GC time. ]
99.96% CPU
1,826,829,348,855 processor cycles
7,518,444,816 bytes consed

dirs2.bin is 8.8M
|#


;;; Reading the list of directories
;;; Using varint this time!

(time
 (with-input-from-binary-file (input (merge-pathnames "dirs2.bin" *examples-dir*))
   (read-header input)
   (let ((root (read-binary-string* input)))
     (loop
       :while (< (file-position input) (file-length input))
       :for path = (merge-pathnames (read-binary-string* input)
                                    root)))))

;; On the bigger folder:
#|
Evaluation took:
2.678 seconds of real time
2.687500 seconds of total run time (1.468750 user, 1.218750 system)
100.37% CPU
9,357,203,875 processor cycles
629,513,440 bytes consed
|#

