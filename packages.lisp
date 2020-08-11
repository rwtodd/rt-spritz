(in-package :cl-user)

(defpackage "RT-SPRITZ-CIPHER"
  (:use :common-lisp)
  (:export #:reset
	   #:make-ss
	   #:reset-ss
	   #:absorb-vec
	   #:absorb-stop
	   #:absorb-int-bytes
	   #:squeeze
	   #:squeeze-xor))

(defpackage "RT-SPRITZ"
  (:use :common-lisp "RT-SPRITZ-CIPHER")
  (:export #:hash-file
	   #:hash-into
	   #:pprint-file-hashes))

