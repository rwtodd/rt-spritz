(in-package :cl-user)

(defpackage "RT-SPRITZ-CIPHER"
  (:use :common-lisp)
  (:export #:reset
	   #:ss
	   #:make-ss
	   #:reset-ss
	   #:absorb-vec
	   #:absorb-stop
	   #:absorb-int-bytes
	   #:random-bytes
	   #:drip
	   #:squeeze
	   #:squeeze-xor))

(defpackage "RT-SPRITZ"
  (:use :common-lisp)
  (:local-nicknames (#:cipher #:rt-spritz-cipher))
  (:export #:hash-file
	   #:hash-into
	   #:encrypt-file
	   #:pprint-file-hashes))

