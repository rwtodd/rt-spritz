;; just some sanity tests that the algorithm is working properly

(in-package :cl-user)
(require 'asdf)
(asdf:load-system "rt-spritz")

(defun str-to-array (str)
  (map '(vector (unsigned-byte 8)) #'char-code str))

(defun check-hex-text (str sz answer)
  (let ((result (rt-bintext:hex-encode
		 (rt-spritz:hash-into (make-array (ceiling sz 8)
						  :element-type '(unsigned-byte 8))
				      (str-to-array str)))))
    (format t "~A => ~A~%" str result)
    (if (string= result answer)
	(format t "   ok!~%")
	(error (format nil "Doesn't match <~a>!" answer)))))

(defun check-base64-text (str sz answer)
  (let ((result (rt-bintext:base64-encode
		 (rt-spritz:hash-into (make-array (ceiling sz 8)
						  :element-type '(unsigned-byte 8))
				      (str-to-array str)))))
    (format t "~A => ~A~%" str result)
    (if (string= result answer)
	(format t "   ok!~%")
	(error (format nil "Doesn't match <~a>!" answer)))))


(mapc #'(lambda (tc) (apply #'check-hex-text tc))
      '(("ABC"     256  "028fa2b48b934a1862b86910513a47677c1c2d95ec3e7570786f1c328bbd4a47")
	("spam"    256  "acbba0813f300d3a30410d14657421c15b55e3a14e3236b03989e797c7af4789")
	("arcfour" 256 	"ff8cf268094c87b95f74ce6fee9d3003a5f9fe6944653cd50e66bf189c63f699")))

(mapc #'(lambda (tc) (apply #'check-base64-text tc))
      '(("arcfour"  256 "/4zyaAlMh7lfdM5v7p0wA6X5/mlEZTzVDma/GJxj9pk=")
	("test of arc" 1024
	 "mODbEBMQN0e4fNQkMpQAFXRnJb+m4qJ4Jj/ZD85JEnqkgx0guarutyDDNUC6kDvDCSnIIxW0md2v8fng9jwOgNZxmp46NaJxjoR1jNfDIa8zf6nWNUdypzFTYQwL34Ci/SWcRq78Kzvod+oGZSTvpMuznWPo2nzVY32LPY/CI4E=")))

(quit)
