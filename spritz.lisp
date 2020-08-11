(in-package "RT-SPRITZ")

(defun hash-into (tgt src)
  "compute the hash of the SRC bytes into the TGT array"
  (declare (type (simple-array (unsigned-byte 8) 1) tgt src))
  (let ((s (make-ss)))
    (absorb-vec s src (length src))
    (absorb-stop s)
    (absorb-int-bytes s (length tgt))
    (squeeze s tgt)))

(defun hash-file (tgt fname &optional
			      (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
			      (spritz (make-ss) supplied))
  "compute the spriz hash of FNAME into a TGT buffer"
  (when supplied (reset-ss spritz))
  (with-open-file (inbytes (pathname fname) :element-type '(unsigned-byte 8))
    (loop :for pos = (read-sequence buffer inbytes)
	  :while (> pos 0)
	  :do (absorb-vec spritz buffer pos)))
  (absorb-stop spritz)
  (absorb-int-bytes spritz (length tgt))
  (squeeze spritz tgt))

(defun pprint-file-hashes (destination format size &rest fnames)
  "pretty-print the hashes of files designated by FNAMES into DESTINATION (t/nil/stream/etc)
via the format function. Hashes are of size SIZE and are formatted according to FORMAT as
:hex or :base64"
  (let ((hash-buf (make-array (ceiling size 8) :element-type '(unsigned-byte 8)))
	(file-buf (make-array 4096 :element-type '(unsigned-byte 8)))
	(spritz (make-ss)))
    (mapc #'(lambda (file)
	      (hash-file hash-buf file file-buf spritz)
	      (format destination "~A ~A~%"
		      (case format
			(:hex    (rt-bintext:hex-encode hash-buf))
			(:base64 (rt-bintext:base64-encode hash-buf))
			(t (error "pprint formats are :hex and :base64!")))
		      file))
	  fnames)))

(defun spritz-main ()
  (format t "hi~%"))
