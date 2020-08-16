(in-package "RT-SPRITZ")

(defconstant +BUFSZ+ 4096)

(defun hash-into (tgt src)
  "compute the hash of the SRC bytes into the TGT array, returning TGT"
  (declare (type (simple-array (unsigned-byte 8) 1) tgt src))
  (let ((s (cipher:make-ss)))
    (cipher:absorb-vec s src (array-dimension src 0))
    (cipher:absorb-stop s)
    (cipher:absorb-int-bytes s (array-dimension tgt 0))
    (cipher:squeeze s tgt)
    tgt))

(defun hash-file (tgt fname &optional
			      (buffer (make-array +BUFSZ+ :element-type '(unsigned-byte 8)))
			      (spritz (cipher:make-ss) supplied))
  "compute the spriz hash of FNAME into a TGT buffer"
  (when supplied (cipher:reset-ss spritz))
  (with-open-file (inbytes (pathname fname) :element-type '(unsigned-byte 8))
    (loop :for pos = (read-sequence buffer inbytes)
	  :while (> pos 0)
	  :do (cipher:absorb-vec spritz buffer pos)))
  (cipher:absorb-stop spritz)
  (cipher:absorb-int-bytes spritz (array-dimension tgt 0))
  (cipher:squeeze spritz tgt))

(defun pprint-file-hashes (destination format size &rest fnames)
  "pretty-print the hashes of files designated by FNAMES into DESTINATION (t/nil/stream/etc)
via the format function. Hashes are of size SIZE and are formatted according to FORMAT as
:hex or :base64"
  (let ((hash-buf (make-array (ceiling size 8) :element-type '(unsigned-byte 8)))
	(file-buf (make-array +BUFSZ+ :element-type '(unsigned-byte 8)))
	(spritz (cipher:make-ss)))
    (mapc #'(lambda (file)
	      (hash-file hash-buf file file-buf spritz)
	      (format destination "~A ~A~%"
		      (case format
			(:hex    (rt-bintext:hex-encode hash-buf))
			(:base64 (rt-bintext:base64-encode hash-buf))
			(t (error "pprint formats are :hex and :base64!")))
		      file))
	  fnames)))

(defun memcpy (tgt src)
  "copy src into tgt"
  (declare (type (simple-array (unsigned-byte 8) 1) tgt src)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (dotimes (idx (array-dimension tgt 0) tgt)
    (setf (aref tgt idx) (aref src idx))))

(defun keygen (n iv pwhash)
  "generate a key from IV and PWHASH, using N rounds, returning the key array"
  (declare (type (simple-array (unsigned-byte 8) (64)) pwhash)
	   (type (simple-array (unsigned-byte 8) (4)) iv)
	   (type fixnum n)
	   (optimize (speed 3) (debug 0) (safety 0)))
  (let ((s       (cipher:make-ss))
	(iv-copy (copy-seq iv))
	(tgt     (copy-seq pwhash))
	(bias    0))
    (declare (type cipher:ss s)
	     (type (simple-array (unsigned-byte 8) (4)) iv-copy)
	     (type (simple-array (unsigned-byte 8) (64)) tgt)
	     (type (integer 0 3) bias))
    (memcpy tgt pwhash)
    (dotimes (_ n tgt)
      (setq bias (logand (aref iv-copy 0) 3))
      (cipher:absorb-vec s iv-copy 4)
      (cipher:absorb-stop s)
      (loop :for idx :from bias :below 4
	    :do (cipher:absorb-int-bytes s (aref iv idx)))
      (cipher:absorb-stop s)
      (cipher:absorb-vec s tgt 64)
      (cipher:absorb-stop s)
      (cipher:squeeze s tgt)
      (cipher:squeeze s iv-copy))))
  
(defstruct header
  (iv      (cipher:random-bytes 4) :type (simple-array (unsigned-byte 8) (4)))
  (chk     (cipher:random-bytes 4) :type (simple-array (unsigned-byte 8) (4)))
  (hsh-chk (make-array 4 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (4)))
  (key     (cipher:random-bytes 64) :type (simple-array (unsigned-byte 8) (64))))

(defun generate-skipped-stream (key n)
  "create a stream based on KEY, with 2048+N values skipped"
  (let ((s (cipher:make-ss)))
    (cipher:absorb-vec s key 64)
    (dotimes (_ (+ 2048 n) s)
      (cipher:drip s))))

(defun encrypt-header (hdr pw-hash)
  "take a header with all the parts filled in, and encrypt it against pw-hash"
  (declare (type header hdr)
	   (type (simple-array (unsigned-byte 8) (64)) pw-hash))
  (let* ((iv         (copy-seq (header-iv hdr)))
	 (key        (keygen (+ 20000 (aref iv 3)) iv pw-hash))
	 (s          (generate-skipped-stream key (aref iv 1)))
	 (extra-skip (+ (aref (header-chk hdr) 0) 5)))
    (declare (type (simple-array (unsigned-byte 8) (4)) iv)
	     (type (simple-array (unsigned-byte 8) (64)) key)
	     (type cipher:ss s))
    ;; xor the IV with the last bytes of the pw-hash
    (dotimes (idx 4) (setf (aref (header-iv hdr) idx)
			   (logxor (aref (header-iv hdr) idx)
				   (aref pw-hash (+ idx 60)))))
    ;; use the skipped stream to encrypt the check int and its hash
    (cipher:squeeze-xor s (header-chk hdr))
    (cipher:squeeze-xor s (header-hsh-chk hdr))
    ;; now skip even more bytes in the stream, dependent on the check int
    (dotimes (_ extra-skip)
      (cipher:drip s))
    ;; now encrypt the header's encryption key
    (cipher:squeeze-xor s (header-key hdr)))
  ;; return the header
  hdr)

(defun write-header (hdr outstr)
  "write the header contents to outstr"
  (declare (type header hdr))
  (write-sequence (header-iv hdr)      outstr)
  (write-sequence (header-chk hdr)     outstr)
  (write-sequence (header-hsh-chk hdr) outstr)
  (write-sequence (header-key hdr)     outstr))

(defun stream-xor-copy (enc in out)
  "copy IN to OUT, xoring the bytes with ENC along the way"
  (let ((buffer (make-array +BUFSZ+ :element-type '(unsigned-byte 8))))
    (loop :for pos = (read-sequence buffer in)
	  :while (> pos 0)
	  :do (cipher:squeeze-xor enc buffer pos)
	      (write-sequence buffer out :end pos))))

(defun encrypt-stream (instr outstr pw-hash)
  "encrypt the contents of instr to outstr, using hashed password pw-hash"
  (declare (type (simple-array (unsigned-byte 8) (64)) pw-hash))
  (let* ((hdr (make-header))
	 (encstr (generate-skipped-stream (header-key hdr) (aref (header-chk hdr) 1))))
    (hash-into (header-hsh-chk hdr) (header-chk hdr))
    (encrypt-header hdr pw-hash)
    (write-header hdr outstr)
    (stream-xor-copy encstr instr outstr)))

(defun encrypt-file (infname outfname pw)
  (let* ((pw-string (sb-ext:string-to-octets pw))
	 (hashed-pw (hash-into (make-array 64 :element-type '(unsigned-byte 8))
			       pw-string)))
    (with-open-file (inp infname :element-type '(unsigned-byte 8))
      (with-open-file (outp outfname :direction :output :element-type '(unsigned-byte 8))
	(encrypt-stream inp outp hashed-pw)))))

(defun spritz-main ()
  (format t "hi~%"))
