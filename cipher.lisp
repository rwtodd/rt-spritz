;; S P R I T Z   C I P H E R

(in-package "RT-SPRITZ-CIPHER")

;; ss ==> "spritz state"
(defstruct ss
  (i 0 :type (unsigned-byte 8))
  (j 0 :type (unsigned-byte 8))
  (k 0 :type (unsigned-byte 8))
  (z 0 :type (unsigned-byte 8))
  (a 0 :type (unsigned-byte 8))
  (w 1 :type (unsigned-byte 8))
  (mem (let ((arr (make-array 256 :element-type '(unsigned-byte 8))))
	 (dotimes (idx 256 arr)
	   (setf (aref arr idx) idx)))
   :type (simple-array (unsigned-byte 8) (256))))

;; take an existing ss struct and reset it to initial values
(defun reset-ss (s)
  (declare (type ss s)
	   (optimize (speed 3)))
  (setf (ss-i s) 0
	(ss-j s) 0
	(ss-k s) 0
	(ss-z s) 0
	(ss-a s) 0
	(ss-w s) 1)
  (let ((mem (ss-mem s)))
    (dotimes (idx 256 s)
      (setf (aref mem idx) idx))))

(defun crush (s)
  (declare (type ss s)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((arr (ss-mem s)))
    (dotimes (idx 128)
      (let ((other (- 255 idx)))
	(if (> (aref arr idx) (aref arr other))
	    (rotatef (aref arr idx) (aref arr other)))))))

(defmacro u8+ (&rest args) `(logand (+ ,@args) 255))
(defmacro mem-at-sum (arr &rest args)
  `(aref ,arr (u8+ ,@args)))

(defun update (s times)
  (declare (type ss s)
	   (type fixnum times)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((i (ss-i s))
	(j (ss-j s))
	(k (ss-k s))
	(w (ss-w s))
	(mem (ss-mem s))) 
    (dotimes (_ times)
      (setq i (u8+ i w))
      (let ((mem-i (aref mem i)))
        (setq j (u8+ k (mem-at-sum mem j mem-i)))
	(let ((mem-j (aref mem j)))
          (setf k            (u8+ i k mem-j)
		(aref mem i) mem-j
		(aref mem j) mem-i))))
    (setf (ss-i s) i
	  (ss-j s) j
	  (ss-k s) k)))

(defun whip (s amt)
  (update s amt)
  (setf (ss-w s) (u8+ (ss-w s) 2)))

(defun shuffle (s)
  (whip s 512)
  (crush s)
  (whip s 512)
  (crush s)
  (whip s 512)
  (setf (ss-a s) 0))

(defmacro maybe-shuffle (s n)
  `(when (>= (ss-a ,s) ,n) (shuffle ,s)))

(defun absorb-nibble (s n)
  (declare (type ss s)
	   (type (unsigned-byte 4) n)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (maybe-shuffle s 128)
  (let ((mem (ss-mem s)))
    (declare (type (simple-array (unsigned-byte 8) (256)) mem))
    (rotatef (aref mem (ss-a s)) (aref mem (+ 128 n))))
  (incf (ss-a s)))

(defmacro absorb (s n)
  `(progn
     (absorb-nibble ,s (logand ,n 15))
     (absorb-nibble ,s (ash ,n -4))))

(defun absorb-vec (s bytes end)
  (declare (type (simple-array (unsigned-byte 8) 1) bytes)
	   (type ss s)
	   (type fixnum end)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x 0))
    (declare (type (unsigned-byte 8) x))
    (dotimes (idx end)
      (setq x (aref bytes idx))
      (absorb s x))))

(defun absorb-stop (s)
  (maybe-shuffle s 128)
  (incf (ss-a s)))

(defun drip (s)
  (declare (type ss s)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (maybe-shuffle s 1)
  (update s 1)
  (let ((mem (ss-mem s)))
    (setf (ss-z s) 
	  (mem-at-sum mem
		      (ss-j s)
		      (mem-at-sum mem
				  (ss-i s)
				  (mem-at-sum mem
					      (ss-z s)
					      (ss-k s)))))))

(defun squeeze (s tgt)
  (declare (type ss s)
	   (type (simple-array (unsigned-byte 8) 1) tgt)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (dotimes (idx (array-dimension tgt 0) tgt)
    (setf (aref tgt idx) (drip s))))

(defun squeeze-xor (s tgt &optional (end (array-dimension tgt 0)))
  (declare (type ss s)
	   (type (simple-array (unsigned-byte 8) 1) tgt)
	   (type fixnum end)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (dotimes (idx end tgt)
    (setf (aref tgt idx) (logxor (aref tgt idx) (drip s)))))
  
(defun absorb-int-bytes (s n)
  (declare (type ss s)
	   (type fixnum n))
  (if (> n 255)
      (absorb-int-bytes s (ash n -8)))
  (absorb s n))

(defun random-bytes (n)
  "create an array of N random bytes"
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (idx n v) (setf (aref v idx) (random 256)))))
