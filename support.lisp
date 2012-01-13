(in-package "NAPA-FFT.SUPPORT")

(deftype index ()
  `(unsigned-byte #. (min (1- (integer-length most-positive-fixnum))
                          (integer-length (1- array-dimension-limit)))))

(deftype half-index ()
  `(unsigned-byte #.(truncate (integer-length most-positive-fixnum) 2)))

(deftype size ()
  `(and (integer 1) index))

(deftype half-size ()
  `(and (integer 1) half-index))

(deftype complex-sample ()
  `(complex double-float))

(deftype complex-sample-array (&optional size)
  `(simple-array complex-sample (,size)))

(deftype real-sample ()
  'double-float)

(deftype real-sample-array (&optional size)
  `(simple-array real-sample (,size)))

(defun bit-reverse-integer (x width)
  (let ((acc 0))
    (loop repeat width
          for bit = (logand x 1)
          do (setf x (ash x -1))
             (setf acc (logior (ash acc 1) bit))
          finally (return acc))))

(defconstant +twiddle-offset+ -1)

(defun make-twiddle (n &optional (dir 1d0))
  (assert (power-of-two-p n))
  (check-type dir (member 1d0 -1d0))
  (let ((vec (make-array n :element-type 'complex-sample
                           :initial-element (complex 0d0 0d0))))
    (loop for size = 4 then (* 2 size)
          while (<= size n)
          do (let ((start (+ (truncate size 2) +twiddle-offset+))
                   (base  (/ (* dir 2 pi) size)))
               (dotimes (i (truncate size 4))
                 (let* ((theta (* -1 i base))
                        (t1 (cis theta))
                        (t2 (cis (* 3 theta))))
                   (setf (aref vec (+ start (* 2 i)))   t1
                         (aref vec (+ start 1 (* 2 i))) t2)))))
    vec))

(declaim (inline power-of-two-p lb))
(defun lb (n)
  (integer-length (1- n)))

(defun power-of-two-p (x)
  (= 1 (logcount x)))

(declaim (ftype (function (sequence) (values complex-sample-array &optional))
                complex-samplify))
(defun complex-samplify (vec)
  (etypecase vec
    (complex-sample-array vec)
    ((simple-array (complex single-float) 1)
     (map-into (make-array (length vec)
                           :element-type 'complex-sample)
               (lambda (x)
                 (coerce x 'complex-sample))
               vec))
    (real-sample-array
     (map-into (make-array (length vec)
                           :element-type 'complex-sample)
               (lambda (x)
                 (coerce x 'complex-sample))
               vec))
    ((simple-array single-float 1)
     (map-into (make-array (length vec)
                           :element-type 'complex-sample)
               (lambda (x)
                 (coerce x 'complex-sample))
               vec))
    (sequence
     (map-into (make-array (length vec)
                           :element-type 'complex-sample)
               (lambda (x)
                 (coerce x 'complex-sample))
               vec))))

(declaim (ftype (function (sequence) (values real-sample-array &optional))
                real-samplify))
(defun real-samplify (vec)
  (etypecase vec
    (real-sample-array vec)
    ((simple-array single-float 1)
     (map-into (make-array (length vec)
                           :element-type 'real-sample)
               (lambda (x)
                 (coerce x 'real-sample))
               vec))
    (sequence
     (map-into (make-array (length vec)
                           :element-type 'real-sample)
               (lambda (x)
                 (coerce x 'real-sample))
               vec))))

