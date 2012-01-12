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

(defun bit-reverse-integer (x width)
  (let ((acc 0))
    (loop repeat width
          for bit = (logand x 1)
          do (setf x (ash x -1))
             (setf acc (logior (ash acc 1) bit))
          finally (return acc))))

(defun slow-bit-reverse (array)
  (let ((dst (copy-seq array))
        (width (integer-length (1- (length array)))))
    (flet ((rev (x)
             (bit-reverse-integer x width)))
      (dotimes (i (length array) dst)
        (setf (aref dst (rev i)) (aref array i))))))

(defconstant +twiddle-offset+ -1)

(defun make-twiddle (n &optional (dir 1d0))
  (assert (= 1 (logcount n)))
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

(defun impulse (i n)
  (let ((vec (make-array n :element-type 'complex-sample
                           :initial-element (complex 0d0 0d0))))
    (setf (aref vec i) (complex 1d0 0d0))
    vec))

(defun iota (n)
  (let ((count 0))
    (map-into (make-array n :element-type 'complex-sample)
              (lambda ()
                (complex (1- (incf count))
                         1d0)))))

(defun lb (n)
  (integer-length (1- n)))

(defun make-vector (n)
  (make-array n :element-type 'complex-sample))

(defun random-vector (n &optional (dst (make-vector n)))
  (declare (type complex-sample-array dst))
  (unless (= n (length dst))
    (setf dst (make-array n :element-type 'complex-sample)))
  (map-into dst (lambda ()
                  (complex (- (random 2d0) 1d0)
                           (- (random 2d0) 1d0)))))

(macrolet ((define-mfun (name op)
             `(defun ,name (x y &optional (dst (make-vector (length x))))
                (declare (type complex-sample-array x y dst))
                (map-into dst #',op x y))))
  (define-mfun m+ +)
  (define-mfun m- -)
  (define-mfun m* *))

(defvar *default-abs-tol* 1d-6)

(defun m= (x y &optional (tol *default-abs-tol*))
  (declare (type complex-sample-array x y)
           (type double-float tol))
  (let ((worst 0d0))
    (declare (type double-float worst))
    (dotimes (i (length x))
      (let ((x (aref x i))
            (y (aref y i)))
        (let ((delta (abs (- x y))))
          (if (< delta tol)
              (setf worst (max worst delta))
              (return-from m= (values nil delta i))))))
    (values t worst nil)))
