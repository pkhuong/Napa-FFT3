(in-package "NAPA-FFT.IMPL")

(declaim (inline swap))
(defun swap (x)
  (declare (type complex-sample x))
  #+(and sbcl complex-float-vops)
  (sb-vm::swap-complex x)
  (complex (imagpart x) (realpart x)))

(declaim (inline fft-swizzled-reals))
(defun fft-swizzled-reals (vec scale)
  (declare (type complex-sample-array vec))
  (let* ((size (length vec))
         (n/2 (truncate size 2)))
    (fft vec :dst vec :size n/2 :scale scale)
    (let ((x (aref vec 0)))
      (setf (aref vec n/2) (complex (imagpart x))
            (aref vec 0)   (complex (realpart x))))
    (loop for i of-type index from 1 upto (truncate n/2 2)
          for j of-type index from (1+ n/2)
          do (let* ((x   (aref vec i))
                    (xs  (swap x))
                    (y   (aref vec (- n/2 i)))
                    (ys  (swap y))
                    (xj  (* .5d0 (+ (conjugate xs) ys)))
                    (-xj (* .5d0 (+ (conjugate ys) xs))))
               (setf (aref vec j)          xj
                     (aref vec (- size i)) -xj
                     (aref vec i)          (- x (napa-fft.gen::mul+i xj))
                     (aref vec (- n/2 i))  (- y (napa-fft.gen::mul+i -xj)))))
    vec))

(defvar *rfft-twiddles* nil)
(defvar *rifft-twiddles* nil)

(defun %get-radix-2-twiddle (n direction)
  (let* ((n/2     (truncate n 2))
         (twiddle (make-array n/2 :element-type 'complex-sample)))
    (let ((root (if (plusp direction)
                    (complex 1d0 0d0)
                    (complex 0d0 1d0)))
          (mul  (exp (* -2 pi (complex 0d0 direction) (/ 1d0 n)))))
      (declare (type complex-sample root mul))
      (loop for k below n/2 do
            (setf (aref twiddle k) root
                  root             (* root mul)))
      twiddle)))

(defun get-radix-2-twiddle (n direction)
  (assert (power-of-two-p n))
  (assert (member direction '(-1d0 1d0)))
  (let ((twiddles (ecase direction
                    (1d0
                     (or *rfft-twiddles*
                         (setf *rfft-twiddles*
                               (make-array 32 :initial-element nil))))
                    (-1d0
                     (or *rifft-twiddles*
                         (setf *rifft-twiddles*
                               (make-array 32 :initial-element nil))))))
        (len (lb n)))
    (or (aref twiddles len)
        (setf (aref twiddles len)
              (%get-radix-2-twiddle n direction)))))

(defmacro with-scale ((scale) &body body)
  `(ecase ,scale
     ((nil 1)
      (flet ((scale (x)
               x))
        (declare (inline scale))
        ,@body))
     ((t :inv)
      (flet ((scale (x)
               (* x .5d0)))
        (declare (inline scale))
        ,@body))
     ((:sqrt sqrt)
      (flet ((scale (x)
               (* x ,(sqrt .5d0))))
        (declare (inline scale))
        ,@body))))

(defun rfft (vec &key dst
                   size
                   (scale nil))
  (declare (type scaling scale)
           (optimize speed))
  (let* ((vec (real-samplify vec))
         (size (or size (length vec))))
    (declare (type real-sample-array vec)
             (type size size))
    (assert (>= (length vec) size))
    (assert (power-of-two-p size))
    (let* ((n   size)
           (n/2 (truncate n 2))
           (dst (or dst
                    (make-array n :element-type 'complex-sample)))
           (twiddle (get-radix-2-twiddle n 1d0)))
      (declare (type complex-sample-array twiddle)
               (type complex-sample-array dst))
      (assert (>= (length dst) size))
      (locally (declare (optimize (safety 0)))
        (with-scale (scale)
          (loop for i of-type index below n by 2
                for j of-type index from 0
                do (setf (aref dst j)
                         (scale (complex (aref vec i)
                                         (aref vec (+ i 1)))))))
        (fft-swizzled-reals dst scale)
        (loop for i of-type index below n/2
              for j of-type index from n/2
              do
                 (let* ((x (aref dst i))
                        (y (* (aref dst j) (aref twiddle i))))
                   (setf (aref dst i) (+ x y)
                         (aref dst j) (- x y)))))
      dst)))

(defun windowed-rfft (signal-vector center length
                      &key (window-fn 'hann)
                           dst
                           (scale     nil))
  (declare (type index length)
           (optimize speed))
  (assert (power-of-two-p length))
  (let* ((signal-vector (real-samplify signal-vector))
         (input-window (extract-centered-window signal-vector center length))
         (window       (window-vector window-fn length)))
    (declare (type real-sample-array signal-vector input-window window))
    (map-into input-window #'* input-window window)
    (rfft input-window
          :dst dst
          :scale scale)))

(defun rifft (vec &key dst size (scale t))
  (declare (type scaling scale)
           (optimize speed))
  (let* ((vec  (complex-samplify vec))
         (size (or size (length vec))))
    (declare (type size size))
    (assert (>= (length vec) size))
    (assert (power-of-two-p size))
    (let* ((n   size)
           (n/2 (truncate n 2))
           (twiddle (get-radix-2-twiddle n -1d0))
           (dst (or dst
                    (make-array n :element-type 'double-float))))
      (declare (type complex-sample-array twiddle)
               (type real-sample-array dst))
      (assert (>= (length dst) size))
      (locally (declare (optimize (safety 0)))
        (loop for i of-type index below n/2
              for j of-type index from n/2
              do (let* ((x (aref vec i))
                        (y (aref vec j))
                        (sum (+ x y))
                        (sub (* (- x y)
                                (aref twiddle i))))
                   (setf (aref vec i) (+ sum sub))))
        (ifft vec :dst vec :size n/2 :scale scale)
        (with-scale (scale)
          (loop for i of-type index below n/2
                for j of-type index by 2
                do (let ((x (scale (aref vec i))))
                     (setf (aref dst j) (realpart x)
                           (aref dst (1+ j)) (imagpart x))))))
      dst)))

