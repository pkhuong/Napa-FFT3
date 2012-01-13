(in-package "NAPA-FFT.IMPL")

(defvar *scratch* nil)
(defvar *double-scratch* nil)

(declaim (inline copy-or-replace))
(defun copy-or-replace (src dst)
  (cond ((eql src dst) dst)
        (dst
         (replace dst src))
        (t (copy-seq src))))

(defun %bit-reverse-double (vec dst size)
  (declare (type (simple-array double-float 1) vec)
           (type (or null (simple-array double-float 1)) dst)
           (type index size))
  (assert (>= (length vec) size))
  (let* ((dst (copy-or-replace vec dst))
         (n   size)
         (scratch (or *double-scratch*
                      (make-array n :element-type 'double-float)))
         (fun (%ensure-reverse n 'double-float)))
    (declare (type (simple-array double-float 1) scratch))
    (assert (>= (length dst) size))
    (when (< (length scratch) n)
      (setf scratch (make-array n :element-type 'double-float)
            *double-scratch* scratch))
    (funcall fun dst 0 scratch 0)))

(defun %bit-reverse-complex (vec dst size)
  (declare (type complex-sample-array vec)
           (type (or null complex-sample-array) dst)
           (type index size))
  (assert (>= (length vec) size))
  (let* ((dst (copy-or-replace vec dst))
         (n   size)
         (scratch (or *scratch*
                      (make-array n :element-type 'complex-sample)))
         (fun (%ensure-reverse n)))
    (declare (type complex-sample-array scratch))
    (assert (>= (length dst) size))
    (when (< (length scratch) n)
      (setf scratch (make-array n :element-type 'complex-sample)
            *scratch* scratch))
    (funcall fun dst 0 scratch 0)))

(defun bit-reverse (vec &optional dst (size (length vec)))
  (etypecase vec
    (complex-sample-array
     (%bit-reverse-complex vec dst size))
    ((simple-array double-float 1)
     (%bit-reverse-double vec dst size))))

(defun get-window-type (window)
  (etypecase window
    (complex-sample-array 'complex-sample)
    ((simple-array double-float 1) 'double-float)))

(defun fft (vec &key dst
                  (size (length vec))
                  (in-order t) (scale nil) (window nil))
  (declare (type (or null complex-sample-array) dst))
  (assert (power-of-two-p size))
  (assert (>= (length vec) size))
  (let* ((vec (complex-samplify vec))
         (dst (copy-or-replace vec dst))
         (n   size))
    (assert (>= (length dst) size))
    (if window
        (funcall (get-windowed-fft n (get-window-type window)
                                   :scale scale
                                   :in-order nil)
                 dst window)
        (funcall (get-fft n :scale scale :in-order nil)
                 dst))
    (if in-order
        (bit-reverse dst dst n)
        dst)))

(defun ifft (vec &key dst
                   (size (length vec))
                   (in-order t) (scale t) (window nil))
  (declare (type (or null complex-sample-array) dst))
  (assert (power-of-two-p size))
  (assert (>= (length vec) size))
  (let* ((vec (complex-samplify vec))
         (dst (copy-or-replace vec dst))
         (n   size))
    (assert (>= (length dst) size))
    (when in-order
      (bit-reverse dst dst n))
    (if window
        (funcall (get-windowed-fft n (get-window-type window)
                                   :forward nil
                                   :scale scale
                                   :in-order nil)
                 dst window)
        (funcall (get-fft n
                          :forward nil
                          :scale scale
                          :in-order nil)
                 dst))))

