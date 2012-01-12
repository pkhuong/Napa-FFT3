(in-package "NAPA-FFT.IMPL")

(defvar *scratch* nil)
(defvar *double-scratch* nil)

(declaim (inline copy-or-replace))
(defun copy-or-replace (src dst)
  (cond ((eql src dst) dst)
        (dst
         (replace dst src))
        (t (copy-seq src))))

(defun %bit-reverse-double (vec dst)
  (declare (type (simple-array double-float 1) vec)
           (type (or null (simple-array double-float 1)) dst))
  (let* ((dst (copy-or-replace vec dst))
         (n   (length vec))
         (scratch (or *double-scratch*
                      (make-array n :element-type 'double-float)))
         (fun (%ensure-reverse n 'double-float)))
    (declare (type (simple-array double-float 1) scratch))
    (when (< (length scratch) n)
      (setf scratch (make-array n :element-type 'double-float)
            *double-scratch* scratch))
    (funcall fun dst 0 scratch 0)))

(defun %bit-reverse-complex (vec dst)
  (declare (type complex-sample-array vec)
           (type (or null complex-sample-array) dst))
  (let* ((dst (copy-or-replace vec dst))
         (n   (length vec))
         (scratch (or *scratch*
                      (make-array n :element-type 'complex-sample)))
         (fun (%ensure-reverse n)))
    (declare (type complex-sample-array scratch))
    (when (< (length scratch) n)
      (setf scratch (make-array n :element-type 'complex-sample)
            *scratch* scratch))
    (funcall fun dst 0 scratch 0)))

(defun bit-reverse (vec &optional dst)
  (etypecase vec
    (complex-sample-array
     (%bit-reverse-complex vec dst))
    ((simple-array double-float 1)
     (%bit-reverse-double vec dst))))

(defun get-window-type (window)
  (etypecase window
    (complex-sample-array 'complex-sample)
    ((simple-array double-float 1) 'double-float)))

(declaim (inline complex-samplify))
(defun complex-samplify (vec)
  (etypecase vec
    (complex-sample-array vec)
    ((simple-array double-float 1)
     (map-into (make-array (length vec)
                           :element-type 'complex-sample)
               (lambda (x)
                 (coerce x 'complex-sample))
               vec))
    (t
     (map-into (make-array (length vec)
                           :element-type 'complex-sample)
               (lambda (x)
                 (coerce x 'complex-sample))
               vec))))

(defun fft (vec &key dst (in-order t) (scale nil) (window nil))
  (declare (type (or null complex-sample-array) dst))
  (let* ((vec (complex-samplify vec))
         (dst (copy-or-replace vec dst))
         (n   (length vec)))
    (if window
        (funcall (get-windowed-fft n (get-window-type window)
                                   :scale scale
                                   :in-order nil)
                 dst window)
        (funcall (get-fft n :scale scale :in-order nil)
                 dst))
    (if in-order
        (bit-reverse dst dst)
        dst)))

(defun ifft (vec &key dst (in-order t) (scale t) (window nil))
  (declare (type (or null complex-sample-array) dst))
  (let* ((vec (complex-samplify vec))
         (dst (copy-or-replace vec dst))
         (n   (length vec)))
    (when in-order
      (bit-reverse dst dst))
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

