(in-package "NAPA-FFT.IMPL")

(deftype direction ()
  '(member 1 -1 :fwd :inv :bwd))

(deftype scaling ()
  '(member nil 1 t :inv sqrt :sqrt))

(deftype windowing ()
  '(member nil float double-float complex complex-sample))

(defun find-index (direction scaling windowing)
  (check-type direction direction)
  (check-type scaling scaling)
  (check-type windowing windowing)
  (let ((direction (ecase direction
                     ((1 :fwd)       0)
                     ((-1 :inv :bwd) 1)))
        (scaling   (ecase scaling
                     ((nil 1)      0)
                     ((t :inv)     1)
                     ((sqrt :sqrt) 2)))
        (windowing (ecase windowing
                     ((nil)            0)
                     ((float
                       double-float)   1)
                     ((complex
                       complex-sample) 2))))
    (+ (* direction 3 3)
       (* scaling   3)
       windowing)))

(defun generate-fft (direction scaling windowing n)
  (declare (type direction direction)
           (type scaling   scaling)
           (type windowing windowing))
  (assert (= 1 (logcount n)))
  (let ((direction (ecase direction
                     ((1 :fwd) 1)
                     ((-1 :inv :bwd) -1)))
        (scaling   (ecase scaling
                     ((nil 1)   1d0)
                     ((t :inv)  (/ 1d0 n))
                     ((sqrt :sqrt) (/ (sqrt (float n 1d0))))))
        (windowing (ecase windowing
                     ((nil) nil)
                     ((float double-float) 'double-float)
                     ((complex complex-sample) 'complex-sample))))
    (compile
     nil
     `(lambda (vec start ,@(and windowing
                            `(window window-start))
               twiddle)
        (declare (type complex-sample-array vec twiddle)
                 (type index start ,@(and windowing
                                          '(window-start)))
                 (ignorable vec start twiddle)
                 ,@(and windowing
                        `((type (simple-array ,windowing 1)
                                window)))
                 (optimize speed (safety 0)))
        ,(funcall
          (ecase direction
            (1 'gen-dif)
            (-1 'gen-dit))
          n
          :scale scaling
          :window (and windowing
                       'window))
        vec))))

(defvar *fft-lock* (sb-thread:make-mutex))
(defvar *ffts* (map-into (make-array 18)
                         (lambda ()
                           (make-array 33 :initial-element nil))))

(defun %ensure-fft (direction scaling windowing n)
  (assert (= 1 (logcount n)))
  (let* ((index (find-index direction scaling windowing))
         (len   (lb n))
         (vec   (aref *ffts* index)))
    (assert (<= len 32))
    (block nil
      (flet ((check ()
               (let ((it (aref vec len)))
                 (when it
                   (return it)))))
        (check)
        (sb-thread:with-mutex (*fft-lock*)
          (check)
          (setf (aref vec len)
                (generate-fft direction scaling windowing
                              n)))))))

(defvar *bit-reverse-lock* (sb-thread:make-mutex))
(defvar *bit-reverses* (make-array 33 :initial-element nil))
(defvar *double-bit-reverses* (make-array 33 :initial-element nil))

(defun %ensure-reverse (n &optional (eltype 'complex-sample))
  (assert (member eltype '(complex-sample double-float)))
  (assert (= 1 (logcount n)))
  (let ((len (lb n))
        (vec (ecase eltype
               (complex-sample *bit-reverses*)
               (double-float   *double-bit-reverses*))))
    (block nil
      (flet ((check ()
               (let ((id (aref vec len)))
                 (when id
                   (return id)))))
        (check)
        (sb-thread:with-mutex (*bit-reverse-lock*)
          (check)
          (setf (aref vec len)
                (compile
                 nil
                 `(lambda (vec start tmp startt)
                    (declare (type (simple-array ,eltype 1)
                                   vec tmp)
                             (type index start startt)
                             (ignorable vec start
                                        tmp startt)
                             (optimize speed (safety 0)))
                    ,(gen-bit-reversal n eltype)
                    vec))))))))

(defun get-reverse (n &optional (eltype 'complex-sample))
  (let ((tmp (make-array n :element-type eltype))
        (fun (%ensure-reverse n eltype)))
    (lambda (vec)
      (funcall fun vec 0 tmp 0))))

(defvar *twiddle-lock* (sb-thread:make-mutex))
(defvar *forward-twiddle* nil)
(defvar *inverse-twiddle* nil)

(defun %ensure-twiddles (n forwardp)
  (assert (= 1 (logcount n)))
  (block nil
    (flet ((check ()
             (let ((vec (if forwardp
                            *forward-twiddle*
                            *inverse-twiddle*)))
               (when (and vec
                          (>= (length vec)
                              n))
                 (return vec)))))
      (check)
      (sb-thread:with-mutex (*twiddle-lock*)
        (check)
        (let* ((forward (make-twiddle n))
               (inverse (map-into (make-array
                                   n
                                   :element-type 'complex-sample)
                                  (lambda (x)
                                    (conjugate x))
                                  forward)))
          (declare (type complex-sample-array forward inverse))
          (setf *forward-twiddle* forward
                *inverse-twiddle* inverse)
          (if forwardp forward inverse))))))

(defun get-fft (size &key
                       (forward t)
                       (scale   (if forward nil :inv))
                       (in-order t))
  (let ((twiddle (%ensure-twiddles size forward))
        (fft     (%ensure-fft (if forward :fwd :inv)
                              scale
                              nil
                              size))
        (tmp     (make-array size :element-type 'complex-sample))
        (reversal (and in-order
                       (%ensure-reverse size))))
    (declare (type function fft)
             (type (or function null) reversal)
             (type complex-sample-array twiddle))
    (cond ((not reversal)
           (lambda (vec)
             (funcall fft vec 0 twiddle)))
          (forward
           (lambda (vec)
             (funcall fft vec 0 twiddle)
             (funcall reversal vec 0 tmp 0)))
          (t
           (lambda (vec)
             (funcall reversal vec 0 tmp 0)
             (funcall fft vec 0 twiddle))))))

(defun get-windowed-fft (size window-type
                         &key
                           (forward t)
                           (scale   (if forward nil :inv))
                           (in-order t))
  (assert window-type)
  (let ((twiddle (%ensure-twiddles size forward))
        (fft     (%ensure-fft (if forward :fwd :inv)
                              scale
                              window-type
                              size))
        (tmp     (make-array size :element-type 'complex-sample))
        (reversal (and in-order
                       (%ensure-reverse size))))
    (declare (type function fft)
             (type (or function null) reversal)
             (type complex-sample-array twiddle))
    (cond ((not reversal)
           (lambda (vec window)
             (funcall fft vec 0 window 0 twiddle)))
          (forward
           (lambda (vec window)
             (funcall fft vec 0 window 0 twiddle)
             (funcall reversal vec 0 tmp 0)))
          (t
           (lambda (vec window)
             (funcall reversal vec 0 tmp 0)
             (funcall fft vec 0 window 0 twiddle))))))
