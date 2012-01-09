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

(defmacro define-inline-function (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args)
       ,@body)
     (define-compiler-macro ,name (&rest arg-forms)
       `((lambda (,@',args) ,@',body)
         ,@arg-forms))))

(define-inline-function mul+i (x)
  (declare (type complex-sample x))
  #+ (and sbcl complex-float-vops)
  (sb-vm::swap-complex (conjugate x))
  #- (and sbcl complex-float-vops)
  (* x #c(0 1d0)))

(define-inline-function mul-i (x)
  (declare (type complex-sample x))
  #+ (and sbcl complex-float-vops)
  (conjugate (sb-vm::swap-complex x))
  #- (and sbcl complex-float-vops)
  (* x #c(0 -1d0)))

(define-inline-function mul+/-sqrt+i (x scale)
  (declare (type complex-sample x)
           (type double-float scale))
  #+ (and sbcl complex-float-vops)
  (let ((x (* x scale)))
    (+ x (sb-vm::swap-complex (conjugate x))))
  #- (and sbcl complex-float-vops)
  (* x (complex scale scale)))

(define-inline-function mul+/-sqrt-i (x scale)
  (declare (type complex-sample x)
           (type double-float scale))
  #+ (and sbcl complex-float-vops)
  (let ((x (* x scale)))
    (- x (sb-vm::swap-complex (conjugate x))))
  #- (and sbcl complex-float-vops)
  (* x (complex scale (- scale))))

(defun mul-root (x root &optional default)
  (setf root (mod root 1))
  (case root
    (0 x)
    (1/2
     `(- ,x))
    (1/4
     `(mul+i ,x))
    (3/4
     `(mul-i ,x))
    ((1/8 5/8)
     `(mul+/-sqrt+i
       ,x
       ,(case root
          (1/8 (/ (sqrt 2d0) 2d0))
          (5/8 (- (/ (sqrt 2d0) 2d0))))))
    ((3/8 7/8)
     `(mul+/-sqrt-i
       ,x
       ,(case root
          (3/8 (- (/ (sqrt 2d0) 2d0)))
          (7/8 (/ (sqrt 2d0) 2d0)))))
    (t
     (assert default)
     `(* ,x ,default))))

(defun bit-reverse (array)
  (let ((dst (copy-seq array))
        (width (integer-length (1- (length array)))))
    (flet ((rev (x)
             (let ((acc 0))
               (loop repeat width
                     for bit = (logand x 1)
                     do (setf x (ash x -1))
                        (setf acc (logior (ash acc 1) bit))
                     finally (return acc)))))
      (dotimes (i (length array) dst)
        (setf (aref dst (rev i)) (aref array i))))))

(defconstant +twiddle-offset+ -1)

(defun make-twiddle (n)
  (assert (= 1 (logcount n)))
  (let ((vec (make-array n :element-type 'complex-sample
                           :initial-element (complex 0d0 0d0))))
    (loop for size = 4 then (* 2 size)
          while (<= size n)
          do (let ((start (+ (truncate size 2) +twiddle-offset+))
                   (base  (/ (* 2 pi) size)))
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

;; todo: unroll, fully or partially
(defun unrolled-for (count bindings body)
  `(progn
     ,@(loop
         for i below count
         collect
         `(symbol-macrolet ,(loop for (name start stride) in bindings
                                  collect (if (numberp stride)
                                              `(,name (+ ,start ,(* stride i)))
                                              `(,name (+ ,start (* ,stride ,i)))))
            ,@body))
     nil))

(defmacro for ((count &rest bindings) &body body)
  (let ((bindings (loop for binding in bindings
                        collect
                        (destructuring-bind (name &optional (start 0) (stride 1))
                            (if (consp binding)
                                binding
                                (list binding))
                          (list name start stride)))))
    (cond ((and (integerp count)
                (<= count 8)
                (not (find-if-not #'atom bindings :key #'second))
                (not (find-if-not #'constantp bindings :key #'third)))
           (unrolled-for count bindings body))
          ((and (integerp count)
                (zerop (mod count 4))
                (not (find-if-not #'constantp bindings :key #'third)))
           (let ((gensyms (mapcar (lambda (binding)
                                    (make-symbol (symbol-name (first binding))))
                                  bindings)))
             `(loop for ,(gensym "DUMMY") of-type index from ,(truncate count 4) above 0
                    ,@(loop for (name start stride) in bindings
                            for gensym in gensyms
                            append `(for ,gensym of-type index from ,start by ,(* stride 4)))
                    do (progn
                         ,@(loop
                             for i below 4
                             collect
                             `(symbol-macrolet
                                  ,(loop for (name start stride) in bindings
                                         for gensym in gensyms
                                         collect (if (numberp stride)
                                                     `(,name (+ ,gensym ,(* stride i)))
                                                     `(,name (+ ,gensym (* ,stride ,i)))))
                                ,@body))))))
          (t
           `(loop for ,(gensym "DUMMY") of-type index from ,count above 0
                  ,@(loop for (name start stride) in bindings
                          append `(for ,name of-type index from ,start by ,stride))
                  do (locally
                         ,@body))))))

