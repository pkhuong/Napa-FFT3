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

(define-inline-function %scale (x scale)
  (declare (type complex-sample x)
           (type double-float scale)
           (muffle-conditions sb-ext:code-deletion-note))
  (case scale
    (1d0 x)
    (-1d0 (- x))
    (t  (* x scale))))

(define-inline-function %window (x window i)
  (declare (type complex-sample x)
           (type (or null (simple-array * 1)) window)
           (type index i)
           (muffle-conditions sb-ext:code-deletion-note))
  (if window
      (* x (aref window i))
      x))

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

(defun emit-unrolled-for (count bindings body)
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

(define-symbol-macro %blocking-factor% 4)
(define-symbol-macro %unroll-count% 8)

(defmacro for ((count &rest bindings) &body body
               &environment env)
  (let ((bindings (loop for binding in bindings
                        collect
                        (destructuring-bind (name &optional (start 0) (stride 1))
                            (if (consp binding)
                                binding
                                (list binding))
                          (list name start stride))))
        (unroll (macroexpand-1 '%unroll-count% env))
        (blocking (macroexpand-1 '%blocking-factor% env)))
    (cond ((and (integerp count)
                (<= count unroll)
                (not (position-if-not #'atom bindings :key #'second))
                (not (position-if-not #'constantp bindings :key #'third)))
           (emit-unrolled-for count bindings body))
          ((and (integerp count)
                (zerop (mod count blocking))
                (not (find-if-not #'constantp bindings :key #'third)))
           (let ((gensyms (mapcar (lambda (binding)
                                     (make-symbol (symbol-name
                                                   (first binding))))
                                   bindings)))
             `(loop for ,(gensym "DUMMY") of-type index
                      from ,(truncate count
                                      blocking)
                      above 0
                    ,@(loop for (name start stride) in bindings
                            for gensym in gensyms
                            append `(for ,gensym of-type index from ,start
                                      by ,(* stride blocking)))
                    do (progn
                         ,@(loop
                             for i below blocking
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

(defun iota (n)
  (let ((count 0))
    (map-into (make-array n :element-type 'complex-sample)
              (lambda ()
                (complex (1- (incf count))
                         1d0)))))

(defun lb (n)
  (integer-length (1- n)))
