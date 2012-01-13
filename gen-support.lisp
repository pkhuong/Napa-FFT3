(in-package "NAPA-FFT.GEN")

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

(defun butterfly (i j)
  (setf (values (@ i) (@ j))
        (op (complex-sample complex-sample)
            `(lambda (x y)
               (values (+ x y)
                       (- x y)))
            (@ i) (@ j))))

(defun rotate (i k root)
  (setf (@ i)
        (op (complex-sample)
            `(lambda (x)
               ,(mul-root 'x root
                          (and k
                               `(aref twiddle ,k))))
            (@ i))))

(defun scale (i scale window &optional (window-i
                                        `(+ window-start ,i)))
  (setf (@ i)
        (op (complex-sample)
            `(lambda (x)
               (%window (%scale x ,scale)
                        ,window
                        ,(if window
                             window-i
                             0)))
            (@ i))))

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
                (> blocking 1)
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

