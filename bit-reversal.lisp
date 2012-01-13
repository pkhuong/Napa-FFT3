(in-package "NAPA-FFT.GEN")

(defun emit-small-bit-reverse-swap (n &aux
                                        (width (lb n)))
  (assert (power-of-two-p n))
  `(progn
     ,@(loop
         for i below n
         for rev = (bit-reverse-integer i width)
         when (<= i rev)
         collect
         (if (= i rev)
             `(rotatef (aref vec (+ start1 ,i))
                       (aref vec (+ start2 ,i)))
             `(progn
                (rotatef (aref vec (+ start1 ,i))
                         (aref vec (+ start2 ,rev)))
                (rotatef (aref vec (+ start2 ,i))
                         (aref vec (+ start1 ,rev))))))))

(defun emit-small-bit-reverse (n &aux
                                   (width (lb n)))
  (assert (power-of-two-p n))
  `(progn
     ,@(loop
         for i below n
         for rev = (bit-reverse-integer i width)
         when (< i rev)
         collect
         `(rotatef (aref vec (+ start ,i))
                   (aref vec (+ start ,rev))))))

(defun emit-small-bit-reverse-copy (n &aux
                                        (width (lb n)))
  (assert (power-of-two-p n))
  `(progn
     ,@(loop
         for i below n
         for rev = (bit-reverse-integer i width)
         collect
         `(setf (aref dst (+ startd ,i))
                (aref src (+ starts ,rev))))))

(defun emit-radix-n-reversal (radix n)
  (assert (power-of-two-p radix))
  (assert (power-of-two-p n))
  (assert (>= n radix))
  (let ((div (truncate n radix))
        (width (lb radix)))
    `(for (,div
           (src-i starts ,radix)
           (dst-i startd))
       ,@(loop
           for i below radix
           collect `(setf (aref dst (+ dst-i
                                       ,(* (bit-reverse-integer
                                            i width)
                                           div)))
                          (aref src (+ src-i ,i)))))))

(defvar *max-small-bit-reverse* 256)
(defvar *max-reversal-radix* 8)

(defun gen-bit-reversal (n eltype)
  (assert (power-of-two-p n))
  (labels ((rec (n vec tmp)
             (cond ((> n *max-small-bit-reverse*)
                    (let* ((radix (min *max-reversal-radix*
                                       (the size
                                            (/ n *max-small-bit-reverse*))))
                           (sub   (/ n radix)))
                      `(progn
                         (let ((dst ,tmp)
                               (src ,vec)
                               (startd startt)
                               (starts start))
                           (declare (type (simple-array ,eltype 1) dst src)
                                    (type index starts startd))
                           ,(emit-radix-n-reversal radix n))
                         (for (,radix
                               (tmp-i startt ,sub)
                               (vec-i start  ,sub))
                           (let ((startt vec-i)
                                 (start  tmp-i))
                             (declare (type index start startt)
                                      (ignorable start startt))
                             ,(rec sub tmp vec))))))
                   ((eql vec 'vec)
                    (emit-small-bit-reverse n))
                   (t
                    (assert (eql tmp 'vec))
                    `(let ((src ,vec)
                           (dst ,tmp)
                           (starts start)
                           (startd startt))
                       (declare (type (simple-array ,eltype 1)
                                      src dst)
                                (type index starts startd))
                       ,(emit-small-bit-reverse-copy n))))))
    ;; args: dst tmp startd startt
    `(symbol-macrolet ((%unroll-count% 1)
                       (%blocking-factor% ,most-positive-fixnum))
       ,(rec n 'vec 'tmp))))
