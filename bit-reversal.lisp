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

(defun z-order-words (pairs)
  (flet ((interleave (x y)
           (let ((acc 0)
                 (mul 0))
             (dotimes (i 32 acc)
               (let ((x (ldb (byte 1 i) x))
                     (y (ldb (byte 1 i) y)))
                 (setf acc (logior (ash (+ (* 2 x) y)
                                        mul)
                                   acc)
                       mul (+ 2 mul)))))))
    (let ((interleaved (map 'simple-vector
                            (lambda (pair)
                              (cons (interleave (car pair)
                                                (cdr pair))
                                    pair))
                            pairs)))
      (sort interleaved #'< :key #'car)
      (map-into interleaved #'cdr interleaved))))

(defun generate-leaf-reverse (outer inner)
  (let ((to-swap '())
        (total    (+ outer inner)))
    (dotimes (i (ash 1 outer))
      (let ((ri (bit-reverse-integer i outer)))
        (dotimes (j (ash 1 outer))
          (let* ((rj (bit-reverse-integer j outer))
                 (a (+ (ash i  total) j))
                 (b (+ (ash rj total) ri)))
            (when (< a b)
              (push (cons a b) to-swap))))))
    `(progn
       ,@(loop
           for (a . b) across (z-order-words to-swap)
           collect `(let ((a (aref vec (+ middle ,a)))
                          (b (aref vec (+ middle ,b))))
                      (setf (aref vec (+ middle ,a)) b
                            (aref vec (+ middle ,b)) a))))))

(defun generate-leaf-reverse-swap (outer inner)
  (let ((to-swap '())
        (total    (+ outer inner)))
    (dotimes (i (ash 1 outer))
      (let ((ri (bit-reverse-integer i outer)))
        (dotimes (j (ash 1 outer))
          (let* ((rj (bit-reverse-integer j outer))
                 (a  (+ (ash i  total) j))
                 (b  (+ (ash rj total) ri)))
            (push (cons a b) to-swap)))))
    `(progn
       ,@(loop
           for (a . b) across (z-order-words to-swap)
           collect
           `(let ((a (aref vec (+ middle-1 ,a)))
                  (b (aref vec (+ middle-2 ,b))))
              (setf (aref vec (+ middle-1 ,a)) b
                    (aref vec (+ middle-2 ,b)) a))))))

(defun generate-large-reversal (outer inner)
  (let ((to-swap '()))
    (dotimes (i (ash 1 inner))
      (let ((ri (bit-reverse-integer i inner)))
        (when (<= i ri)
          (push (cons i ri) to-swap))))
    (let* ((ordered (z-order-words to-swap))
           (data (make-array (* 2 (length ordered))
                             :element-type '(unsigned-byte 32))))
      (loop for i upfrom 0 by 2
            for (a . b) across ordered
            do (setf (aref data      i) (ash a outer)
                     (aref data (1+ i)) (ash b outer)))
      `(flet ((rev (middle)
                (declare (type index middle))
                ,(generate-leaf-reverse outer inner)
                vec)
              (swap (middle-1 middle-2)
                (declare (type index middle-1 middle-2))
                ,(generate-leaf-reverse-swap outer inner)
                vec))
         (declare (ignorable #'swap #'rev))
         ,(if (> (length ordered) 8)
              `(let ((data ,data))
                 (loop for i of-type index below ,(length data) by 2 do
                   (let ((middle-1 (aref data i))
                         (middle-2 (aref data (1+ i))))
                     (if (= middle-1 middle-2)
                         (rev  (+ start middle-1))
                         (swap (+ start middle-1)
                               (+ start middle-2)))))
                 vec)
              `(progn
                 ,@(loop for i below (length data) by 2
                         collect
                         (let ((middle-1 (aref data i))
                               (middle-2 (aref data (1+ i))))
                           (if (= middle-1 middle-2)
                               `(rev  (+ start ,middle-1))
                               `(swap (+ start ,middle-1)
                                      (+ start ,middle-2)))))))))))

(defvar *outer-width* 3)
(defvar *max-small-bit-reverse* 256)
(defvar *max-reversal-radix* 8)

(defun gen-bit-reversal (n el-type)
  el-type
  (assert (power-of-two-p n))
  (if (<= n *max-small-bit-reverse*)
      (emit-small-bit-reverse n)
      (let* ((width (lb n))
             (outer (min *outer-width*
                         (floor width 2)))
             (inner (- width (* 2 outer))))
        (generate-large-reversal outer inner))))

#+nil
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
